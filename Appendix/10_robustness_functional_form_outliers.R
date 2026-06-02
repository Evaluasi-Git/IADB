# ==============================================================================
# IADB - 09 Robustness: Functional Forms and Outlier Sensitivity -----------------
# Author: Cedric Antunes (Evaluasi)
# Revised by: ChatGPT
# Date: June 2026
# Version: v4 - robustly maps focal coefficient names and preserves diagnostics for linear CR2 robustness
#              models, sanitizes output columns, and preserves skipped-model diagnostics
#
# Purpose:
#   Implement PAP/SAP robustness checks for alternative functional forms and
#   outlier-sensitive cost/time outcomes, then export client-facing barplots.
#
# Interpretation:
#   These outputs are ROBUSTNESS / SENSITIVITY checks. They do not replace the
#   Script 08 confirmatory PAP/SAP results, Romano-Wolf correction for channels,
#   or Holm-Bonferroni correction for amount/delivery.
#
# Main robustness checks:
#   1. Success: LPM with CR2, logit AMEs, probit AMEs.
#   2. KYC: OLS on 0-3 score, binary any-KYC LPM, high-KYC LPM, and optional
#      ordered-logit AMEs on expected KYC score.
#   3. Cost: raw, log1p, asinh, 1/99 winsorized, 5/95 winsorized, IQR-trimmed.
#   4. Time: raw, log1p, asinh, 1/99 winsorized, 5/95 winsorized, IQR-trimmed.
#
# Inference and plotting:
#   - Linear models use estimatr::lm_robust(..., se_type = "CR2"), clustered at
#     the confederate level.
#   - Nonlinear AMEs use a cluster bootstrap at the confederate level.
#   - Plots use 95% CIs and color bars only if raw p < 0.05 AND the 95% CI
#     excludes zero. These are sensitivity plots, not confirmatory tests.
# ===============================================================================

# ------------------------------------------------------------------------------
# 0. Setup ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
  library(here)
  library(estimatr)
  library(broom)
  library(scales)
})

# Optional package for ordered-logit robustness. MASS ships with most R installs.
has_mass <- requireNamespace("MASS", quietly = TRUE)

# Bootstrap settings for nonlinear AMEs. Increase BOOT_B for final appendix runs.
RUN_GLM_AME_BOOTSTRAP <- TRUE
RUN_ORDERED_KYC <- TRUE
BOOT_B <- 499
BOOT_SEED <- 20260603

# ------------------------------------------------------------------------------
# 1. Paths ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_dir <- here("data", "clean", "sap_dataset_builder")
etable_dir <- file.path(sap_dir, "final_etables")
robust_dir <- file.path(sap_dir, "robustness")
figure_dir <- file.path(sap_dir, "final_figures", "robustness")

dir.create(robust_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

sap_main_path <- file.path(sap_dir, "IADB_sap_observed_first_pass.rds")
ct_main_path <- file.path(sap_dir, "IADB_sap_observed_first_pass_cost_time.rds")

required_files <- c(sap_main_path, ct_main_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required input file(s):\n", paste(missing_files, collapse = "\n"))
}

# ------------------------------------------------------------------------------
# 2. Visual settings -----------------------------------------------------------
# ------------------------------------------------------------------------------
COLOR_NULL <- "#dfe2d2"
COLOR_POSITIVE_SIG <- "#6cbf84"
COLOR_NEGATIVE_SIG <- "#f26968"
COLOR_CI <- "#323339"
COLOR_ZERO <- "#323339"
BASE_SIZE <- 12

term_labels <- c(
  MTO = "MTOs\nvs Banks",
  Fintech = "Fintech\nvs Banks",
  Crypto = "Crypto\nvs Banks",
  Amount250 = "USD 250\nvs USD 100",
  Online = "Online\nvs in-person"
)

channel_terms <- c("MTO", "Fintech", "Crypto")
transaction_terms <- c("Amount250", "Online")
all_focal_terms <- c(channel_terms, transaction_terms)

# ------------------------------------------------------------------------------
# 3. General helpers -----------------------------------------------------------
# ------------------------------------------------------------------------------
to_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

as_logical_safe <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  x_clean <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  case_when(
    x_clean %in% c("true", "t", "1", "yes", "sim", "si") ~ TRUE,
    x_clean %in% c("false", "f", "0", "no", "na", "", "missing") ~ FALSE,
    TRUE ~ FALSE
  )
}

add_missing_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) df[[cc]] <- NA
  }
  df
}

filter_required_flag <- function(df, flag_name) {
  if (!flag_name %in% names(df)) {
    stop("Required sample flag is missing: ", flag_name)
  }
  df |> filter(as_logical_safe(.data[[flag_name]]))
}

standardize_model_vars <- function(df) {
  df |>
    clean_names() |>
    add_missing_cols(c(
      "success",
      "kyc_score",
      "kyc_score_composite_0_5",
      "total_cost_without_time_usd",
      "reported_time_hours",
      "interaction_time_hours",
      "transaction_duration_hours",
      "assigned_channel",
      "assigned_amount",
      "assigned_delivery",
      "confederate_match_key",
      "country",
      "sample_cost_usd_success_only",
      "sample_transaction_duration"
    )) |>
    mutate(
      success = to_num(success),
      kyc_score_0_3 = to_num(kyc_score),
      kyc_score_0_5 = to_num(kyc_score_composite_0_5),
      assigned_channel = factor(
        assigned_channel,
        levels = c("Banks", "MTOs", "Fintech", "Crypto")
      ),
      assigned_delivery = factor(
        assigned_delivery,
        levels = c("In-person", "Online")
      ),
      assigned_amount = to_num(assigned_amount),
      MTO = as.numeric(assigned_channel == "MTOs"),
      Fintech = as.numeric(assigned_channel == "Fintech"),
      Crypto = as.numeric(assigned_channel == "Crypto"),
      Amount250 = as.numeric(assigned_amount == 250),
      Online = as.numeric(assigned_delivery == "Online"),
      country = as.factor(country),
      confederate_match_key = as.factor(confederate_match_key),
      any_kyc = as.numeric(!is.na(kyc_score_0_3) & kyc_score_0_3 > 0),
      high_kyc = as.numeric(!is.na(kyc_score_0_3) & kyc_score_0_3 >= 2)
    )
}

prep_model_data <- function(df, outcome) {
  df |>
    filter(
      !is.na(.data[[outcome]]),
      !is.na(confederate_match_key),
      !is.na(MTO),
      !is.na(Fintech),
      !is.na(Crypto),
      !is.na(Amount250),
      !is.na(Online)
    )
}

safe_lm_cr2 <- function(
    formula,
    data,
    cluster = "confederate_match_key"
) {
  outcome_name <- all.vars(formula)[1]

  if (nrow(data) == 0) return(list(model = NULL, reason = "zero-row estimation sample"))
  if (!outcome_name %in% names(data)) return(list(model = NULL, reason = paste0("outcome not found: ", outcome_name)))

  y <- data[[outcome_name]]
  if (length(unique(na.omit(y))) <= 1) return(list(model = NULL, reason = "outcome is constant or all missing"))
  if (!cluster %in% names(data)) return(list(model = NULL, reason = paste0("cluster variable not found: ", cluster)))

  out <- tryCatch(
    estimatr::lm_robust(
      formula = formula,
      data = data,
      clusters = data[[cluster]],
      se_type = "CR2"
    ),
    error = function(e) e
  )

  if (inherits(out, "error")) return(list(model = NULL, reason = out$message))
  list(model = out, reason = NA_character_)
}

# Keep only scalar, CSV-safe columns from model output.
# This prevents readr::write_csv() from failing when a model object carries
# auxiliary list/matrix columns through broom or bind_rows().
sanitize_model_output <- function(df) {
  out_cols <- c(
    "model_class", "outcome_family", "outcome_label", "spec_id", "spec_label",
    "effect_unit", "sample_label", "term", "estimate", "std.error", "statistic",
    "p.value", "conf.low", "conf.high", "nobs", "n_clusters", "skipped",
    "skip_reason"
  )

  df |>
    select(any_of(out_cols)) |>
    mutate(
      across(
        any_of(c("model_class", "outcome_family", "outcome_label", "spec_id",
                 "spec_label", "effect_unit", "sample_label", "term",
                 "skip_reason")),
        as.character
      ),
      across(
        any_of(c("estimate", "std.error", "statistic", "p.value",
                 "conf.low", "conf.high", "nobs", "n_clusters")),
        as.numeric
      ),
      skipped = as.logical(skipped)
    )
}

winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  pmin(pmax(x, qs[1]), qs[2])
}

get_outlier_thresholds <- function(x) {
  q1 <- stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE, type = 7)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE, type = 7)
  iqr <- q3 - q1
  tibble(q1 = q1, q3 = q3, iqr = iqr, lower = q1 - 1.5 * iqr, upper = q3 + 1.5 * iqr)
}

classify_bar_status <- function(df, p_col = "p.value", alpha = 0.05, require_ci_excludes_zero = TRUE) {
  if (!p_col %in% names(df)) stop("Requested p-value column not found: ", p_col)

  out <- df |>
    mutate(
      p_for_coloring = .data[[p_col]],
      alpha_for_coloring = alpha,
      ci_excludes_zero = !is.na(conf.low) & !is.na(conf.high) &
        ((conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0)),
      significant_for_coloring = !is.na(p_for_coloring) & p_for_coloring < alpha
    )

  if (isTRUE(require_ci_excludes_zero)) {
    out <- out |> mutate(significant_for_coloring = significant_for_coloring & ci_excludes_zero)
  }

  out |>
    mutate(
      bar_status = case_when(
        significant_for_coloring & estimate > 0 ~ "significant_positive",
        significant_for_coloring & estimate < 0 ~ "significant_negative",
        TRUE ~ "null_or_not_significant"
      ),
      bar_status = factor(
        bar_status,
        levels = c("null_or_not_significant", "significant_positive", "significant_negative")
      )
    )
}

# ------------------------------------------------------------------------------
# 4. Load data and primary analysis samples ------------------------------------
# ------------------------------------------------------------------------------
sap_main <- readRDS(sap_main_path) |> standardize_model_vars()
ct_main <- readRDS(ct_main_path) |> standardize_model_vars()

success_kyc_sample <- sap_main
cost_success_sample <- filter_required_flag(ct_main, "sample_cost_usd_success_only")
duration_sample <- filter_required_flag(ct_main, "sample_transaction_duration")

# ------------------------------------------------------------------------------
# 5. Linear CR2 robustness models ----------------------------------------------
# ------------------------------------------------------------------------------
run_linear_cr2_spec <- function(
    df,
    outcome,
    outcome_family,
    outcome_label,
    spec_id,
    spec_label,
    effect_unit,
    sample_label = "main_strict_slot_level"
) {
  df_model <- prep_model_data(df, outcome) |>
    mutate(confederate_match_key = factor(confederate_match_key))

  # Use explicit confederate fixed effects here. This matches Script 08's
  # preferred confederate-FE specification.
  fml <- as.formula(paste0(
    outcome,
    " ~ MTO + Fintech + Crypto + Amount250 + Online + factor(confederate_match_key)"
  ))

  fit <- safe_lm_cr2(fml, df_model)

  if (is.null(fit$model)) {
    return(tibble(
      model_class = "linear_cr2",
      outcome_family = outcome_family,
      outcome_label = outcome_label,
      spec_id = spec_id,
      spec_label = spec_label,
      effect_unit = effect_unit,
      sample_label = sample_label,
      term = NA_character_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_,
      p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
      nobs = nrow(df_model), n_clusters = n_distinct(df_model$confederate_match_key),
      skipped = TRUE, skip_reason = fit$reason
    ) |> sanitize_model_output())
  }

  # Some model/tidy combinations can rename binary indicators or carry additional
  # attributes. Map term names back to the five focal indicators robustly before
  # filtering. This prevents linear robustness models from disappearing silently.
  tidy_raw <- broom::tidy(fit$model, conf.int = TRUE) |>
    mutate(
      raw_term = as.character(term),
      term = case_when(
        raw_term == "MTO" | stringr::str_detect(raw_term, "^MTO") ~ "MTO",
        raw_term == "Fintech" | stringr::str_detect(raw_term, "^Fintech") ~ "Fintech",
        raw_term == "Crypto" | stringr::str_detect(raw_term, "^Crypto") ~ "Crypto",
        raw_term == "Amount250" | stringr::str_detect(raw_term, "^Amount250") ~ "Amount250",
        raw_term == "Online" | stringr::str_detect(raw_term, "^Online") ~ "Online",
        TRUE ~ raw_term
      )
    )

  focal_tidy <- tidy_raw |>
    filter(term %in% all_focal_terms)

  if (nrow(focal_tidy) == 0) {
    return(tibble(
      model_class = "linear_cr2",
      outcome_family = outcome_family,
      outcome_label = outcome_label,
      spec_id = spec_id,
      spec_label = spec_label,
      effect_unit = effect_unit,
      sample_label = sample_label,
      term = NA_character_, estimate = NA_real_, std.error = NA_real_, statistic = NA_real_,
      p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_,
      nobs = as.numeric(stats::nobs(fit$model)),
      n_clusters = as.numeric(n_distinct(df_model$confederate_match_key)),
      skipped = TRUE,
      skip_reason = paste0(
        "model estimated but no focal coefficients were returned by broom::tidy(); available terms: ",
        paste(utils::head(unique(tidy_raw$raw_term), 25), collapse = ", ")
      )
    ) |> sanitize_model_output())
  }

  focal_tidy |>
    transmute(
      model_class = "linear_cr2",
      outcome_family = outcome_family,
      outcome_label = outcome_label,
      spec_id = spec_id,
      spec_label = spec_label,
      effect_unit = effect_unit,
      sample_label = sample_label,
      term = as.character(term),
      estimate = as.numeric(estimate),
      std.error = as.numeric(std.error),
      statistic = as.numeric(statistic),
      p.value = as.numeric(p.value),
      conf.low = as.numeric(conf.low),
      conf.high = as.numeric(conf.high),
      nobs = as.numeric(stats::nobs(fit$model)),
      n_clusters = as.numeric(n_distinct(df_model$confederate_match_key)),
      skipped = FALSE,
      skip_reason = NA_character_
    ) |>
    sanitize_model_output()
}

make_cost_time_specs <- function(df, base_var, outcome_family) {
  base_x <- df[[base_var]]
  thr <- get_outlier_thresholds(base_x)

  specs <- list(
    raw = list(
      label = if_else(outcome_family == "cost", "Raw cost", "Raw duration"),
      unit = if_else(outcome_family == "cost", "USD", "hours"),
      data = df |> mutate(y_robust = .data[[base_var]])
    ),
    log1p = list(
      label = if_else(outcome_family == "cost", "log(1 + cost)", "log(1 + duration)"),
      unit = "log points",
      data = df |> mutate(y_robust = log1p(.data[[base_var]]))
    ),
    asinh = list(
      label = if_else(outcome_family == "cost", "asinh(cost)", "asinh(duration)"),
      unit = "asinh units",
      data = df |> mutate(y_robust = asinh(.data[[base_var]]))
    ),
    winsor_1_99 = list(
      label = "Winsorized 1/99",
      unit = if_else(outcome_family == "cost", "USD", "hours"),
      data = df |> mutate(y_robust = winsorize_vec(.data[[base_var]], c(0.01, 0.99)))
    ),
    winsor_5_95 = list(
      label = "Winsorized 5/95",
      unit = if_else(outcome_family == "cost", "USD", "hours"),
      data = df |> mutate(y_robust = winsorize_vec(.data[[base_var]], c(0.05, 0.95)))
    ),
    iqr_trimmed = list(
      label = "IQR-trimmed",
      unit = if_else(outcome_family == "cost", "USD", "hours"),
      data = df |>
        filter(.data[[base_var]] >= thr$lower, .data[[base_var]] <= thr$upper) |>
        mutate(y_robust = .data[[base_var]])
    )
  )

  list(specs = specs, thresholds = thr)
}

# Linear models: Success and KYC alternatives ----------------------------------
linear_results <- list(
  success_lpm = run_linear_cr2_spec(
    success_kyc_sample, "success", "success", "Transaction success", "success_lpm",
    "LPM", "probability"
  ),
  kyc_ols = run_linear_cr2_spec(
    success_kyc_sample, "kyc_score_0_3", "kyc", "KYC score", "kyc_ols_0_3",
    "OLS on 0-3 score", "score points"
  ),
  kyc_any = run_linear_cr2_spec(
    success_kyc_sample, "any_kyc", "kyc", "Any KYC", "kyc_any_lpm",
    "Any KYC LPM", "probability"
  ),
  kyc_high = run_linear_cr2_spec(
    success_kyc_sample, "high_kyc", "kyc", "High KYC", "kyc_high_lpm",
    "High KYC (>=2) LPM", "probability"
  )
)

# Cost/time transformations and outlier sensitivity ----------------------------
cost_specs_obj <- make_cost_time_specs(cost_success_sample, "total_cost_without_time_usd", "cost")
time_specs_obj <- make_cost_time_specs(duration_sample, "transaction_duration_hours", "time")

cost_results <- purrr::imap(
  cost_specs_obj$specs,
  ~ run_linear_cr2_spec(
    .x$data, "y_robust", "cost", "Transaction cost", paste0("cost_", .y), .x$label, .x$unit
  )
)

time_results <- purrr::imap(
  time_specs_obj$specs,
  ~ run_linear_cr2_spec(
    .x$data, "y_robust", "time", "Transaction duration", paste0("time_", .y), .x$label, .x$unit
  )
)

# IMPORTANT: bind the lists of tibble outputs after concatenating the lists.
# Using bind_rows(linear_results, cost_results, time_results) can treat the
# named lists themselves as nested/list columns in some dplyr versions, causing
# the linear/cost/time robustness outputs to disappear after sanitization.
linear_results_df <- c(linear_results, cost_results, time_results) |>
  purrr::compact() |>
  dplyr::bind_rows() |>
  sanitize_model_output()

cat("\nLinear/cost/time robustness specifications generated:\n")
print(
  linear_results_df |>
    dplyr::distinct(outcome_family, spec_id, spec_label, skipped, skip_reason) |>
    dplyr::arrange(outcome_family, spec_id)
)

outlier_thresholds <- bind_rows(
  cost_specs_obj$thresholds |> mutate(outcome_family = "cost", base_var = "total_cost_without_time_usd"),
  time_specs_obj$thresholds |> mutate(outcome_family = "time", base_var = "transaction_duration_hours")
) |>
  select(outcome_family, base_var, everything())

# ------------------------------------------------------------------------------
# 6. Nonlinear AME robustness: logit/probit success ----------------------------
# ------------------------------------------------------------------------------
compute_glm_ames <- function(fit, df, terms) {
  purrr::map_dfr(terms, function(tt) {
    df1 <- df
    df0 <- df
    df1[[tt]] <- 1
    df0[[tt]] <- 0

    pred1 <- tryCatch(stats::predict(fit, newdata = df1, type = "response"), error = function(e) rep(NA_real_, nrow(df)))
    pred0 <- tryCatch(stats::predict(fit, newdata = df0, type = "response"), error = function(e) rep(NA_real_, nrow(df)))

    tibble(term = tt, estimate = mean(pred1 - pred0, na.rm = TRUE))
  })
}

bootstrap_cluster_sample <- function(df, cluster_var = "confederate_match_key") {
  clusters <- unique(df[[cluster_var]])
  sampled_clusters <- sample(clusters, size = length(clusters), replace = TRUE)

  bind_rows(lapply(seq_along(sampled_clusters), function(ii) {
    df[df[[cluster_var]] == sampled_clusters[[ii]], , drop = FALSE] |>
      mutate(.boot_cluster_draw = ii)
  }))
}

run_glm_ame_boot <- function(
    df,
    link,
    spec_id,
    spec_label,
    terms = all_focal_terms,
    B = BOOT_B,
    seed = BOOT_SEED
) {
  df_model <- prep_model_data(df, "success") |>
    mutate(confederate_match_key = factor(confederate_match_key))

  fml <- success ~ MTO + Fintech + Crypto + Amount250 + Online + factor(confederate_match_key)

  fit <- tryCatch(
    stats::glm(fml, data = df_model, family = stats::binomial(link = link), control = list(maxit = 50)),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(tibble(
      model_class = "glm_ame_cluster_boot",
      outcome_family = "success",
      outcome_label = "Transaction success",
      spec_id = spec_id,
      spec_label = spec_label,
      effect_unit = "probability",
      sample_label = "main_strict_slot_level",
      term = terms,
      estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_, nobs = nrow(df_model),
      n_clusters = n_distinct(df_model$confederate_match_key), skipped = TRUE,
      skip_reason = fit$message
    ))
  }

  point <- compute_glm_ames(fit, df_model, terms)

  set.seed(seed)
  boot_mat <- matrix(NA_real_, nrow = B, ncol = length(terms), dimnames = list(NULL, terms))

  if (isTRUE(RUN_GLM_AME_BOOTSTRAP) && B > 0) {
    for (bb in seq_len(B)) {
      boot_df <- bootstrap_cluster_sample(df_model)
      boot_fit <- tryCatch(
        stats::glm(fml, data = boot_df, family = stats::binomial(link = link), control = list(maxit = 50)),
        error = function(e) NULL,
        warning = function(w) suppressWarnings(stats::glm(fml, data = boot_df, family = stats::binomial(link = link), control = list(maxit = 50)))
      )

      if (!is.null(boot_fit)) {
        boot_ames <- tryCatch(compute_glm_ames(boot_fit, boot_df, terms), error = function(e) NULL)
        if (!is.null(boot_ames)) {
          boot_mat[bb, boot_ames$term] <- boot_ames$estimate
        }
      }
    }
  }

  point |>
    mutate(
      std.error = purrr::map_dbl(term, ~ stats::sd(boot_mat[, .x], na.rm = TRUE)),
      statistic = estimate / std.error,
      p.value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      conf.low = purrr::map_dbl(term, ~ stats::quantile(boot_mat[, .x], 0.025, na.rm = TRUE, names = FALSE)),
      conf.high = purrr::map_dbl(term, ~ stats::quantile(boot_mat[, .x], 0.975, na.rm = TRUE, names = FALSE)),
      model_class = "glm_ame_cluster_boot",
      outcome_family = "success",
      outcome_label = "Transaction success",
      spec_id = spec_id,
      spec_label = spec_label,
      effect_unit = "probability",
      sample_label = "main_strict_slot_level",
      nobs = nrow(df_model),
      n_clusters = n_distinct(df_model$confederate_match_key),
      skipped = FALSE,
      skip_reason = NA_character_,
      .before = 1
    )
}

glm_results_df <- bind_rows(
  run_glm_ame_boot(success_kyc_sample, "logit", "success_logit_ame", "Logit AME"),
  run_glm_ame_boot(success_kyc_sample, "probit", "success_probit_ame", "Probit AME")
)

# ------------------------------------------------------------------------------
# 7. Optional ordered-logit AMEs for KYC expected score -------------------------
# ------------------------------------------------------------------------------
compute_polr_ames <- function(fit, df, terms) {
  score_levels <- as.numeric(levels(df$kyc_ordered))

  purrr::map_dfr(terms, function(tt) {
    df1 <- df
    df0 <- df
    df1[[tt]] <- 1
    df0[[tt]] <- 0

    probs1 <- tryCatch(stats::predict(fit, newdata = df1, type = "probs"), error = function(e) NULL)
    probs0 <- tryCatch(stats::predict(fit, newdata = df0, type = "probs"), error = function(e) NULL)

    if (is.null(probs1) || is.null(probs0)) {
      return(tibble(term = tt, estimate = NA_real_))
    }

    probs1 <- as.matrix(probs1)
    probs0 <- as.matrix(probs0)
    exp1 <- as.numeric(probs1 %*% score_levels)
    exp0 <- as.numeric(probs0 %*% score_levels)

    tibble(term = tt, estimate = mean(exp1 - exp0, na.rm = TRUE))
  })
}

run_polr_ame_boot <- function(df, terms = all_focal_terms, B = BOOT_B, seed = BOOT_SEED) {
  if (!has_mass || !isTRUE(RUN_ORDERED_KYC)) {
    return(tibble(
      model_class = "ordered_logit_ame_cluster_boot",
      outcome_family = "kyc",
      outcome_label = "KYC expected score",
      spec_id = "kyc_ordered_logit_ame",
      spec_label = "Ordered logit AME",
      effect_unit = "score points",
      sample_label = "main_strict_slot_level",
      term = terms,
      estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_, nobs = NA_integer_, n_clusters = NA_integer_,
      skipped = TRUE,
      skip_reason = "MASS unavailable or ordered KYC robustness disabled"
    ))
  }

  df_model <- prep_model_data(df, "kyc_score_0_3") |>
    mutate(
      confederate_match_key = factor(confederate_match_key),
      kyc_ordered = ordered(kyc_score_0_3, levels = c(0, 1, 2, 3))
    )

  fml <- kyc_ordered ~ MTO + Fintech + Crypto + Amount250 + Online + factor(confederate_match_key)

  fit <- tryCatch(
    MASS::polr(fml, data = df_model, Hess = TRUE, method = "logistic"),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(tibble(
      model_class = "ordered_logit_ame_cluster_boot",
      outcome_family = "kyc",
      outcome_label = "KYC expected score",
      spec_id = "kyc_ordered_logit_ame",
      spec_label = "Ordered logit AME",
      effect_unit = "score points",
      sample_label = "main_strict_slot_level",
      term = terms,
      estimate = NA_real_, std.error = NA_real_, statistic = NA_real_, p.value = NA_real_,
      conf.low = NA_real_, conf.high = NA_real_, nobs = nrow(df_model),
      n_clusters = n_distinct(df_model$confederate_match_key), skipped = TRUE,
      skip_reason = fit$message
    ))
  }

  point <- compute_polr_ames(fit, df_model, terms)

  set.seed(seed + 1)
  boot_mat <- matrix(NA_real_, nrow = B, ncol = length(terms), dimnames = list(NULL, terms))

  if (B > 0) {
    for (bb in seq_len(B)) {
      boot_df <- bootstrap_cluster_sample(df_model)
      boot_fit <- tryCatch(
        MASS::polr(fml, data = boot_df, Hess = FALSE, method = "logistic"),
        error = function(e) NULL,
        warning = function(w) suppressWarnings(MASS::polr(fml, data = boot_df, Hess = FALSE, method = "logistic"))
      )

      if (!is.null(boot_fit)) {
        boot_ames <- tryCatch(compute_polr_ames(boot_fit, boot_df, terms), error = function(e) NULL)
        if (!is.null(boot_ames)) {
          boot_mat[bb, boot_ames$term] <- boot_ames$estimate
        }
      }
    }
  }

  point |>
    mutate(
      std.error = purrr::map_dbl(term, ~ stats::sd(boot_mat[, .x], na.rm = TRUE)),
      statistic = estimate / std.error,
      p.value = 2 * stats::pnorm(abs(statistic), lower.tail = FALSE),
      conf.low = purrr::map_dbl(term, ~ stats::quantile(boot_mat[, .x], 0.025, na.rm = TRUE, names = FALSE)),
      conf.high = purrr::map_dbl(term, ~ stats::quantile(boot_mat[, .x], 0.975, na.rm = TRUE, names = FALSE)),
      model_class = "ordered_logit_ame_cluster_boot",
      outcome_family = "kyc",
      outcome_label = "KYC expected score",
      spec_id = "kyc_ordered_logit_ame",
      spec_label = "Ordered logit AME",
      effect_unit = "score points",
      sample_label = "main_strict_slot_level",
      nobs = nrow(df_model),
      n_clusters = n_distinct(df_model$confederate_match_key),
      skipped = FALSE,
      skip_reason = NA_character_,
      .before = 1
    )
}

polr_results_df <- run_polr_ame_boot(success_kyc_sample)

# ------------------------------------------------------------------------------
# 8. Combine and export robustness results -------------------------------------
# ------------------------------------------------------------------------------
robustness_results <- bind_rows(
  linear_results_df,
  glm_results_df,
  polr_results_df
) |>
  sanitize_model_output() |>
  relocate(
    model_class, outcome_family, outcome_label, spec_id, spec_label, effect_unit,
    sample_label, term, estimate, std.error, statistic, p.value, conf.low, conf.high,
    nobs, n_clusters, skipped, skip_reason
  )

robustness_results_clean <- robustness_results |>
  filter(!is.na(term)) |>
  sanitize_model_output()

cat("\nAll robustness coefficient rows by outcome family/specification:\n")
print(
  robustness_results_clean |>
    dplyr::count(outcome_family, spec_id, spec_label, name = "n_terms") |>
    dplyr::arrange(outcome_family, spec_id)
)

# Keep skipped models in the diagnostics. The coefficient-level output excludes
# rows without a focal term, but the skipped-model log preserves failed checks.
skipped_robustness_models <- robustness_results |>
  filter(skipped == TRUE) |>
  distinct(
    model_class, outcome_family, outcome_label, spec_id, spec_label, effect_unit,
    sample_label, nobs, n_clusters, skip_reason
  ) |>
  arrange(outcome_family, spec_id)

model_sample_summary <- robustness_results |>
  group_by(
    model_class, outcome_family, outcome_label, spec_id, spec_label,
    effect_unit, sample_label
  ) |>
  summarise(
    nobs = suppressWarnings(max(nobs, na.rm = TRUE)),
    n_clusters = suppressWarnings(max(n_clusters, na.rm = TRUE)),
    skipped = all(skipped),
    skip_reason = paste(unique(na.omit(skip_reason)), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    nobs = if_else(is.infinite(nobs), NA_real_, as.numeric(nobs)),
    n_clusters = if_else(is.infinite(n_clusters), NA_real_, as.numeric(n_clusters)),
    skip_reason = na_if(skip_reason, "")
  ) |>
  arrange(outcome_family, spec_id)

write_csv(robustness_results_clean, file.path(robust_dir, "IADB_09_robustness_all_results.csv"))
write_csv(model_sample_summary, file.path(robust_dir, "IADB_09_robustness_model_sample_summary.csv"))
write_csv(skipped_robustness_models, file.path(robust_dir, "IADB_09_skipped_robustness_models.csv"))
write_csv(outlier_thresholds, file.path(robust_dir, "IADB_09_outlier_thresholds.csv"))

# ------------------------------------------------------------------------------
# 9. Client-facing robustness barplots -----------------------------------------
# ------------------------------------------------------------------------------
plot_scale_results <- function(df) {
  df |>
    mutate(
      term_key = as.character(term),
      term_clean = recode(term_key, !!!term_labels, .default = term_key),
      # Probability outcomes shown in percentage points.
      plot_scale = if_else(effect_unit == "probability", 100, 1),
      estimate_plot = estimate * plot_scale,
      conf.low_plot = conf.low * plot_scale,
      conf.high_plot = conf.high * plot_scale,
      effect_unit_plot = if_else(effect_unit == "probability", "percentage points", effect_unit),
      spec_label_plot = paste0(spec_label, "\n(", effect_unit_plot, ")"),
      term_clean = factor(term_clean, levels = unname(term_labels[all_focal_terms]))
    )
}

make_robustness_plot <- function(
    df,
    outcome_family_filter,
    terms_to_plot,
    title,
    subtitle,
    caption,
    output_stem,
    width = 12,
    height = 8.5
) {
  plot_df <- df |>
    filter(
      skipped == FALSE,
      outcome_family == outcome_family_filter,
      term %in% terms_to_plot,
      !is.na(estimate), !is.na(conf.low), !is.na(conf.high)
    ) |>
    classify_bar_status(p_col = "p.value", alpha = 0.05, require_ci_excludes_zero = TRUE) |>
    plot_scale_results()

  if (nrow(plot_df) == 0) {
    warning("No rows available for plot: ", output_stem)
    return(invisible(NULL))
  }

  # Keep a stable and readable specification order.
  spec_order <- plot_df |>
    distinct(spec_id, spec_label_plot) |>
    arrange(spec_id) |>
    pull(spec_label_plot)

  plot_df <- plot_df |>
    mutate(spec_label_plot = factor(spec_label_plot, levels = spec_order))

  p <- ggplot(plot_df, aes(x = term_clean, y = estimate_plot, fill = bar_status)) +
    geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
    geom_col(width = 0.68, color = NA) +
    geom_errorbar(
      aes(ymin = conf.low_plot, ymax = conf.high_plot),
      width = 0.18,
      linewidth = 0.75,
      color = COLOR_CI
    ) +
    facet_wrap(~ spec_label_plot, scales = "free_y") +
    scale_fill_manual(
      values = c(
        null_or_not_significant = COLOR_NULL,
        significant_positive = COLOR_POSITIVE_SIG,
        significant_negative = COLOR_NEGATIVE_SIG
      ),
      drop = FALSE
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Estimated difference from reference category",
      caption = caption
    ) +
    theme_minimal(base_size = BASE_SIZE) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE + 2),
      plot.subtitle = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
      plot.caption = element_text(color = COLOR_CI, size = BASE_SIZE - 3, hjust = 0),
      axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 3),
      axis.text.y = element_text(color = COLOR_CI, size = BASE_SIZE - 2),
      axis.title.y = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
      strip.text = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE - 2),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  ggsave(file.path(figure_dir, paste0(output_stem, ".png")), p, width = width, height = height, dpi = 320)
  ggsave(file.path(figure_dir, paste0(output_stem, ".pdf")), p, width = width, height = height, device = cairo_pdf)

  invisible(p)
}

sensitivity_caption_channel <- paste(
  "Sensitivity figure only. Bars are colored green/red only when raw p < 0.05 and the 95% CI excludes zero.",
  "Confirmatory claims should use Script 08 Romano-Wolf adjusted channel results."
)

sensitivity_caption_transaction <- paste(
  "Sensitivity figure only. Bars are colored green/red only when raw p < 0.05 and the 95% CI excludes zero.",
  "Confirmatory claims should use Script 08 Holm-adjusted amount/delivery results."
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "success",
  terms_to_plot = channel_terms,
  title = "Success robustness: channel effects relative to Banks",
  subtitle = "Alternative functional forms: LPM, logit AME, and probit AME.",
  caption = sensitivity_caption_channel,
  output_stem = "IADB_09_plot_success_functional_forms_channel"
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "success",
  terms_to_plot = transaction_terms,
  title = "Success robustness: transaction-characteristic effects",
  subtitle = "Alternative functional forms: LPM, logit AME, and probit AME.",
  caption = sensitivity_caption_transaction,
  output_stem = "IADB_09_plot_success_functional_forms_transaction",
  width = 10
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "kyc",
  terms_to_plot = channel_terms,
  title = "KYC robustness: channel effects relative to Banks",
  subtitle = "Alternative outcome/function forms for KYC intensity.",
  caption = sensitivity_caption_channel,
  output_stem = "IADB_09_plot_kyc_functional_forms_channel"
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "kyc",
  terms_to_plot = transaction_terms,
  title = "KYC robustness: transaction-characteristic effects",
  subtitle = "Alternative outcome/function forms for KYC intensity.",
  caption = sensitivity_caption_transaction,
  output_stem = "IADB_09_plot_kyc_functional_forms_transaction",
  width = 10
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "cost",
  terms_to_plot = channel_terms,
  title = "Cost robustness: channel effects relative to Banks",
  subtitle = "Raw, transformed, winsorized, and IQR-trimmed cost outcomes.",
  caption = sensitivity_caption_channel,
  output_stem = "IADB_09_plot_cost_outlier_transforms_channel",
  height = 9.2
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "cost",
  terms_to_plot = transaction_terms,
  title = "Cost robustness: transaction-characteristic effects",
  subtitle = "Raw, transformed, winsorized, and IQR-trimmed cost outcomes.",
  caption = sensitivity_caption_transaction,
  output_stem = "IADB_09_plot_cost_outlier_transforms_transaction",
  width = 10,
  height = 9.2
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "time",
  terms_to_plot = channel_terms,
  title = "Duration robustness: channel effects relative to Banks",
  subtitle = "Raw, transformed, winsorized, and IQR-trimmed transaction-duration outcomes.",
  caption = sensitivity_caption_channel,
  output_stem = "IADB_09_plot_time_outlier_transforms_channel",
  height = 9.2
)

make_robustness_plot(
  robustness_results_clean,
  outcome_family_filter = "time",
  terms_to_plot = transaction_terms,
  title = "Duration robustness: transaction-characteristic effects",
  subtitle = "Raw, transformed, winsorized, and IQR-trimmed transaction-duration outcomes.",
  caption = sensitivity_caption_transaction,
  output_stem = "IADB_09_plot_time_outlier_transforms_transaction",
  width = 10,
  height = 9.2
)

# ------------------------------------------------------------------------------
# 10. Console summary ----------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n=== IADB 09 ROBUSTNESS SCRIPT COMPLETE ===\n")
cat("Robustness tables saved to:\n")
cat("  ", robust_dir, "\n", sep = "")
cat("Robustness figures saved to:\n")
cat("  ", figure_dir, "\n", sep = "")
cat("\nAvailable robustness specifications by outcome family:\n")
print(
  robustness_results_clean |>
    distinct(outcome_family, spec_id, spec_label) |>
    count(outcome_family, name = "n_specs")
)

cat("\nImportant interpretation note:\n")
cat("  These are sensitivity checks, not confirmatory PAP/SAP tests.\n")
cat("  Use Script 08 adjusted p-values for confirmatory claims.\n")
cat("  Robustness plot colors use raw p < 0.05 AND 95% CI excludes zero.\n")
