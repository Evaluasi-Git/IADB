# ==============================================================================
# IADB - 08 Generate Final PAP/SAP Results, Boss-Revision Version --------------
# Author: Cedric Antunes / Evaluasi --------------------------------------------
# Revision date: June 2026 -----------------------------------------------------
#
# Purpose:
#   Generate the revised production results requested in the IADB revision memo.
#   This script updates the previous Script 08 in six main ways:
#
#   1. Treats the realized first-pass observed sample as the primary sample.
#   2. Promotes binary any-KYC (kyc_score_0_3 >= 1) to the primary KYC outcome.
#      Enhanced KYC (kyc_score_0_3 == 3) and the 0-3 score are retained as
#      secondary/robustness outcomes.
#   3. Uses two design-matched confirmatory multiple-testing families:
#        Family A: channel contrasts, 3 channels x 4 outcomes = 12 tests,
#                  Romano-Wolf stepdown via wild cluster bootstrap.
#        Family B: amount and delivery contrasts, 2 terms x 4 outcomes = 8 tests,
#                  Westfall-Young free stepdown via within-confederate permutation.
#      BH-FDR is exported as a labeled complement only.
#   4. Adds selection diagnostics: coverage/adherence, deviation taxonomy, Lee
#      bounds for success/timing-conditioned cost and duration outcomes.
#   5. Adds channel/delivery collinearity diagnostics:
#        a) online-only channel comparisons;
#        b) within-Banks/MTO delivery comparisons.
#   6. Adds exploratory temporal/order permutation diagnostics.
#
# Important interpretation register:
#   - Channel contrasts are descriptive/associational comparisons between
#     pre-existing institution types. Channel order was randomized, not channel
#     type as a manipulable treatment.
#   - Amount and delivery labels were randomized to transaction slots, so the
#     transaction-characteristic family uses design-based within-confederate
#     permutation inference.
#   - CR2 confidence intervals are exported for display. Confirmatory claims
#     should use the FWER-adjusted p-values: RW for channel, WY for amount/delivery.
#
# Required inputs from earlier scripts:
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol.rds
#   data/clean/sap_dataset_builder/IADB_sap_reviewed_submissions.rds
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_reviewed_submissions_cost_time.rds
#
# Optional input for sharper coverage diagnostics, if available:
#   data/clean/sap_dataset_builder/IADB_canonical_schedule_slots.rds
#   The optional file should contain one row per canonical schedule slot and an
#   observed/realized flag or a slot identifier mergeable to observed data.
#
# Main outputs:
#   data/clean/sap_dataset_builder/final_etables_boss_revision/
#     IADB_08R_core_results_cr2.csv
#     IADB_08R_core_results_with_mht.csv
#     IADB_08R_family_A_channel_RW.csv
#     IADB_08R_family_B_amount_delivery_WY.csv
#     IADB_08R_family_B_amount_delivery_RW_crosscheck.csv
#     IADB_08R_coverage_adherence_summary.csv
#     IADB_08R_channel_realization_diagnostics.csv
#     IADB_08R_deviation_taxonomy.csv
#     IADB_08R_lee_bounds_cost_duration.csv
#     IADB_08R_collinearity_diagnostics.csv
#     IADB_08R_temporal_permutation.csv
#     IADB_08R_realized_mde_table.csv
#     IADB_08R_deviations_log.csv
# ==============================================================================

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
  library(here)
  library(estimatr)
  library(broom)
  library(fixest)
})

# ------------------------------------------------------------------------------
# User-facing switches ---------------------------------------------------------
# ------------------------------------------------------------------------------
# Set these lower while debugging, then restore to 9999 for final production.
B_CONFIRMATORY <- 9999L
B_EXPLORATORY  <- 9999L
SEED_MAIN      <- 20260608L

# Confirmatory engines. Keep TRUE for final production.
RUN_RW_CHANNEL                <- TRUE
RUN_WY_TRANSACTION            <- TRUE
RUN_RW_TRANSACTION_CROSSCHECK <- TRUE

# Exploratory / diagnostic modules. These are requested by the revision memo.
RUN_LEE_BOUNDS             <- TRUE
RUN_COLLINEARITY_DIAGNOSTICS <- TRUE
RUN_TEMPORAL_PERMUTATION   <- TRUE
RUN_REALIZED_MDE           <- TRUE

# If no canonical schedule-slot file is available, the script uses this denominator
# for schedule-realization diagnostics, matching the revision memo.
CANONICAL_SLOT_DENOMINATOR <- 980L

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_dir <- here("data", "clean", "sap_dataset_builder")
etable_dir <- file.path(sap_dir, "final_etables_boss_revision")
dir.create(etable_dir, showWarnings = FALSE, recursive = TRUE)

sap_main_path     <- file.path(sap_dir, "IADB_sap_observed_first_pass.rds")
sap_pp_path       <- file.path(sap_dir, "IADB_sap_per_protocol.rds")
sap_reviewed_path <- file.path(sap_dir, "IADB_sap_reviewed_submissions.rds")

ct_main_path      <- file.path(sap_dir, "IADB_sap_observed_first_pass_cost_time.rds")
ct_pp_path        <- file.path(sap_dir, "IADB_sap_per_protocol_cost_time.rds")
ct_reviewed_path  <- file.path(sap_dir, "IADB_sap_reviewed_submissions_cost_time.rds")

candidate_schedule_paths <- c(
  file.path(sap_dir, "IADB_canonical_schedule_slots.rds"),
  file.path(sap_dir, "IADB_canonical_schedule.rds"),
  file.path(sap_dir, "IADB_all_canonical_slots.rds")
)
canonical_schedule_path <- candidate_schedule_paths[file.exists(candidate_schedule_paths)][1]
if (length(canonical_schedule_path) == 0 || is.na(canonical_schedule_path)) {
  canonical_schedule_path <- NA_character_
}

required_files <- c(
  sap_main_path, sap_pp_path, sap_reviewed_path,
  ct_main_path, ct_pp_path, ct_reviewed_path
)
missing_required <- required_files[!file.exists(required_files)]
if (length(missing_required) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste0("  - ", missing_required, collapse = "\n"),
    "\nRun Scripts 01-07 first, or update `sap_dir`."
  )
}

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
to_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

as_logical_safe <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  x_clean <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    x_clean %in% c("true", "t", "1", "yes", "y", "sim", "si") ~ TRUE,
    x_clean %in% c("false", "f", "0", "no", "n", "na", "", "missing") ~ FALSE,
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

first_existing_name <- function(df, candidates) {
  hits <- intersect(candidates, names(df))
  if (length(hits) == 0) return(NA_character_)
  hits[[1]]
}

filter_required_flag <- function(df, flag_name) {
  if (!flag_name %in% names(df)) {
    stop("Required sample flag is missing: ", flag_name)
  }
  df |> dplyr::filter(as_logical_safe(.data[[flag_name]]))
}

safe_write_csv <- function(x, path) {
  readr::write_csv(x, path, na = "")
  invisible(path)
}

# Small utility used in Lee bounds and robustness transformations.
winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  pmin(pmax(x, qs[1]), qs[2])
}

asinh_safe <- function(x) {
  asinh(x)
}

log1p_safe <- function(x) {
  ifelse(x < -1, NA_real_, log1p(x))
}

# ------------------------------------------------------------------------------
# Standardize production variables --------------------------------------------
# ------------------------------------------------------------------------------
standardize_model_vars <- function(df) {
  df <- df |>
    janitor::clean_names() |>
    add_missing_cols(c(
      # outcome variables
      "success",
      "kyc_score",
      "kyc_score_composite_0_5",
      "total_cost_without_time_usd",
      "reported_time_hours",
      "interaction_time_hours",
      "transaction_duration_hours",
      # assigned design variables
      "assigned_channel",
      "assigned_amount",
      "assigned_delivery",
      # cluster/context variables
      "confederate_match_key",
      "country",
      # likely order variables
      "transaction_number",
      "transaction_order",
      "assigned_order",
      "schedule_order",
      "approximate_date",
      # quality/deviation variables
      "needs_manual_review_for_final",
      "sample_cost_usd_any_attempt",
      "sample_cost_usd_success_only",
      "sample_reported_time",
      "sample_interaction_time",
      "sample_transaction_duration"
    ))

  # Build a sequence/order variable. Prefer an explicit schedule/order variable.
  order_col <- first_existing_name(
    df,
    c("schedule_order", "assigned_order", "transaction_number", "transaction_order")
  )

  df <- df |>
    mutate(
      success = to_num(success),

      # Registered 0-3 score retained, but no longer primary in this revision.
      kyc_score_0_3 = to_num(kyc_score),
      kyc_score_0_5 = to_num(kyc_score_composite_0_5),

      # Boss-revision KYC outcomes.
      kyc_any = case_when(
        is.na(kyc_score_0_3) ~ NA_real_,
        kyc_score_0_3 >= 1 ~ 1,
        kyc_score_0_3 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      kyc_enhanced = case_when(
        is.na(kyc_score_0_3) ~ NA_real_,
        kyc_score_0_3 == 3 ~ 1,
        kyc_score_0_3 < 3 ~ 0,
        TRUE ~ NA_real_
      ),

      assigned_channel = stringr::str_squish(as.character(assigned_channel)),
      assigned_channel = dplyr::case_when(
        assigned_channel %in% c("Bank", "Banks", "Traditional banks", "Traditional Banks") ~ "Banks",
        assigned_channel %in% c("MTO", "MTOs", "Money transfer", "Money Transfer Operator", "Money Transfer Operators") ~ "MTOs",
        assigned_channel %in% c("Fintech", "FinTech") ~ "Fintech",
        assigned_channel %in% c("Crypto", "Cryptocurrency", "Cryptocurrency exchanges") ~ "Crypto",
        TRUE ~ assigned_channel
      ),
      assigned_channel = factor(assigned_channel, levels = c("Banks", "MTOs", "Fintech", "Crypto")),

      assigned_delivery = stringr::str_squish(as.character(assigned_delivery)),
      assigned_delivery = dplyr::case_when(
        stringr::str_to_lower(assigned_delivery) %in% c("in-person", "in person", "in_person", "person") ~ "In-person",
        stringr::str_to_lower(assigned_delivery) %in% c("online", "digital", "app") ~ "Online",
        TRUE ~ assigned_delivery
      ),
      assigned_delivery = factor(assigned_delivery, levels = c("In-person", "Online")),
      assigned_amount = to_num(assigned_amount),

      MTO = as.numeric(assigned_channel == "MTOs"),
      Fintech = as.numeric(assigned_channel == "Fintech"),
      Crypto = as.numeric(assigned_channel == "Crypto"),
      Amount250 = as.numeric(assigned_amount == 250),
      Online = as.numeric(assigned_delivery == "Online"),

      total_cost_without_time_usd = to_num(total_cost_without_time_usd),
      reported_time_hours = to_num(reported_time_hours),
      interaction_time_hours = to_num(interaction_time_hours),
      transaction_duration_hours = to_num(transaction_duration_hours),

      country = as.factor(country),
      confederate_match_key = as.factor(confederate_match_key),
      needs_manual_review_for_final = as_logical_safe(needs_manual_review_for_final)
    )

  if (!is.na(order_col)) {
    df <- df |> mutate(seqpos_raw = to_num(.data[[order_col]]))
  } else {
    df <- df |> mutate(seqpos_raw = NA_real_)
  }

  df |>
    group_by(confederate_match_key) |>
    mutate(
      seqpos = ifelse(is.na(seqpos_raw), row_number(), seqpos_raw),
      seqpos = as.numeric(seqpos),
      seqpos_centered = seqpos - mean(seqpos, na.rm = TRUE),
      late = as.numeric(seqpos > stats::median(seqpos, na.rm = TRUE))
    ) |>
    ungroup()
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

# ------------------------------------------------------------------------------
# Safe model estimators --------------------------------------------------------
# ------------------------------------------------------------------------------
safe_lm_cr2 <- function(formula, data, cluster = "confederate_match_key") {
  outcome_name <- all.vars(formula)[1]

  if (nrow(data) == 0) {
    return(list(model = NULL, reason = "zero-row estimation sample"))
  }
  if (!outcome_name %in% names(data)) {
    return(list(model = NULL, reason = paste0("outcome not found: ", outcome_name)))
  }
  if (length(unique(na.omit(data[[outcome_name]]))) <= 1) {
    return(list(model = NULL, reason = "outcome is constant or all missing"))
  }
  if (!cluster %in% names(data)) {
    return(list(model = NULL, reason = paste0("cluster variable not found: ", cluster)))
  }
  if (dplyr::n_distinct(stats::na.omit(data[[cluster]])) < 2) {
    return(list(model = NULL, reason = "fewer than two clusters"))
  }

  out <- tryCatch(
    estimatr::lm_robust(
      formula = formula,
      data = data,
      clusters = data[[cluster]],
      se_type = "CR2"
    ),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    return(list(model = NULL, reason = out$message))
  }

  list(model = out, reason = NA_character_)
}

estimate_model_family <- function(df, outcome, outcome_label, sample_label) {
  df_model <- prep_model_data(df, outcome)
  has_country <- "country" %in% names(df_model) &&
    dplyr::n_distinct(stats::na.omit(df_model$country)) > 1

  formulas <- list(
    M1_channel_only = as.formula(
      paste0(outcome, " ~ MTO + Fintech + Crypto")
    ),
    M2_adjusted_confederate_fe_PRIMARY = as.formula(
      paste0(
        outcome,
        " ~ MTO + Fintech + Crypto + Amount250 + Online + factor(confederate_match_key)"
      )
    )
  )

  if (has_country) {
    formulas$M3_country_fe_exploratory <- as.formula(
      paste0(outcome, " ~ MTO + Fintech + Crypto + Amount250 + Online + factor(country)")
    )
  }

  estimated <- purrr::imap(formulas, function(fml, model_label) {
    fit <- safe_lm_cr2(fml, data = df_model)
    list(
      model = fit$model,
      reason = fit$reason,
      formula = paste(deparse(fml), collapse = " "),
      model_label = model_label,
      sample_label = sample_label,
      outcome = outcome,
      outcome_label = outcome_label,
      n_estimation_rows = nrow(df_model),
      n_clusters = dplyr::n_distinct(df_model$confederate_match_key)
    )
  })

  names(estimated) <- paste(outcome_label, sample_label, names(formulas), sep = "__")
  estimated
}

tidy_model_list <- function(model_list) {
  purrr::imap_dfr(model_list, function(x, model_id) {
    if (is.null(x$model)) {
      return(tibble(
        model_id = model_id,
        outcome = x$outcome,
        outcome_label = x$outcome_label,
        sample_label = x$sample_label,
        model_label = x$model_label,
        formula = x$formula,
        term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        nobs = x$n_estimation_rows,
        n_clusters = x$n_clusters,
        skipped = TRUE,
        skip_reason = x$reason
      ))
    }

    broom::tidy(x$model, conf.int = TRUE) |>
      mutate(
        model_id = model_id,
        outcome = x$outcome,
        outcome_label = x$outcome_label,
        sample_label = x$sample_label,
        model_label = x$model_label,
        formula = x$formula,
        nobs = stats::nobs(x$model),
        n_clusters = x$n_clusters,
        skipped = FALSE,
        skip_reason = NA_character_,
        .before = 1
      )
  })
}

# ------------------------------------------------------------------------------
# Load production datasets -----------------------------------------------------
# ------------------------------------------------------------------------------
sap_main     <- readRDS(sap_main_path)     |> standardize_model_vars()
sap_pp       <- readRDS(sap_pp_path)       |> standardize_model_vars()
sap_reviewed <- readRDS(sap_reviewed_path) |> standardize_model_vars()

ct_main      <- readRDS(ct_main_path)      |> standardize_model_vars()
ct_pp        <- readRDS(ct_pp_path)        |> standardize_model_vars()
ct_reviewed  <- readRDS(ct_reviewed_path)  |> standardize_model_vars()

sap_samples <- list(
  first_pass_observed_primary = sap_main,
  per_protocol_robustness = sap_pp,
  reviewed_submissions_robustness = sap_reviewed
)

ct_samples <- list(
  first_pass_observed_primary = ct_main,
  per_protocol_robustness = ct_pp,
  reviewed_submissions_robustness = ct_reviewed
)

cost_any_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_cost_usd_any_attempt"))
cost_success_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_cost_usd_success_only"))
reported_time_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_reported_time"))
interaction_time_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_interaction_time"))
duration_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_transaction_duration"))

# ------------------------------------------------------------------------------
# Outcome and deviation logs ---------------------------------------------------
# ------------------------------------------------------------------------------
outcome_definition_log <- tribble(
  ~outcome_label, ~outcome_variable, ~dataset_family, ~analysis_sample, ~boss_revision_status, ~notes,
  "success", "success", "success_kyc", "first-pass observed sample", "Co-primary", "Binary LPM; all valid attempts.",
  "kyc_any", "kyc_any", "success_kyc", "first-pass observed sample", "Co-primary; revised from PAP", "1 if KYC score >= 1; 0 if KYC score = 0. This is the revised primary KYC contrast.",
  "cost_success", "total_cost_without_time_usd", "cost_time", "successful transactions only", "Co-primary", "Complete-case conditional estimate; Lee bounds assess selection.",
  "time_duration", "transaction_duration_hours", "cost_time", "successful transactions with observed duration", "Co-primary", "Complete-case conditional estimate; Lee bounds absorb success selection and timing missingness.",
  "kyc_enhanced", "kyc_enhanced", "success_kyc", "first-pass observed sample", "Secondary", "1 if KYC score = 3.",
  "kyc_0_3", "kyc_score_0_3", "success_kyc", "first-pass observed sample", "Robustness", "Original PAP primary KYC score retained as robustness.",
  "kyc_0_5", "kyc_score_0_5", "success_kyc", "first-pass observed sample", "Sensitivity", "Composite KYC index; not substituted for the 0-3 score.",
  "cost_any", "total_cost_without_time_usd", "cost_time", "any attempt with observed monetary cost", "Sensitivity", "Descriptive/sensitivity, not the primary conditional cost estimand.",
  "reported_time", "reported_time_hours", "cost_time", "rows passing sample_reported_time", "Sensitivity", "Alternative time measure.",
  "interaction_time", "interaction_time_hours", "cost_time", "rows passing sample_interaction_time", "Sensitivity", "Alternative time measure."
)

safe_write_csv(outcome_definition_log, file.path(etable_dir, "IADB_08R_outcome_definition_log.csv"))

deviations_log <- tribble(
  ~item, ~classification, ~decision, ~rationale,
  "Multiple testing", "Confirmatory inference", "Use FWER control via RW-WCB for 12 channel tests and WY permutation for 8 amount/delivery tests; report BH as complement.", "Harmonizes conflicting PAP/SAP/deck specifications and matches the resampling method to what was randomized.",
  "Mechanism family", "Exploratory", "Do not include mechanism/mediation in confirmatory Family B.", "Avoids collision between old Family 2 mechanism language and revised amount/delivery family.",
  "KYC primary contrast", "Outcome construction", "Promote any-KYC binary to revised primary KYC outcome; retain enhanced-KYC and 0-3 score as secondary/robustness.", "Observed KYC distribution is zero-inflated/bimodal; binary contrasts are clearer and more defensible.",
  "Primary analysis sample", "Sample definition", "Use first-pass observed sample as primary; per-protocol and reviewed-submissions as robustness.", "Treats schedule realization/adherence as selection diagnostics rather than silent attrition.",
  "Inference register", "Language", "Use associational language for channel; randomized-factor language only for amount and delivery.", "Channel type is a pre-existing institution type; only order/amount/delivery slots were randomized.",
  "Order randomization", "Design diagnostic", "Use order randomization for temporal/sequence permutation tests and channel-time de-confounding claim, not channel permutation.", "Permuting channel labels would not match the design."
)

safe_write_csv(deviations_log, file.path(etable_dir, "IADB_08R_deviations_log.csv"))

# ------------------------------------------------------------------------------
# Primary and robustness model estimation --------------------------------------
# ------------------------------------------------------------------------------
primary_model_lists <- list(
  success = estimate_model_family(
    sap_samples$first_pass_observed_primary,
    outcome = "success",
    outcome_label = "success",
    sample_label = "first_pass_observed_primary"
  ),
  kyc_any = estimate_model_family(
    sap_samples$first_pass_observed_primary,
    outcome = "kyc_any",
    outcome_label = "kyc_any",
    sample_label = "first_pass_observed_primary"
  ),
  cost_success = estimate_model_family(
    cost_success_samples$first_pass_observed_primary,
    outcome = "total_cost_without_time_usd",
    outcome_label = "cost_success",
    sample_label = "first_pass_observed_primary"
  ),
  time_duration = estimate_model_family(
    duration_samples$first_pass_observed_primary,
    outcome = "transaction_duration_hours",
    outcome_label = "time_duration",
    sample_label = "first_pass_observed_primary"
  )
) |> purrr::flatten()

primary_results <- tidy_model_list(primary_model_lists)

all_model_lists <- list()
for (sample_name in names(sap_samples)) {
  all_model_lists <- c(
    all_model_lists,
    estimate_model_family(sap_samples[[sample_name]], "success", "success", sample_name),
    estimate_model_family(sap_samples[[sample_name]], "kyc_any", "kyc_any", sample_name),
    estimate_model_family(sap_samples[[sample_name]], "kyc_enhanced", "kyc_enhanced_secondary", sample_name),
    estimate_model_family(sap_samples[[sample_name]], "kyc_score_0_3", "kyc_0_3_robustness", sample_name),
    estimate_model_family(sap_samples[[sample_name]], "kyc_score_0_5", "kyc_0_5_sensitivity", sample_name)
  )
}

for (sample_name in names(ct_samples)) {
  all_model_lists <- c(
    all_model_lists,
    estimate_model_family(cost_success_samples[[sample_name]], "total_cost_without_time_usd", "cost_success", sample_name),
    estimate_model_family(cost_any_samples[[sample_name]], "total_cost_without_time_usd", "cost_any_sensitivity", sample_name),
    estimate_model_family(duration_samples[[sample_name]], "transaction_duration_hours", "time_duration", sample_name),
    estimate_model_family(reported_time_samples[[sample_name]], "reported_time_hours", "reported_time_sensitivity", sample_name),
    estimate_model_family(interaction_time_samples[[sample_name]], "interaction_time_hours", "interaction_time_sensitivity", sample_name)
  )
}

all_results <- tidy_model_list(all_model_lists)

safe_write_csv(primary_results, file.path(etable_dir, "IADB_08R_core_results_cr2.csv"))
safe_write_csv(all_results, file.path(etable_dir, "IADB_08R_all_results_cr2.csv"))
saveRDS(primary_model_lists, file.path(etable_dir, "IADB_08R_core_models_cr2.rds"))
saveRDS(all_model_lists, file.path(etable_dir, "IADB_08R_all_models_cr2.rds"))

preferred_model_label <- "M2_adjusted_confederate_fe_PRIMARY"
primary_outcomes <- c("success", "kyc_any", "cost_success", "time_duration")

multiplicity_base <- primary_results |>
  filter(
    skipped == FALSE,
    sample_label == "first_pass_observed_primary",
    model_label == preferred_model_label,
    outcome_label %in% primary_outcomes
  )

# ------------------------------------------------------------------------------
# Common free-stepdown maxT engine ---------------------------------------------
# ------------------------------------------------------------------------------
free_stepdown_maxT <- function(t_obs, t_null) {
  stopifnot(is.numeric(t_obs))
  if (is.null(dim(t_null))) stop("t_null must be a B x K matrix")

  keep <- is.finite(t_obs) & colSums(is.finite(t_null)) == nrow(t_null)
  out <- rep(NA_real_, length(t_obs))
  names(out) <- names(t_obs)
  if (!any(keep)) return(out)

  t_obs_k <- abs(t_obs[keep])
  t_null_k <- abs(t_null[, keep, drop = FALSE])
  K <- length(t_obs_k)
  ord <- order(t_obs_k, decreasing = TRUE)
  to <- t_obs_k[ord]
  tn <- t_null_k[, ord, drop = FALSE]
  p_raw <- numeric(K)

  for (r in seq_len(K)) {
    maxset <- if (r == K) tn[, K] else apply(tn[, r:K, drop = FALSE], 1, max)
    p_raw[r] <- mean(maxset >= to[r])
  }

  p_adj_ordered <- cummax(p_raw)
  p_adj <- numeric(K)
  p_adj[ord] <- p_adj_ordered
  out[keep] <- pmin(1, p_adj)
  out
}

# ------------------------------------------------------------------------------
# Family A: channel contrasts, Romano-Wolf wild cluster bootstrap --------------
# ------------------------------------------------------------------------------
run_rw_family <- function(
    terms,
    family_name,
    B = B_CONFIRMATORY,
    seed = SEED_MAIN,
    boot_type = "rademacher",
    bootstrap_type = "fnw11",
    engine = "R"
) {
  required_rw_packages <- c("fwildclusterboot")
  missing_rw_packages <- required_rw_packages[
    !vapply(required_rw_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]
  if (length(missing_rw_packages) > 0) {
    stop(
      family_name, " requires package(s): ", paste(missing_rw_packages, collapse = ", "),
      "\nInstall with:\n",
      "install.packages(c('dqrng', 'fwildclusterboot'), repos = c('https://s3alfisc.r-universe.dev', 'https://cloud.r-project.org'))"
    )
  }

  rw_specs <- list(
    success = list(
      outcome_label = "success",
      outcome = "success",
      data = sap_samples$first_pass_observed_primary
    ),
    kyc_any = list(
      outcome_label = "kyc_any",
      outcome = "kyc_any",
      data = sap_samples$first_pass_observed_primary
    ),
    cost_success = list(
      outcome_label = "cost_success",
      outcome = "total_cost_without_time_usd",
      data = cost_success_samples$first_pass_observed_primary
    ),
    time_duration = list(
      outcome_label = "time_duration",
      outcome = "transaction_duration_hours",
      data = duration_samples$first_pass_observed_primary
    )
  )

  estimate_lm_model <- function(spec) {
    df_model <- prep_model_data(spec$data, spec$outcome) |>
      mutate(confederate_match_key = factor(confederate_match_key))

    fml <- as.formula(paste0(
      spec$outcome,
      " ~ MTO + Fintech + Crypto + Amount250 + Online + confederate_match_key"
    ))
    stats::lm(fml, data = df_model)
  }

  lm_models <- purrr::map(rw_specs, estimate_lm_model)

  test_grid <- tidyr::crossing(
    outcome_label = names(rw_specs),
    term = terms
  ) |>
    mutate(test_id = paste(outcome_label, term, sep = "__"))

  run_one_boot_test <- function(outcome_label, term) {
    set.seed(seed)
    if (requireNamespace("dqrng", quietly = TRUE)) {
      dqrng::dqset.seed(seed)
    }

    fwildclusterboot::boottest(
      object = lm_models[[outcome_label]],
      param = term,
      clustid = "confederate_match_key",
      B = B,
      type = boot_type,
      bootstrap_type = bootstrap_type,
      engine = engine,
      p_val_type = "two-tailed",
      impose_null = TRUE,
      conf_int = FALSE,
      nthreads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
    )
  }

  boot_tests <- purrr::map2(test_grid$outcome_label, test_grid$term, run_one_boot_test)

  boot_t_stats <- boot_tests |>
    purrr::map(~ .x[["t_boot"]]) |>
    Reduce(f = cbind)

  observed_t_stats <- boot_tests |>
    purrr::map(fwildclusterboot::teststat) |>
    unlist(use.names = FALSE)

  names(observed_t_stats) <- test_grid$test_id
  colnames(boot_t_stats) <- test_grid$test_id

  p_rw <- free_stepdown_maxT(observed_t_stats, boot_t_stats)

  test_grid |>
    mutate(
      family = family_name,
      p_romano_wolf = as.numeric(p_rw[test_id]),
      significant_romano_wolf_05 = p_romano_wolf < 0.05,
      rw_bootstrap_B = B,
      rw_seed = seed,
      rw_boot_type = boot_type,
      rw_bootstrap_type = bootstrap_type,
      rw_engine = engine,
      rw_bootstrap_model = "lm_explicit_confederate_fe"
    )
}

if (RUN_RW_CHANNEL) {
  rw_channel <- run_rw_family(
    terms = c("MTO", "Fintech", "Crypto"),
    family_name = "Family A: channel comparisons",
    B = B_CONFIRMATORY,
    seed = SEED_MAIN
  )
} else {
  rw_channel <- tibble(
    outcome_label = character(), term = character(), family = character(),
    p_romano_wolf = numeric()
  )
}

family_A_channel <- multiplicity_base |>
  filter(term %in% c("MTO", "Fintech", "Crypto")) |>
  mutate(
    family = "Family A: channel comparisons",
    p_bh_complement = p.adjust(p.value, method = "BH"),
    significant_raw_05 = p.value < 0.05,
    significant_bh_complement_05 = p_bh_complement < 0.05
  ) |>
  left_join(
    rw_channel |>
      select(outcome_label, term, p_romano_wolf, significant_romano_wolf_05,
             rw_bootstrap_B, rw_seed, rw_boot_type, rw_bootstrap_type,
             rw_engine, rw_bootstrap_model),
    by = c("outcome_label", "term")
  ) |>
  arrange(p_romano_wolf, p.value)

safe_write_csv(family_A_channel, file.path(etable_dir, "IADB_08R_family_A_channel_RW.csv"))

# ------------------------------------------------------------------------------
# Family B: amount/delivery, Westfall-Young within-confederate permutation -----
# ------------------------------------------------------------------------------
get_fixest_abs_t <- function(df, outcome, terms = c("Amount250", "Online")) {
  df_model <- prep_model_data(df, outcome)
  out <- rep(NA_real_, length(terms))
  names(out) <- terms

  if (nrow(df_model) == 0 || length(unique(na.omit(df_model[[outcome]]))) <= 1) {
    return(out)
  }

  fml <- as.formula(paste0(
    outcome,
    " ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key"
  ))

  mod <- tryCatch(
    fixest::feols(fml, data = df_model, cluster = ~confederate_match_key, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(mod)) return(out)

  ct <- tryCatch(as.data.frame(fixest::coeftable(mod)), error = function(e) NULL)
  if (is.null(ct)) return(out)
  ct$term <- rownames(ct)

  for (tt in terms) {
    if (tt %in% ct$term && "t value" %in% names(ct)) {
      out[[tt]] <- abs(ct[ct$term == tt, "t value"][[1]])
    }
  }
  out
}

fit_family_B_stats <- function(data_list) {
  specs <- list(
    success = list(data = data_list$sap, outcome = "success"),
    kyc_any = list(data = data_list$sap, outcome = "kyc_any"),
    cost_success = list(data = data_list$cost_success, outcome = "total_cost_without_time_usd"),
    time_duration = list(data = data_list$duration, outcome = "transaction_duration_hours")
  )

  purrr::imap(specs, function(spec, outcome_label) {
    stats <- get_fixest_abs_t(spec$data, spec$outcome, terms = c("Amount250", "Online"))
    names(stats) <- paste(outcome_label, names(stats), sep = "__")
    stats
  }) |>
    unlist(use.names = TRUE)
}

permute_amount_delivery_within_confederate <- function(df) {
  df |>
    group_by(confederate_match_key) |>
    mutate(
      Amount250 = sample(Amount250, size = dplyr::n(), replace = FALSE),
      Online = {
        o <- Online
        idx <- assigned_channel %in% c("Banks", "MTOs") & !is.na(Online)
        if (sum(idx, na.rm = TRUE) > 1) {
          o[idx] <- sample(o[idx], size = sum(idx, na.rm = TRUE), replace = FALSE)
        }
        o
      }
    ) |>
    ungroup()
}

run_westfall_young_family_B <- function(B = B_CONFIRMATORY, seed = SEED_MAIN) {
  set.seed(seed)

  observed_data_list <- list(
    sap = sap_samples$first_pass_observed_primary,
    cost_success = cost_success_samples$first_pass_observed_primary,
    duration = duration_samples$first_pass_observed_primary
  )

  t_obs <- fit_family_B_stats(observed_data_list)

  t_null <- replicate(B, {
    permuted_data_list <- list(
      sap = permute_amount_delivery_within_confederate(observed_data_list$sap),
      cost_success = permute_amount_delivery_within_confederate(observed_data_list$cost_success),
      duration = permute_amount_delivery_within_confederate(observed_data_list$duration)
    )
    fit_family_B_stats(permuted_data_list)
  })
  t_null <- t(t_null)
  colnames(t_null) <- names(t_obs)

  p_wy <- free_stepdown_maxT(t_obs, t_null)

  tibble(
    test_id = names(t_obs),
    outcome_label = stringr::str_replace(test_id, "__(Amount250|Online)$", ""),
    term = stringr::str_extract(test_id, "Amount250|Online"),
    observed_abs_t = as.numeric(t_obs),
    p_westfall_young = as.numeric(p_wy),
    significant_westfall_young_05 = p_westfall_young < 0.05,
    wy_B = B,
    wy_seed = seed,
    wy_permutation = "Amount250 permuted within confederate; Online permuted within confederate among Banks/MTOs only"
  )
}

if (RUN_WY_TRANSACTION) {
  wy_transaction <- run_westfall_young_family_B(B = B_CONFIRMATORY, seed = SEED_MAIN)
} else {
  wy_transaction <- tibble(
    outcome_label = character(), term = character(), p_westfall_young = numeric()
  )
}

family_B_transaction <- multiplicity_base |>
  filter(term %in% c("Amount250", "Online")) |>
  mutate(
    family = "Family B: randomized transaction characteristics",
    p_bh_complement = p.adjust(p.value, method = "BH"),
    significant_raw_05 = p.value < 0.05,
    significant_bh_complement_05 = p_bh_complement < 0.05
  ) |>
  left_join(
    wy_transaction |>
      select(outcome_label, term, p_westfall_young, significant_westfall_young_05,
             wy_B, wy_seed, wy_permutation),
    by = c("outcome_label", "term")
  ) |>
  arrange(p_westfall_young, p.value)

safe_write_csv(family_B_transaction, file.path(etable_dir, "IADB_08R_family_B_amount_delivery_WY.csv"))

# Family B RW-WCB cross-check --------------------------------------------------
if (RUN_RW_TRANSACTION_CROSSCHECK) {
  rw_transaction <- run_rw_family(
    terms = c("Amount250", "Online"),
    family_name = "Family B cross-check: randomized transaction characteristics, RW-WCB",
    B = B_CONFIRMATORY,
    seed = SEED_MAIN + 1L
  )

  family_B_rw_crosscheck <- multiplicity_base |>
    filter(term %in% c("Amount250", "Online")) |>
    left_join(
      rw_transaction |>
        select(outcome_label, term, p_romano_wolf, significant_romano_wolf_05),
      by = c("outcome_label", "term")
    ) |>
    left_join(
      family_B_transaction |>
        select(outcome_label, term, p_westfall_young, significant_westfall_young_05),
      by = c("outcome_label", "term")
    ) |>
    mutate(
      agreement_raw = case_when(
        is.na(p_romano_wolf) | is.na(p_westfall_young) ~ NA_character_,
        (p_romano_wolf < 0.05) == (p_westfall_young < 0.05) ~ "same_05_decision",
        TRUE ~ "different_05_decision"
      ),
      abs_p_difference = abs(p_romano_wolf - p_westfall_young)
    )
} else {
  family_B_rw_crosscheck <- tibble()
}

safe_write_csv(
  family_B_rw_crosscheck,
  file.path(etable_dir, "IADB_08R_family_B_amount_delivery_RW_crosscheck.csv")
)

# Core result table with MHT columns -------------------------------------------
core_results_with_mht <- multiplicity_base |>
  mutate(
    contrast_family = case_when(
      term %in% c("MTO", "Fintech", "Crypto") ~ "Family A: channel",
      term %in% c("Amount250", "Online") ~ "Family B: amount/delivery",
      TRUE ~ "not_confirmatory_family"
    )
  ) |>
  left_join(
    family_A_channel |>
      select(outcome_label, term, p_romano_wolf, significant_romano_wolf_05, p_bh_complement),
    by = c("outcome_label", "term")
  ) |>
  left_join(
    family_B_transaction |>
      select(outcome_label, term, p_westfall_young, significant_westfall_young_05, p_bh_complement),
    by = c("outcome_label", "term"),
    suffix = c("_channel", "_transaction")
  ) |>
  mutate(
    p_fwer_primary = case_when(
      contrast_family == "Family A: channel" ~ p_romano_wolf,
      contrast_family == "Family B: amount/delivery" ~ p_westfall_young,
      TRUE ~ NA_real_
    ),
    significant_fwer_primary_05 = p_fwer_primary < 0.05,
    p_bh_complement = case_when(
      contrast_family == "Family A: channel" ~ p_bh_complement_channel,
      contrast_family == "Family B: amount/delivery" ~ p_bh_complement_transaction,
      TRUE ~ NA_real_
    )
  )

safe_write_csv(core_results_with_mht, file.path(etable_dir, "IADB_08R_core_results_with_mht.csv"))

# ------------------------------------------------------------------------------
# Coverage/adherence diagnostics -----------------------------------------------
# ------------------------------------------------------------------------------
coverage_summary <- tibble(
  sample = c("first_pass_observed_primary", "per_protocol_robustness", "reviewed_submissions_robustness"),
  n_rows_success_kyc = c(nrow(sap_main), nrow(sap_pp), nrow(sap_reviewed)),
  n_clusters_success_kyc = c(
    n_distinct(sap_main$confederate_match_key),
    n_distinct(sap_pp$confederate_match_key),
    n_distinct(sap_reviewed$confederate_match_key)
  ),
  canonical_slot_denominator = CANONICAL_SLOT_DENOMINATOR,
  realization_rate_vs_canonical = n_rows_success_kyc / canonical_slot_denominator,
  note = c(
    "Primary first-pass observed sample",
    "Per-protocol robustness sample",
    "Reviewed-submissions robustness sample"
  )
)

safe_write_csv(coverage_summary, file.path(etable_dir, "IADB_08R_coverage_adherence_summary.csv"))

if (!is.na(canonical_schedule_path)) {
  canonical_schedule <- readRDS(canonical_schedule_path) |>
    standardize_model_vars()

  slot_id_col <- first_existing_name(canonical_schedule, c("transaction_uid", "slot_uid", "schedule_slot_id", "canonical_slot_id"))
  obs_id_col <- first_existing_name(sap_main, c("transaction_uid", "slot_uid", "schedule_slot_id", "canonical_slot_id"))

  if (!is.na(slot_id_col) && !is.na(obs_id_col)) {
    observed_ids <- unique(as.character(sap_main[[obs_id_col]]))
    channel_realization <- canonical_schedule |>
      mutate(realized = as.character(.data[[slot_id_col]]) %in% observed_ids) |>
      group_by(assigned_channel) |>
      summarise(
        canonical_slots = n(),
        realized_slots = sum(realized, na.rm = TRUE),
        realization_rate = realized_slots / canonical_slots,
        .groups = "drop"
      )
  } else {
    channel_realization <- sap_main |>
      count(assigned_channel, name = "realized_slots") |>
      mutate(
        canonical_slots = CANONICAL_SLOT_DENOMINATOR / 4,
        realization_rate = realized_slots / canonical_slots,
        diagnostic_basis = "No mergeable canonical slot id found; assumes equal channel slots."
      )
  }
} else {
  channel_realization <- sap_main |>
    count(assigned_channel, name = "realized_slots") |>
    mutate(
      canonical_slots = CANONICAL_SLOT_DENOMINATOR / 4,
      realization_rate = realized_slots / canonical_slots,
      diagnostic_basis = "No canonical schedule file found; assumes equal channel slots."
    )
}

safe_write_csv(channel_realization, file.path(etable_dir, "IADB_08R_channel_realization_diagnostics.csv"))

possible_deviation_cols <- intersect(
  names(sap_reviewed),
  c(
    "deviation_type", "deviation_category", "protocol_deviation", "protocol_deviation_type",
    "per_protocol_failure_reason", "schedule_match_status", "match_status",
    "needs_manual_review_for_final", "manual_review_reason", "recovery_status"
  )
)

if (length(possible_deviation_cols) > 0) {
  deviation_taxonomy <- purrr::map_dfr(possible_deviation_cols, function(cc) {
    sap_reviewed |>
      mutate(value = as.character(.data[[cc]])) |>
      count(variable = cc, value, name = "n") |>
      mutate(percent = n / sum(n))
  })
} else {
  deviation_taxonomy <- tibble(
    variable = "none_found",
    value = "No deviation/taxonomy columns found in reviewed-submissions file.",
    n = NA_integer_,
    percent = NA_real_
  )
}

safe_write_csv(deviation_taxonomy, file.path(etable_dir, "IADB_08R_deviation_taxonomy.csv"))

# ------------------------------------------------------------------------------
# Lee bounds for cost and duration ---------------------------------------------
# ------------------------------------------------------------------------------
trim_mean_fraction <- function(x, trim_prop, side = c("lower", "upper")) {
  side <- match.arg(side)
  x <- sort(x[is.finite(x)])
  n <- length(x)
  if (n == 0) return(NA_real_)
  if (trim_prop <= 0) return(mean(x))
  if (trim_prop >= 1) return(NA_real_)

  k <- floor(trim_prop * n)
  if (side == "lower") {
    # Lower mean for a high-selection group: trim largest values.
    keep <- seq_len(max(1, n - k))
  } else {
    # Upper mean for a high-selection group: trim smallest values.
    keep <- seq.int(min(n, k + 1), n)
  }
  mean(x[keep])
}

lee_bounds_one_contrast <- function(df, y, channel_value, ref_value = "Banks") {
  d <- df |>
    filter(assigned_channel %in% c(ref_value, channel_value)) |>
    mutate(
      group = as.character(assigned_channel),
      observed_y = !is.na(.data[[y]])
    )

  if (nrow(d) == 0) {
    return(tibble())
  }

  x_ref <- d |> filter(group == ref_value) |> pull(.data[[y]])
  x_ch  <- d |> filter(group == channel_value) |> pull(.data[[y]])

  s_ref <- mean(!is.na(x_ref))
  s_ch  <- mean(!is.na(x_ch))
  m_ref <- mean(x_ref, na.rm = TRUE)
  m_ch  <- mean(x_ch, na.rm = TRUE)

  if (!is.finite(s_ref) || !is.finite(s_ch) || s_ref == 0 || s_ch == 0) {
    return(tibble(
      outcome = y, channel = channel_value, reference = ref_value,
      selection_rate_channel = s_ch, selection_rate_reference = s_ref,
      trim_group = NA_character_, trim_proportion = NA_real_,
      lee_lower = NA_real_, lee_upper = NA_real_
    ))
  }

  if (s_ch > s_ref) {
    trim_prop <- (s_ch - s_ref) / s_ch
    ch_lower <- trim_mean_fraction(x_ch, trim_prop, side = "lower")
    ch_upper <- trim_mean_fraction(x_ch, trim_prop, side = "upper")
    lower <- ch_lower - m_ref
    upper <- ch_upper - m_ref
    trim_group <- channel_value
  } else if (s_ref > s_ch) {
    trim_prop <- (s_ref - s_ch) / s_ref
    ref_lower <- trim_mean_fraction(x_ref, trim_prop, side = "lower")
    ref_upper <- trim_mean_fraction(x_ref, trim_prop, side = "upper")
    lower <- m_ch - ref_upper
    upper <- m_ch - ref_lower
    trim_group <- ref_value
  } else {
    trim_prop <- 0
    lower <- m_ch - m_ref
    upper <- m_ch - m_ref
    trim_group <- "none_equal_selection"
  }

  tibble(
    outcome = y,
    channel = channel_value,
    reference = ref_value,
    n_channel_all = sum(d$group == channel_value),
    n_reference_all = sum(d$group == ref_value),
    n_channel_observed = sum(d$group == channel_value & d$observed_y),
    n_reference_observed = sum(d$group == ref_value & d$observed_y),
    selection_rate_channel = s_ch,
    selection_rate_reference = s_ref,
    complete_case_difference = m_ch - m_ref,
    trim_group = trim_group,
    trim_proportion = trim_prop,
    lee_lower = lower,
    lee_upper = upper
  )
}

if (RUN_LEE_BOUNDS) {
  lee_bounds <- bind_rows(
    purrr::map_dfr(c("MTOs", "Fintech", "Crypto"), ~ lee_bounds_one_contrast(
      ct_samples$first_pass_observed_primary,
      y = "total_cost_without_time_usd",
      channel_value = .x
    )),
    purrr::map_dfr(c("MTOs", "Fintech", "Crypto"), ~ lee_bounds_one_contrast(
      ct_samples$first_pass_observed_primary,
      y = "transaction_duration_hours",
      channel_value = .x
    ))
  ) |>
    mutate(
      note = "Lee-style trimming bounds on observed outcome, using first-pass cost/time file as the all-attempt denominator. Interpret as sensitivity, not as randomized-treatment bounds for channel."
    )
} else {
  lee_bounds <- tibble()
}

safe_write_csv(lee_bounds, file.path(etable_dir, "IADB_08R_lee_bounds_cost_duration.csv"))

# ------------------------------------------------------------------------------
# Collinearity diagnostics: online-only and Banks/MTO delivery -----------------
# ------------------------------------------------------------------------------
estimate_single_model <- function(df, outcome, fml_rhs, outcome_label, diagnostic) {
  df_model <- df |> filter(!is.na(.data[[outcome]]), !is.na(confederate_match_key))
  fml <- as.formula(paste0(outcome, " ~ ", fml_rhs))
  fit <- safe_lm_cr2(fml, df_model)
  tidy_model_list(list(
    result = list(
      model = fit$model,
      reason = fit$reason,
      formula = paste(deparse(fml), collapse = " "),
      model_label = diagnostic,
      sample_label = diagnostic,
      outcome = outcome,
      outcome_label = outcome_label,
      n_estimation_rows = nrow(df_model),
      n_clusters = n_distinct(df_model$confederate_match_key)
    )
  ))
}

run_collinearity_diagnostics <- function() {
  specs <- list(
    success = list(data = sap_samples$first_pass_observed_primary, outcome = "success"),
    kyc_any = list(data = sap_samples$first_pass_observed_primary, outcome = "kyc_any"),
    cost_success = list(data = cost_success_samples$first_pass_observed_primary, outcome = "total_cost_without_time_usd"),
    time_duration = list(data = duration_samples$first_pass_observed_primary, outcome = "transaction_duration_hours")
  )

  purrr::imap_dfr(specs, function(spec, outcome_label) {
    online_only <- spec$data |>
      filter(assigned_delivery == "Online")

    banks_mto_only <- spec$data |>
      filter(assigned_channel %in% c("Banks", "MTOs"))

    bind_rows(
      estimate_single_model(
        online_only,
        outcome = spec$outcome,
        fml_rhs = "MTO + Fintech + Crypto + Amount250 + factor(confederate_match_key)",
        outcome_label = outcome_label,
        diagnostic = "online_only_channel_comparison"
      ),
      estimate_single_model(
        banks_mto_only,
        outcome = spec$outcome,
        fml_rhs = "MTO + Amount250 + Online + factor(confederate_match_key)",
        outcome_label = outcome_label,
        diagnostic = "banks_mto_only_delivery_comparison"
      )
    )
  }) |>
    mutate(
      diagnostic_note = case_when(
        model_label == "online_only_channel_comparison" ~ "Drops in-person rows; compares channels among online transactions only.",
        model_label == "banks_mto_only_delivery_comparison" ~ "Restricts to Banks and MTOs; this is the cleanest delivery contrast because Fintech/Crypto are online-only.",
        TRUE ~ NA_character_
      )
    )
}

if (RUN_COLLINEARITY_DIAGNOSTICS) {
  collinearity_diagnostics <- run_collinearity_diagnostics()
} else {
  collinearity_diagnostics <- tibble()
}

safe_write_csv(collinearity_diagnostics, file.path(etable_dir, "IADB_08R_collinearity_diagnostics.csv"))

# ------------------------------------------------------------------------------
# Functional-form sensitivity --------------------------------------------------
# ------------------------------------------------------------------------------
make_continuous_sensitivity_data <- function(df, y) {
  df |>
    mutate(
      y_raw = .data[[y]],
      y_log1p = log1p_safe(.data[[y]]),
      y_asinh = asinh_safe(.data[[y]]),
      y_winsor_01_99 = winsorize_vec(.data[[y]], probs = c(0.01, 0.99)),
      y_winsor_05_95 = winsorize_vec(.data[[y]], probs = c(0.05, 0.95))
    )
}

run_functional_form_sensitivity <- function() {
  cost_df <- make_continuous_sensitivity_data(
    cost_success_samples$first_pass_observed_primary,
    "total_cost_without_time_usd"
  )
  dur_df <- make_continuous_sensitivity_data(
    duration_samples$first_pass_observed_primary,
    "transaction_duration_hours"
  )

  sens <- list()
  for (yy in c("y_raw", "y_log1p", "y_asinh", "y_winsor_01_99", "y_winsor_05_95")) {
    sens <- c(
      sens,
      estimate_model_family(cost_df, yy, paste0("cost_success__", yy), "functional_form"),
      estimate_model_family(dur_df, yy, paste0("time_duration__", yy), "functional_form")
    )
  }

  # IQR-trimmed versions for cost and duration.
  iqr_trim <- function(df, y) {
    q1 <- stats::quantile(df[[y]], 0.25, na.rm = TRUE)
    q3 <- stats::quantile(df[[y]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    df |> filter(.data[[y]] >= q1 - 1.5 * iqr, .data[[y]] <= q3 + 1.5 * iqr)
  }

  sens <- c(
    sens,
    estimate_model_family(
      iqr_trim(cost_success_samples$first_pass_observed_primary, "total_cost_without_time_usd"),
      "total_cost_without_time_usd", "cost_success__iqr_trim", "functional_form"
    ),
    estimate_model_family(
      iqr_trim(duration_samples$first_pass_observed_primary, "transaction_duration_hours"),
      "transaction_duration_hours", "time_duration__iqr_trim", "functional_form"
    )
  )

  tidy_model_list(sens)
}

functional_form_sensitivity <- run_functional_form_sensitivity()
safe_write_csv(functional_form_sensitivity, file.path(etable_dir, "IADB_08R_functional_form_sensitivity.csv"))

# ------------------------------------------------------------------------------
# Temporal / sequence permutation diagnostics ----------------------------------
# ------------------------------------------------------------------------------
get_temporal_abs_t <- function(df, outcome) {
  df_model <- prep_model_data(df, outcome) |>
    filter(!is.na(seqpos_centered), !is.na(late))

  terms <- c("seqpos_centered", "MTO:late", "Fintech:late", "Crypto:late")
  out <- rep(NA_real_, length(terms))
  names(out) <- terms

  if (nrow(df_model) == 0 || length(unique(na.omit(df_model[[outcome]]))) <= 1) return(out)

  fml <- as.formula(paste0(
    outcome,
    " ~ MTO + Fintech + Crypto + Amount250 + Online + seqpos_centered + MTO:late + Fintech:late + Crypto:late | confederate_match_key"
  ))

  mod <- tryCatch(
    fixest::feols(fml, data = df_model, cluster = ~confederate_match_key, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(mod)) return(out)

  ct <- tryCatch(as.data.frame(fixest::coeftable(mod)), error = function(e) NULL)
  if (is.null(ct)) return(out)
  ct$term <- rownames(ct)

  for (tt in terms) {
    if (tt %in% ct$term && "t value" %in% names(ct)) {
      out[[tt]] <- abs(ct[ct$term == tt, "t value"][[1]])
    }
  }
  out
}

fit_temporal_stats <- function(data_list) {
  specs <- list(
    success = list(data = data_list$sap, outcome = "success"),
    kyc_any = list(data = data_list$sap, outcome = "kyc_any"),
    cost_success = list(data = data_list$cost_success, outcome = "total_cost_without_time_usd"),
    time_duration = list(data = data_list$duration, outcome = "transaction_duration_hours")
  )

  purrr::imap(specs, function(spec, outcome_label) {
    stats <- get_temporal_abs_t(spec$data, spec$outcome)
    names(stats) <- paste(outcome_label, names(stats), sep = "__")
    stats
  }) |>
    unlist(use.names = TRUE)
}

permute_order_within_confederate <- function(df) {
  df |>
    group_by(confederate_match_key) |>
    mutate(
      seqpos = sample(seqpos, size = dplyr::n(), replace = FALSE),
      seqpos_centered = seqpos - mean(seqpos, na.rm = TRUE),
      late = as.numeric(seqpos > stats::median(seqpos, na.rm = TRUE))
    ) |>
    ungroup()
}

run_temporal_permutation <- function(B = B_EXPLORATORY, seed = SEED_MAIN + 2L) {
  set.seed(seed)

  observed_data_list <- list(
    sap = sap_samples$first_pass_observed_primary,
    cost_success = cost_success_samples$first_pass_observed_primary,
    duration = duration_samples$first_pass_observed_primary
  )

  t_obs <- fit_temporal_stats(observed_data_list)
  t_null <- replicate(B, {
    permuted_data_list <- list(
      sap = permute_order_within_confederate(observed_data_list$sap),
      cost_success = permute_order_within_confederate(observed_data_list$cost_success),
      duration = permute_order_within_confederate(observed_data_list$duration)
    )
    fit_temporal_stats(permuted_data_list)
  })
  t_null <- t(t_null)
  colnames(t_null) <- names(t_obs)

  p_perm <- free_stepdown_maxT(t_obs, t_null)

  tibble(
    test_id = names(t_obs),
    outcome_label = stringr::str_replace(test_id, "__(seqpos_centered|MTO:late|Fintech:late|Crypto:late)$", ""),
    term = stringr::str_extract(test_id, "seqpos_centered|MTO:late|Fintech:late|Crypto:late"),
    observed_abs_t = as.numeric(t_obs),
    p_temporal_permutation = as.numeric(p_perm),
    significant_temporal_permutation_05 = p_temporal_permutation < 0.05,
    temporal_B = B,
    temporal_seed = seed,
    note = "Exploratory design-based temporal/order diagnostic. This tests sequence/time contamination, not channel effects."
  )
}

if (RUN_TEMPORAL_PERMUTATION) {
  temporal_permutation <- run_temporal_permutation(B = B_EXPLORATORY, seed = SEED_MAIN + 2L)
} else {
  temporal_permutation <- tibble()
}

safe_write_csv(temporal_permutation, file.path(etable_dir, "IADB_08R_temporal_permutation.csv"))

# ------------------------------------------------------------------------------
# Realized MDE diagnostics -----------------------------------------------------
# ------------------------------------------------------------------------------
if (RUN_REALIZED_MDE) {
  realized_mde_table <- multiplicity_base |>
    filter(term %in% c("MTO", "Fintech", "Crypto", "Amount250", "Online")) |>
    mutate(
      alpha = 0.05,
      target_power = 0.80,
      z_alpha_two_sided = stats::qnorm(1 - alpha / 2),
      z_power = stats::qnorm(target_power),
      mde_approx = (z_alpha_two_sided + z_power) * std.error,
      practical_threshold = case_when(
        outcome_label == "success" ~ 0.05,
        outcome_label == "kyc_any" ~ 0.05,
        outcome_label == "cost_success" ~ 5,
        outcome_label == "time_duration" ~ 2,
        TRUE ~ NA_real_
      ),
      can_rule_out_threshold = abs(conf.low) < practical_threshold & abs(conf.high) < practical_threshold,
      note = "Approximate realized MDE = (1.96 + 0.84) * CR2 SE; use for null-result phrasing, not as exact design power."
    )
} else {
  realized_mde_table <- tibble()
}

safe_write_csv(realized_mde_table, file.path(etable_dir, "IADB_08R_realized_mde_table.csv"))

# ------------------------------------------------------------------------------
# Skipped-model and sample diagnostics -----------------------------------------
# ------------------------------------------------------------------------------
model_sample_summary <- bind_rows(
  imap_dfr(sap_samples, ~ tibble(
    model_family = "success_kyc",
    sample = .y,
    n_rows = nrow(.x),
    n_clusters = n_distinct(.x$confederate_match_key),
    n_success_nonmissing = sum(!is.na(.x$success)),
    n_kyc_any_nonmissing = sum(!is.na(.x$kyc_any)),
    n_kyc_enhanced_nonmissing = sum(!is.na(.x$kyc_enhanced)),
    n_kyc_0_3_nonmissing = sum(!is.na(.x$kyc_score_0_3)),
    n_kyc_0_5_nonmissing = sum(!is.na(.x$kyc_score_0_5))
  )),
  imap_dfr(cost_success_samples, ~ tibble(
    model_family = "cost_success",
    sample = .y,
    n_rows = nrow(.x),
    n_clusters = n_distinct(.x$confederate_match_key),
    n_outcome_nonmissing = sum(!is.na(.x$total_cost_without_time_usd))
  )),
  imap_dfr(duration_samples, ~ tibble(
    model_family = "time_duration",
    sample = .y,
    n_rows = nrow(.x),
    n_clusters = n_distinct(.x$confederate_match_key),
    n_outcome_nonmissing = sum(!is.na(.x$transaction_duration_hours))
  ))
)

skipped_models <- all_results |>
  filter(skipped == TRUE) |>
  distinct(model_id, outcome_label, sample_label, model_label, formula, skip_reason)

safe_write_csv(model_sample_summary, file.path(etable_dir, "IADB_08R_model_sample_summary.csv"))
safe_write_csv(skipped_models, file.path(etable_dir, "IADB_08R_skipped_models.csv"))

# ------------------------------------------------------------------------------
# Console summary --------------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n=== IADB SCRIPT 08 BOSS-REVISION COMPLETE ===\n")
cat("Outputs saved in:\n")
cat("  ", etable_dir, "\n", sep = "")
cat("\nCore files:\n")
cat("  - IADB_08R_core_results_cr2.csv\n")
cat("  - IADB_08R_core_results_with_mht.csv\n")
cat("  - IADB_08R_family_A_channel_RW.csv\n")
cat("  - IADB_08R_family_B_amount_delivery_WY.csv\n")
cat("  - IADB_08R_coverage_adherence_summary.csv\n")
cat("  - IADB_08R_lee_bounds_cost_duration.csv\n")
cat("  - IADB_08R_collinearity_diagnostics.csv\n")
cat("  - IADB_08R_temporal_permutation.csv\n")
cat("\nInterpretation note:\n")
cat("  Channel comparisons are associational/descriptive.\n")
cat("  Confirmatory inference uses RW-WCB for channel and WY permutation for amount/delivery.\n")
cat("  BH-FDR columns are complementary, not confirmatory.\n")
