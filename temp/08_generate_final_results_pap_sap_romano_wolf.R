# ==============================================================================
# IADB - 08 Generate Final PAP/SAP Results --------------------------------------
# Author: Cedric Antunes (Evaluasi)
# Date: June 2026
#
# Purpose:
#   Generate PAP/SAP-aligned final results tables for the IADB KYC/AML audit study.
#
# What this script does:
#   1. Loads already-cleaned final SAP analysis datasets.
#   2. Standardizes modeling variables without changing raw/cleaned data.
#   3. Keeps PAP-primary outcomes separate from sensitivity outcomes.
#   4. Estimates PAP/SAP Model 1, Model 2, Model 3, and a preferred
#      confederate fixed-effects adjusted model.
#   5. Uses CR2 cluster-robust standard errors at the confederate level for the
#      PAP/SAP model families via estimatr::lm_robust().
#   6. Exports coefficient-level CSVs, sample diagnostics, skipped-model logs,
#      and multiple-testing correction tables.
#
# What this script does NOT do:
#   - It does not clean SurveyCTO data.
#   - It does not perform matching/recovery/manual review.
#   - It does not decide which submitted rows are valid.
#   - It does not silently convert a 0-5 KYC scale into the PAP-primary 0-3 scale.
#
# Main PAP/SAP interpretation:
#   - Model 2 is the adjusted PAP/SAP model: channel + amount + delivery.
#   - The confederate FE model is treated as the preferred within-confederate
#     adjusted specification because the realized audit has repeated transactions
#     by each confederate. Since confederates are nested in countries, country FE
#     and confederate FE cannot both be interpreted as independent controls.
#   - The 0-3 KYC score is the PAP-primary KYC outcome.
#   - The 0-5 KYC composite is exported as a sensitivity outcome only.
#   - Cost and time primary models are estimated on successful transactions only,
#     following the PAP/SAP measurement logic. Any-attempt cost and alternative
#     time measures are exported as sensitivity/descriptive models.
#
# Inputs:
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol.rds
#   data/clean/sap_dataset_builder/IADB_sap_reviewed_submissions.rds
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_reviewed_submissions_cost_time.rds
#
# Outputs:
#   data/clean/sap_dataset_builder/final_etables/
#     IADB_08_pap_primary_models_cr2.csv
#     IADB_08_pap_primary_models_cr2.rds
#     IADB_08_pap_all_models_cr2.csv
#     IADB_08_pap_all_models_cr2.rds
#     IADB_08_multiplicity_channel_family.csv
#     IADB_08_multiplicity_transaction_family.csv
#     IADB_08_model_sample_summary.csv
#     IADB_08_skipped_models.csv
#     IADB_08_primary_outcome_definition_log.csv
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
  library(estimatr)   # lm_robust(..., se_type = "CR2")
  library(broom)      # tidy model outputs
  library(fixest)     # FE models for Romano-Wolf wild bootstrap correction
})

# Optional package. The script does not require modelsummary, but will use it for
# publication-style .tex tables if it is installed.
has_modelsummary <- requireNamespace("modelsummary", quietly = TRUE)

# ------------------------------------------------------------------------------
# 1. Paths ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_dir <- here("data", "clean", "sap_dataset_builder")
etable_dir <- file.path(sap_dir, "final_etables")

dir.create(etable_dir, showWarnings = FALSE, recursive = TRUE)

# Success/KYC datasets ---------------------------------------------------------
sap_main_path <- file.path(sap_dir, "IADB_sap_observed_first_pass.rds")
sap_pp_path <- file.path(sap_dir, "IADB_sap_per_protocol.rds")
sap_reviewed_path <- file.path(sap_dir, "IADB_sap_reviewed_submissions.rds")

# Cost/time datasets -----------------------------------------------------------
ct_main_path <- file.path(sap_dir, "IADB_sap_observed_first_pass_cost_time.rds")
ct_pp_path <- file.path(sap_dir, "IADB_sap_per_protocol_cost_time.rds")
ct_reviewed_path <- file.path(sap_dir, "IADB_sap_reviewed_submissions_cost_time.rds")

required_files <- c(
  sap_main_path,
  sap_pp_path,
  sap_reviewed_path,
  ct_main_path,
  ct_pp_path,
  ct_reviewed_path
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------------------------
# 2. General helpers -----------------------------------------------------------
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

# Convenience wrapper for binary sample flags. If a flag is missing, the script
# stops rather than silently changing the analysis sample.
filter_required_flag <- function(df, flag_name) {
  if (!flag_name %in% names(df)) {
    stop("Required sample flag is missing: ", flag_name)
  }
  df |> filter(as_logical_safe(.data[[flag_name]]))
}

# ------------------------------------------------------------------------------
# 3. Standardize modeling variables -------------------------------------------
# ------------------------------------------------------------------------------
standardize_model_vars <- function(df) {
  df |>
    clean_names() |>
    add_missing_cols(c(
      # PAP-primary outcomes and sensitivity outcomes
      "success",
      "kyc_score",
      "kyc_score_composite_0_5",
      "total_cost_without_time_usd",
      "reported_time_hours",
      "interaction_time_hours",
      "transaction_duration_hours",
      # assignment variables
      "assigned_channel",
      "assigned_amount",
      "assigned_delivery",
      # clustering/blocking/context variables
      "confederate_match_key",
      "country",
      # quality flags
      "needs_manual_review_for_final"
    )) |>
    mutate(
      # Binary success outcome. Keep as numeric 0/1 for LPM.
      success = to_num(success),
      
      # PAP-primary KYC scale: 0-3 only.
      # IMPORTANT: Do not fall back to the 0-5 composite.
      kyc_score_0_3 = to_num(kyc_score),
      
      # Sensitivity/exploratory KYC scale: 0-5 composite.
      kyc_score_0_5 = to_num(kyc_score_composite_0_5),
      
      # Treatment assignment variables. These should reflect the randomized
      # schedule/assigned protocol, not ex-post actual behavior.
      assigned_channel = factor(
        assigned_channel,
        levels = c("Banks", "MTOs", "Fintech", "Crypto")
      ),
      assigned_delivery = factor(
        assigned_delivery,
        levels = c("In-person", "Online")
      ),
      assigned_amount = to_num(assigned_amount),
      
      # Channel indicators. Banks are the omitted reference category.
      MTO = as.numeric(assigned_channel == "MTOs"),
      Fintech = as.numeric(assigned_channel == "Fintech"),
      Crypto = as.numeric(assigned_channel == "Crypto"),
      
      # Transaction characteristic indicators.
      Amount250 = as.numeric(assigned_amount == 250),
      Online = as.numeric(assigned_delivery == "Online"),
      
      # Standardize country/confederate fields.
      country = as.factor(country),
      confederate_match_key = as.factor(confederate_match_key),
      
      needs_manual_review_for_final =
        as_logical_safe(needs_manual_review_for_final)
    )
}

# Model data restriction: keep only rows with non-missing outcome, cluster ID,
# and assignment variables needed for all PAP/SAP specifications.
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
# 4. Safe CR2 model estimation -------------------------------------------------
# ------------------------------------------------------------------------------
# lm_robust() with se_type = "CR2" implements the small-cluster correction
# pre-specified in the SAP. This is the inferential source of truth.
safe_lm_cr2 <- function(formula, data, cluster = "confederate_match_key") {
  outcome_name <- all.vars(formula)[1]
  
  if (nrow(data) == 0) {
    return(list(model = NULL, reason = "zero-row estimation sample"))
  }
  
  if (!outcome_name %in% names(data)) {
    return(list(model = NULL, reason = paste0("outcome not found: ", outcome_name)))
  }
  
  y <- data[[outcome_name]]
  if (length(unique(na.omit(y))) <= 1) {
    return(list(model = NULL, reason = "outcome is constant or all missing"))
  }
  
  if (!cluster %in% names(data)) {
    return(list(model = NULL, reason = paste0("cluster variable not found: ", cluster)))
  }
  
  out <- tryCatch(
    {
      estimatr::lm_robust(
        formula = formula,
        data = data,
        clusters = data[[cluster]],
        se_type = "CR2"
      )
    },
    error = function(e) e
  )
  
  if (inherits(out, "error")) {
    return(list(model = NULL, reason = out$message))
  }
  
  list(model = out, reason = NA_character_)
}

# Estimate PAP/SAP model family for one outcome and one sample.
#
# Models:
#   M1_unadjusted:       Y ~ channel indicators
#   M2_adjusted:         Y ~ channel indicators + amount + delivery
#   M3_country_fe:       M2 + country fixed effects, when country is available
#   M2_confederate_fe:   M2 + confederate fixed effects; preferred repeated-audit
#                        specification for within-confederate comparisons.
#
# Note: M3_country_fe and M2_confederate_fe should not be stacked together when
# confederates are nested in countries. Confederate FE absorb country-level
# differences.
estimate_pap_model_family <- function(df, outcome, outcome_label, sample_label) {
  df_model <- prep_model_data(df, outcome)
  
  has_country <- "country" %in% names(df_model) &&
    dplyr::n_distinct(stats::na.omit(df_model$country)) > 1
  
  formulas <- list(
    M1_unadjusted = as.formula(
      paste0(outcome, " ~ MTO + Fintech + Crypto")
    ),
    M2_adjusted = as.formula(
      paste0(outcome, " ~ MTO + Fintech + Crypto + Amount250 + Online")
    ),
    M2_confederate_fe = as.formula(
      paste0(
        outcome,
        " ~ MTO + Fintech + Crypto + Amount250 + Online + factor(confederate_match_key)"
      )
    )
  )
  
  if (has_country) {
    formulas$M3_country_fe <- as.formula(
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

# Convert model list to tidy coefficient table.
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
# 5. Load final SAP datasets ---------------------------------------------------
# ------------------------------------------------------------------------------
sap_main <- readRDS(sap_main_path) |> standardize_model_vars()
sap_pp <- readRDS(sap_pp_path) |> standardize_model_vars()
sap_reviewed <- readRDS(sap_reviewed_path) |> standardize_model_vars()

ct_main <- readRDS(ct_main_path) |> standardize_model_vars()
ct_pp <- readRDS(ct_pp_path) |> standardize_model_vars()
ct_reviewed <- readRDS(ct_reviewed_path) |> standardize_model_vars()

sap_samples <- list(
  main_strict_slot_level = sap_main,
  per_protocol_strict_slot = sap_pp,
  reviewed_submissions = sap_reviewed
)

ct_samples <- list(
  main_strict_slot_level = ct_main,
  per_protocol_strict_slot = ct_pp,
  reviewed_submissions = ct_reviewed
)

# ------------------------------------------------------------------------------
# 6. Define PAP-primary and sensitivity samples --------------------------------
# ------------------------------------------------------------------------------
# Cost/time flags are expected to be created upstream. This script does not
# redefine them because doing so would change the analysis sample outside the
# SAP dataset builder.
cost_any_samples <- purrr::map(
  ct_samples,
  ~ filter_required_flag(.x, "sample_cost_usd_any_attempt")
)

cost_success_samples <- purrr::map(
  ct_samples,
  ~ filter_required_flag(.x, "sample_cost_usd_success_only")
)

reported_time_samples <- purrr::map(
  ct_samples,
  ~ filter_required_flag(.x, "sample_reported_time")
)

interaction_time_samples <- purrr::map(
  ct_samples,
  ~ filter_required_flag(.x, "sample_interaction_time")
)

duration_samples <- purrr::map(
  ct_samples,
  ~ filter_required_flag(.x, "sample_transaction_duration")
)

# Primary outcome definitions. This log is useful for the report appendix.
primary_outcome_definition_log <- tribble(
  ~outcome_label, ~outcome_variable, ~dataset_family, ~analysis_sample, ~pap_status, ~notes,
  "success", "success", "success_kyc", "all valid attempted/observed transactions in main strict slot-level sample", "PAP primary", "Linear probability model; 1 = completed/successful transaction.",
  "kyc_0_3", "kyc_score_0_3", "success_kyc", "all codable transactions in main strict slot-level sample", "PAP primary", "Ordinal 0-3 score treated linearly in primary OLS/LPM-style model; ordinal model belongs in robustness.",
  "cost_success", "total_cost_without_time_usd", "cost_time", "successful transactions only", "PAP primary", "Primary cost model follows PAP measurement logic: cost observed for successful transactions.",
  "time_duration", "transaction_duration_hours", "cost_time", "successful transactions with observed transaction duration", "PAP primary / preferred operationalization", "Use one primary time measure in main table; report reported/interaction time as sensitivity.",
  "kyc_0_5", "kyc_score_0_5", "success_kyc", "all codable transactions", "Sensitivity", "Composite 0-5 KYC score; never substituted into the PAP-primary 0-3 outcome.",
  "cost_any", "total_cost_without_time_usd", "cost_time", "any attempt with observed monetary cost", "Sensitivity", "Useful descriptive/sensitivity estimand but not the primary PAP cost estimand.",
  "reported_time", "reported_time_hours", "cost_time", "rows passing sample_reported_time", "Sensitivity", "Alternative time measure.",
  "interaction_time", "interaction_time_hours", "cost_time", "rows passing sample_interaction_time", "Sensitivity", "Alternative time measure."
)

write_csv(
  primary_outcome_definition_log,
  file.path(etable_dir, "IADB_08_primary_outcome_definition_log.csv")
)

# ------------------------------------------------------------------------------
# 7. Estimate PAP/SAP model families ------------------------------------------
# ------------------------------------------------------------------------------
# 7.1 Main PAP-primary outcomes ------------------------------------------------
# These are the models to privilege in the report.
primary_model_lists <- list(
  success = estimate_pap_model_family(
    sap_samples$main_strict_slot_level,
    outcome = "success",
    outcome_label = "success",
    sample_label = "main_strict_slot_level"
  ),
  kyc_0_3 = estimate_pap_model_family(
    sap_samples$main_strict_slot_level,
    outcome = "kyc_score_0_3",
    outcome_label = "kyc_0_3",
    sample_label = "main_strict_slot_level"
  ),
  cost_success = estimate_pap_model_family(
    cost_success_samples$main_strict_slot_level,
    outcome = "total_cost_without_time_usd",
    outcome_label = "cost_success",
    sample_label = "main_strict_slot_level"
  ),
  time_duration = estimate_pap_model_family(
    duration_samples$main_strict_slot_level,
    outcome = "transaction_duration_hours",
    outcome_label = "time_duration",
    sample_label = "main_strict_slot_level"
  )
) |>
  purrr::flatten()

primary_results <- tidy_model_list(primary_model_lists)

# 7.2 Sensitivity/sample-robustness outcomes ----------------------------------
# These are not the core confirmatory table, but help document robustness.
all_model_lists <- list()

# Success and KYC across all three samples
for (sample_name in names(sap_samples)) {
  all_model_lists <- c(
    all_model_lists,
    estimate_pap_model_family(
      sap_samples[[sample_name]],
      outcome = "success",
      outcome_label = "success",
      sample_label = sample_name
    ),
    estimate_pap_model_family(
      sap_samples[[sample_name]],
      outcome = "kyc_score_0_3",
      outcome_label = "kyc_0_3",
      sample_label = sample_name
    ),
    estimate_pap_model_family(
      sap_samples[[sample_name]],
      outcome = "kyc_score_0_5",
      outcome_label = "kyc_0_5_sensitivity",
      sample_label = sample_name
    )
  )
}

# Cost/time across all three samples and all operationalizations
for (sample_name in names(ct_samples)) {
  all_model_lists <- c(
    all_model_lists,
    estimate_pap_model_family(
      cost_success_samples[[sample_name]],
      outcome = "total_cost_without_time_usd",
      outcome_label = "cost_success",
      sample_label = sample_name
    ),
    estimate_pap_model_family(
      cost_any_samples[[sample_name]],
      outcome = "total_cost_without_time_usd",
      outcome_label = "cost_any_sensitivity",
      sample_label = sample_name
    ),
    estimate_pap_model_family(
      duration_samples[[sample_name]],
      outcome = "transaction_duration_hours",
      outcome_label = "time_duration",
      sample_label = sample_name
    ),
    estimate_pap_model_family(
      reported_time_samples[[sample_name]],
      outcome = "reported_time_hours",
      outcome_label = "reported_time_sensitivity",
      sample_label = sample_name
    ),
    estimate_pap_model_family(
      interaction_time_samples[[sample_name]],
      outcome = "interaction_time_hours",
      outcome_label = "interaction_time_sensitivity",
      sample_label = sample_name
    )
  )
}

all_results <- tidy_model_list(all_model_lists)

# ------------------------------------------------------------------------------
# 8. Multiple-testing correction tables ----------------------------------------
# ------------------------------------------------------------------------------
# PAP/SAP hypothesis families:
#   Family 1: channel comparisons across the four co-primary outcomes
#             3 channel terms x 4 outcomes = 12 tests.
#             PAP-primary correction: Romano-Wolf stepdown FWER correction.
#   Family 2: randomized transaction characteristics across co-primary outcomes
#             Amount250 and Online x 4 outcomes = 8 tests.
#             PAP-primary correction: Holm-Bonferroni FWER correction.
#
# Important implementation note:
#   The coefficient tables above use estimatr::lm_robust(..., se_type = "CR2"),
#   which is the preferred small-cluster analytic inference for the individual
#   regressions. The Romano-Wolf correction below is implemented using wild
#   cluster bootstrap test statistics from fwildclusterboot + wildrwolf.
#
#   Implementation choice:
#   We use base lm() with explicit confederate fixed effects for the bootstrap
#   models. This avoids known data re-evaluation problems that can occur when
#   fwildclusterboot::boottest() is called on fixest models estimated inside
#   helper functions. The lm() bootstrap models have the same right-hand-side
#   specification as the preferred M2_confederate_fe model used in the CR2
#   coefficient tables.
#
#   We therefore keep the CR2 coefficient table as the source for estimates and
#   raw CR2 p-values, and add the Romano-Wolf adjusted p-values for Family 1.

preferred_model_for_multiplicity <- "M2_confederate_fe"

multiplicity_base <- primary_results |>
  filter(
    skipped == FALSE,
    sample_label == "main_strict_slot_level",
    model_label == preferred_model_for_multiplicity,
    outcome_label %in% c("success", "kyc_0_3", "cost_success", "time_duration")
  )

# 8.1 Family 1: channel comparisons, Romano-Wolf stepdown ----------------------
# The family contains 12 tests:
#   MTO, Fintech, Crypto x success, kyc_0_3, cost_success, time_duration.
#
# This block follows the non-standard-family workflow:
#   1. Estimate the four preferred FE models with lm() and explicit confederate FE.
#   2. For each channel coefficient in each model, run a wild cluster bootstrap.
#   3. Collect bootstrapped t-statistics and pass them to
#      wildrwolf::get_rwolf_pval().

run_romano_wolf_channel_family <- function(
  B = 9999,
  seed = 20260602,
  boot_type = "rademacher",
  bootstrap_type = "fnw11",
  engine = "R"
) {
  
  required_rw_packages <- c("fwildclusterboot", "wildrwolf")
  missing_rw_packages <- required_rw_packages[
    !vapply(required_rw_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  ]
  
  if (length(missing_rw_packages) > 0) {
    stop(
      "Romano-Wolf correction requires these package(s): ",
      paste(missing_rw_packages, collapse = ", "),
      "\nInstall with:\n",
      "install.packages(c('dqrng', 'fwildclusterboot', 'wildrwolf'), repos = c('https://s3alfisc.r-universe.dev', 'https://cloud.r-project.org'))\n"
    )
  }
  
  rw_outcome_specs <- list(
    success = list(
      outcome_label = "success",
      outcome = "success",
      data = sap_samples$main_strict_slot_level
    ),
    kyc_0_3 = list(
      outcome_label = "kyc_0_3",
      outcome = "kyc_score_0_3",
      data = sap_samples$main_strict_slot_level
    ),
    cost_success = list(
      outcome_label = "cost_success",
      outcome = "total_cost_without_time_usd",
      data = cost_success_samples$main_strict_slot_level
    ),
    time_duration = list(
      outcome_label = "time_duration",
      outcome = "transaction_duration_hours",
      data = duration_samples$main_strict_slot_level
    )
  )
  
  channel_terms <- c("MTO", "Fintech", "Crypto")
  
  estimate_lm_model <- function(spec) {
    df_model <- prep_model_data(spec$data, spec$outcome) |>
      mutate(
        # Ensure the FE and cluster variable is present in the model frame.
        confederate_match_key = factor(confederate_match_key)
      )
    
    fml <- as.formula(paste0(
      spec$outcome,
      " ~ MTO + Fintech + Crypto + Amount250 + Online + confederate_match_key"
    ))
    
    stats::lm(fml, data = df_model)
  }
  
  lm_models <- purrr::map(rw_outcome_specs, estimate_lm_model)
  
  test_grid <- tidyr::crossing(
    outcome_label = names(rw_outcome_specs),
    term = channel_terms
  ) |>
    mutate(test_id = paste(outcome_label, term, sep = "__"))
  
  run_one_boot_test <- function(outcome_label, term) {
    # Set both RNGs. fwildclusterboot uses dqrng for common weight types.
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
  
  boot_tests <- purrr::map2(
    test_grid$outcome_label,
    test_grid$term,
    run_one_boot_test
  )
  
  boot_t_stats <- boot_tests |>
    purrr::map(~ .x[["t_boot"]]) |>
    Reduce(f = cbind)
  
  observed_t_stats <- boot_tests |>
    purrr::map(fwildclusterboot::teststat) |>
    unlist(use.names = FALSE)
  
  p_rw <- wildrwolf::get_rwolf_pval(
    t_stats = observed_t_stats,
    boot_t_stats = boot_t_stats
  )
  
  test_grid |>
    mutate(
      p_romano_wolf = as.numeric(p_rw),
      significant_romano_wolf_05 = p_romano_wolf < 0.05,
      rw_bootstrap_B = B,
      rw_seed = seed,
      rw_boot_type = boot_type,
      rw_bootstrap_type = bootstrap_type,
      rw_engine = engine,
      rw_bootstrap_model = "lm_explicit_confederate_fe"
    )
}

romano_wolf_channel <- run_romano_wolf_channel_family(
  B = 9999,
  seed = 20260602,
  boot_type = "rademacher",
  bootstrap_type = "fnw11",
  engine = "R"
)

channel_family <- multiplicity_base |>
  filter(term %in% c("MTO", "Fintech", "Crypto")) |>
  mutate(
    family = "Family 1: channel comparisons",
    p_bh_supplementary = p.adjust(p.value, method = "BH"),
    p_holm_supplementary = p.adjust(p.value, method = "holm"),
    significant_raw_05 = p.value < 0.05,
    significant_bh_supplementary_05 = p_bh_supplementary < 0.05,
    significant_holm_supplementary_05 = p_holm_supplementary < 0.05
  ) |>
  left_join(
    romano_wolf_channel |>
      select(
        outcome_label,
        term,
        p_romano_wolf,
        significant_romano_wolf_05,
        rw_bootstrap_B,
        rw_seed,
        rw_boot_type,
        rw_bootstrap_type,
        rw_engine,
        rw_bootstrap_model
      ),
    by = c("outcome_label", "term")
  ) |>
  arrange(p_romano_wolf, p.value)

# 8.2 Family 2: amount and delivery, Holm-Bonferroni ---------------------------
# This follows the PAP/SAP for the transaction-characteristics family.
transaction_family <- multiplicity_base |>
  filter(term %in% c("Amount250", "Online")) |>
  mutate(
    family = "Family 2: transaction characteristics",
    p_holm = p.adjust(p.value, method = "holm"),
    p_bh_supplementary = p.adjust(p.value, method = "BH"),
    significant_raw_05 = p.value < 0.05,
    significant_holm_05 = p_holm < 0.05,
    significant_bh_supplementary_05 = p_bh_supplementary < 0.05
  ) |>
  arrange(p_holm, p.value)

write_csv(
  channel_family,
  file.path(etable_dir, "IADB_08_multiplicity_channel_family.csv")
)

write_csv(
  transaction_family,
  file.path(etable_dir, "IADB_08_multiplicity_transaction_family.csv")
)

# ------------------------------------------------------------------------------
# 9. Sample-size and skipped-model diagnostics ---------------------------------
# ------------------------------------------------------------------------------
model_sample_summary <- bind_rows(
  imap_dfr(sap_samples, ~ tibble(
    model_family = "success_kyc",
    sample = .y,
    n_rows = nrow(.x),
    n_clusters = n_distinct(.x$confederate_match_key),
    n_success_nonmissing = sum(!is.na(.x$success)),
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
  imap_dfr(cost_any_samples, ~ tibble(
    model_family = "cost_any_sensitivity",
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
  )),
  imap_dfr(reported_time_samples, ~ tibble(
    model_family = "reported_time_sensitivity",
    sample = .y,
    n_rows = nrow(.x),
    n_clusters = n_distinct(.x$confederate_match_key),
    n_outcome_nonmissing = sum(!is.na(.x$reported_time_hours))
  )),
  imap_dfr(interaction_time_samples, ~ tibble(
    model_family = "interaction_time_sensitivity",
    sample = .y,
    n_rows = nrow(.x),
    n_clusters = n_distinct(.x$confederate_match_key),
    n_outcome_nonmissing = sum(!is.na(.x$interaction_time_hours))
  ))
)

skipped_models <- all_results |>
  filter(skipped == TRUE) |>
  distinct(
    model_id,
    outcome_label,
    sample_label,
    model_label,
    formula,
    skip_reason
  )

write_csv(
  model_sample_summary,
  file.path(etable_dir, "IADB_08_model_sample_summary.csv")
)

write_csv(
  skipped_models,
  file.path(etable_dir, "IADB_08_skipped_models.csv")
)

# ------------------------------------------------------------------------------
# 10. Export model results -----------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(
  primary_results,
  file.path(etable_dir, "IADB_08_pap_primary_models_cr2.csv")
)

write_csv(
  all_results,
  file.path(etable_dir, "IADB_08_pap_all_models_cr2.csv")
)

saveRDS(
  primary_model_lists,
  file.path(etable_dir, "IADB_08_pap_primary_models_cr2.rds")
)

saveRDS(
  all_model_lists,
  file.path(etable_dir, "IADB_08_pap_all_models_cr2.rds")
)

# Optional: publication-style LaTeX tables using modelsummary ------------------
# These are convenience outputs only. The authoritative coefficient-level output
# is the CSV produced above.
if (has_modelsummary) {
  primary_nonnull <- primary_model_lists |>
    purrr::keep(~ !is.null(.x$model)) |>
    purrr::map("model")
  
  # Shorten table to the preferred M2_confederate_fe models for the four primary
  # outcomes. This mirrors the main report table.
  primary_preferred <- primary_model_lists |>
    purrr::keep(~ !is.null(.x$model)) |>
    purrr::keep(~ .x$model_label == preferred_model_for_multiplicity) |>
    purrr::map("model")
  
  if (length(primary_preferred) > 0) {
    modelsummary::modelsummary(
      primary_preferred,
      output = file.path(etable_dir, "IADB_08_primary_preferred_models.tex"),
      stars = TRUE,
      gof_omit = "AIC|BIC|Log|F|RMSE",
      coef_map = c(
        "MTO" = "MTOs",
        "Fintech" = "Fintech",
        "Crypto" = "Crypto",
        "Amount250" = "Amount: USD 250",
        "Online" = "Online delivery"
      ),
      notes = paste(
        "CR2 cluster-robust standard errors at the confederate level.",
        "Preferred model includes confederate fixed effects.",
        "Banks, USD 100, and in-person are reference categories."
      )
    )
  }
}

# ------------------------------------------------------------------------------
# 11. Console summary ----------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n=== IADB PAP/SAP RESULTS SCRIPT COMPLETE ===\n")
cat("Primary CR2 results saved to:\n")
cat("  ", file.path(etable_dir, "IADB_08_pap_primary_models_cr2.csv"), "\n", sep = "")
cat("All CR2 results saved to:\n")
cat("  ", file.path(etable_dir, "IADB_08_pap_all_models_cr2.csv"), "\n", sep = "")
cat("Multiple-testing tables saved to:\n")
cat("  ", file.path(etable_dir, "IADB_08_multiplicity_channel_family.csv"), "\n", sep = "")
cat("  ", file.path(etable_dir, "IADB_08_multiplicity_transaction_family.csv"), "\n", sep = "")
cat("\nImportant interpretation note:\n")
cat("  Raw p-values should not be used alone for confirmatory claims.\n")
cat("  Use the multiplicity tables for channel and transaction-characteristic families.\n")
