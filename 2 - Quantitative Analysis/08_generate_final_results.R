# ==============================================================================
# IADB - 08 Generate Final etable Outputs --------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: May 2026 ---------------------------------------------------------------
#
# Purpose:
#   Generate final fixest::etable outputs for all SAP models/specifications.
#
# This script does NOT clean, merge, match, or transform raw data.
# It only loads final analysis datasets and estimates/exports tables.
#
# Inputs:
#   data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto.rds
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
#
#   data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_observed_clean_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_observed_conservative_cost_time.rds
#
# Outputs:
#   data/clean/sap_dataset_builder/final_etables/
#     IADB_07_success_models.*
#     IADB_07_kyc_models.*
#     IADB_07_main_outcomes_models.*
#     IADB_07_cost_any_attempt_models.*
#     IADB_07_cost_success_only_models.*
#     IADB_07_reported_time_models.*
#     IADB_07_interaction_time_models.*
#     IADB_07_transaction_duration_models.*
#     IADB_07_all_models.*
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
  library(fixest)
})

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_dir <- here(
  "data",
  "clean",
  "sap_dataset_builder"
)

etable_dir <- file.path(
  sap_dir,
  "final_etables"
)

dir.create(
  etable_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# Success/KYC datasets
sap_main_path <- file.path(
  sap_dir,
  "IADB_sap_observed_maximal_auto.rds"
)

sap_pp_path <- file.path(
  sap_dir,
  "IADB_sap_per_protocol_maximal_auto.rds"
)

sap_conservative_path <- file.path(
  sap_dir,
  "IADB_sap_observed_first_pass.rds"
)

# Cost/time datasets
ct_main_path <- file.path(
  sap_dir,
  "IADB_sap_observed_maximal_auto_cost_time.rds"
)

ct_clean_path <- file.path(
  sap_dir,
  "IADB_sap_observed_clean_cost_time.rds"
)

ct_pp_path <- file.path(
  sap_dir,
  "IADB_sap_per_protocol_maximal_auto_cost_time.rds"
)

ct_conservative_path <- file.path(
  sap_dir,
  "IADB_sap_observed_conservative_cost_time.rds"
)

required_files <- c(
  sap_main_path,
  sap_pp_path,
  sap_conservative_path,
  ct_main_path,
  ct_clean_path,
  ct_pp_path,
  ct_conservative_path
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
to_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

# Safe numeric
as_logical_safe <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  
  x_clean <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    x_clean %in% c("true", "t", "1", "yes", "sim", "si") ~ TRUE,
    x_clean %in% c("false", "f", "0", "no") ~ FALSE,
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

standardize_model_vars <- function(df) {
  df |>
    clean_names() |>
    add_missing_cols(c(
      "success",
      "kyc_score",
      "kyc_score_composite_0_5",
      "assigned_channel",
      "assigned_amount",
      "assigned_delivery",
      "confederate_match_key",
      "needs_manual_review_for_final"
    )) |>
    mutate(
      success = to_num(success),
      
      # Prefer kyc_score if present; otherwise fall back to the composite name.
      kyc_score = case_when(
        !is.na(to_num(kyc_score)) ~ to_num(kyc_score),
        !is.na(to_num(kyc_score_composite_0_5)) ~
          to_num(kyc_score_composite_0_5),
        TRUE ~ NA_real_
      ),
      
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
      
      needs_manual_review_for_final =
        as_logical_safe(needs_manual_review_for_final)
    )
}

# safe models predictors
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

# Safe FE data
safe_feols <- function(formula, data, vcov = ~ confederate_match_key) {
  outcome_name <- all.vars(formula)[1]
  y <- data[[outcome_name]]
  
  if (nrow(data) == 0) {
    message("Skipping model because data has zero rows: ", outcome_name)
    return(NULL)
  }
  
  if (length(unique(na.omit(y))) <= 1) {
    message("Skipping model because outcome is constant or all missing: ", outcome_name)
    return(NULL)
  }
  
  tryCatch(
    feols(
      formula,
      data = data,
      vcov = vcov
    ),
    error = function(e) {
      message("Skipping model due to estimation error: ", outcome_name)
      message(e$message)
      NULL
    }
  )
}

# Safe confederate FE specification
estimate_model_set <- function(outcome, samples_named, prefix) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key"
    )
  )
  
  out <- purrr::imap(
    samples_named,
    function(df, sample_label) {
      df_model <- prep_model_data(df, outcome)
      safe_feols(fml, data = df_model)
    }
  )
  
  names(out) <- paste0("m_", prefix, "_", names(samples_named))
  
  out
}

export_etable <- function(models, file_stub, title = NULL) {
  models_nonnull <- models[
    !vapply(models, is.null, logical(1))
  ]
  
  if (length(models_nonnull) == 0) {
    warning("No non-null models to export for: ", file_stub)
    return(invisible(NULL))
  }
  
  # TXT output
  do.call(
    etable,
    c(
      models_nonnull,
      list(
        tex = FALSE,
        file = file.path(etable_dir, paste0(file_stub, ".txt")),
        title = title
      )
    )
  )
  
  # TeX output
  do.call(
    etable,
    c(
      models_nonnull,
      list(
        tex = TRUE,
        file = file.path(etable_dir, paste0(file_stub, ".tex")),
        title = title
      )
    )
  )
  
  # RDS output
  saveRDS(
    models_nonnull,
    file.path(etable_dir, paste0(file_stub, ".rds"))
  )
  
  cat("\n=== ", file_stub, " ===\n", sep = "")
  print(
    do.call(
      etable,
      c(
        models_nonnull,
        list(tex = FALSE, title = title)
      )
    )
  )
  
  invisible(models_nonnull)
}

# ------------------------------------------------------------------------------
# Loading success/KYC SAP datasets ---------------------------------------------
# ------------------------------------------------------------------------------
sap_main <- readRDS(sap_main_path) |>
  standardize_model_vars()

sap_clean <- sap_main |>
  filter(!needs_manual_review_for_final)

sap_pp <- readRDS(sap_pp_path) |>
  standardize_model_vars()

sap_conservative <- readRDS(sap_conservative_path) |>
  standardize_model_vars()

sap_samples <- list(
  main = sap_main,
  clean = sap_clean,
  pp = sap_pp,
  conservative = sap_conservative
)

# ------------------------------------------------------------------------------
# Loading cost/time datasets ---------------------------------------------------
# ------------------------------------------------------------------------------
ct_main <- readRDS(ct_main_path) |>
  standardize_model_vars()

ct_clean <- readRDS(ct_clean_path) |>
  standardize_model_vars()

ct_pp <- readRDS(ct_pp_path) |>
  standardize_model_vars()

ct_conservative <- readRDS(ct_conservative_path) |>
  standardize_model_vars()

ct_samples <- list(
  main = ct_main,
  clean = ct_clean,
  pp = ct_pp,
  conservative = ct_conservative
)

# ------------------------------------------------------------------------------
# Building model samples -------------------------------------------------------
# ------------------------------------------------------------------------------
cost_any_samples <- purrr::map(
  ct_samples,
  ~ .x |> filter(sample_cost_usd_any_attempt)
)

cost_success_samples <- purrr::map(
  ct_samples,
  ~ .x |> filter(sample_cost_usd_success_only)
)

reported_time_samples <- purrr::map(
  ct_samples,
  ~ .x |> filter(sample_reported_time)
)

interaction_time_samples <- purrr::map(
  ct_samples,
  ~ .x |> filter(sample_interaction_time)
)

duration_samples <- purrr::map(
  ct_samples,
  ~ .x |> filter(sample_transaction_duration)
)

# ------------------------------------------------------------------------------
# Estimating all model groups --------------------------------------------------
# ------------------------------------------------------------------------------
success_models <- estimate_model_set(
  outcome = "success",
  samples_named = sap_samples,
  prefix = "success"
)

kyc_models <- estimate_model_set(
  outcome = "kyc_score",
  samples_named = sap_samples,
  prefix = "kyc"
)

cost_any_models <- estimate_model_set(
  outcome = "total_cost_without_time_usd",
  samples_named = cost_any_samples,
  prefix = "cost_any"
)

cost_success_models <- estimate_model_set(
  outcome = "total_cost_without_time_usd",
  samples_named = cost_success_samples,
  prefix = "cost_success"
)

reported_time_models <- estimate_model_set(
  outcome = "reported_time_hours",
  samples_named = reported_time_samples,
  prefix = "reported_time"
)

interaction_time_models <- estimate_model_set(
  outcome = "interaction_time_hours",
  samples_named = interaction_time_samples,
  prefix = "interaction_time"
)

duration_models <- estimate_model_set(
  outcome = "transaction_duration_hours",
  samples_named = duration_samples,
  prefix = "duration"
)

# ------------------------------------------------------------------------------
# Main-outcome table: main sample only -----------------------------------------
# ------------------------------------------------------------------------------
main_outcome_models <- list(
  m_success_main = success_models$m_success_main,
  m_kyc_main = kyc_models$m_kyc_main,
  m_cost_main = cost_any_models$m_cost_any_main,
  m_reported_time_main = reported_time_models$m_reported_time_main,
  m_interaction_time_main = interaction_time_models$m_interaction_time_main,
  m_duration_main = duration_models$m_duration_main
)

# ------------------------------------------------------------------------------
# All models list --------------------------------------------------------------
# ------------------------------------------------------------------------------
all_models <- c(
  success_models,
  kyc_models,
  cost_any_models,
  cost_success_models,
  reported_time_models,
  interaction_time_models,
  duration_models
)

all_models_nonnull <- all_models[
  !vapply(all_models, is.null, logical(1))
]

skipped_models <- tibble(
  model = names(all_models)[vapply(all_models, is.null, logical(1))],
  reason = "Outcome is constant, all missing, zero-row sample, or model could not be estimated."
)

# ------------------------------------------------------------------------------
# Model sample summary ---------------------------------------------------------
# ------------------------------------------------------------------------------
model_sample_summary <- bind_rows(
  imap_dfr(sap_samples, ~ tibble(model_family = "success_kyc", sample = .y, n = nrow(.x))),
  imap_dfr(cost_any_samples, ~ tibble(model_family = "cost_any", sample = .y, n = nrow(.x))),
  imap_dfr(cost_success_samples, ~ tibble(model_family = "cost_success", sample = .y, n = nrow(.x))),
  imap_dfr(reported_time_samples, ~ tibble(model_family = "reported_time", sample = .y, n = nrow(.x))),
  imap_dfr(interaction_time_samples, ~ tibble(model_family = "interaction_time", sample = .y, n = nrow(.x))),
  imap_dfr(duration_samples, ~ tibble(model_family = "transaction_duration", sample = .y, n = nrow(.x)))
)

write_csv(
  model_sample_summary,
  file.path(etable_dir, "IADB_07_model_sample_summary.csv")
)

write_csv(
  skipped_models,
  file.path(etable_dir, "IADB_07_skipped_models.csv")
)

saveRDS(
  all_models,
  file.path(etable_dir, "IADB_07_all_models_full_list.rds")
)

saveRDS(
  all_models_nonnull,
  file.path(etable_dir, "IADB_07_all_models_nonnull.rds")
)

# ------------------------------------------------------------------------------
# Exporting etables ------------------------------------------------------------
# ------------------------------------------------------------------------------
export_etable(
  main_outcome_models,
  file_stub = "IADB_07_main_outcomes_models",
  title = "Main SAP Outcomes: Main Sample"
)

export_etable(
  success_models,
  file_stub = "IADB_07_success_models",
  title = "Success Models across Samples"
)

export_etable(
  kyc_models,
  file_stub = "IADB_07_kyc_models",
  title = "KYC Models across Samples"
)

export_etable(
  cost_any_models,
  file_stub = "IADB_07_cost_any_attempt_models",
  title = "Monetary Cost Models: Any Attempt with Observed Cost"
)

export_etable(
  cost_success_models,
  file_stub = "IADB_07_cost_success_only_models",
  title = "Monetary Cost Models: Successful Transactions Only"
)

export_etable(
  reported_time_models,
  file_stub = "IADB_07_reported_time_models",
  title = "Reported Time Models"
)

export_etable(
  interaction_time_models,
  file_stub = "IADB_07_interaction_time_models",
  title = "Interaction Time Models"
)

export_etable(
  duration_models,
  file_stub = "IADB_07_transaction_duration_models",
  title = "Transaction Duration Models"
)

export_etable(
  all_models_nonnull,
  file_stub = "IADB_07_all_models",
  title = "All SAP Models"
)
