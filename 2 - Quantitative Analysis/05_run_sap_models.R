# ==============================================================================
# IADB - 05 Run SAP Models -----------------------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Revised: June 2026 -----------------------------------------------------------
# Rvisions: June, 2026
# Purpose:
#   1. Run main SAP success/KYC models on the strict slot-level sample;
#   2. Run attempted-after-funding sensitivity models;
#   3. Run per-protocol sensitivity models;
#   4. Run reviewed-submissions sensitivity models.
# ==============================================================================

# Cleaning my environment 
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(readr)
  library(janitor)
})

# ------------------------------------------------------------------------------
# Replication notes ------------------------------------------------------------
# ------------------------------------------------------------------------------
# Required inputs:
#   These files must already exist from Script 03:
#   - IADB_sap_observed_first_pass.rds
#   - IADB_sap_attempted_after_funding.rds
#   - IADB_sap_per_protocol.rds
#   - IADB_sap_reviewed_submissions.rds
#
# What to change before running:
#   - Update `output_dir` so it points to the local folder where Script 03 saved
#     the SAP dataset-builder outputs.
#   - `results_dir` is created automatically inside `output_dir` and stores the
#     model tables and diagnostics produced by this script.
#
# Example:
#   output_dir <- "C:/Users/YourName/Drive/IADB_outputs/data/clean/sap_dataset_builder"
#   results_dir <- file.path(output_dir, "sap_results_maximal_auto")

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
output_dir <- "D:/Evaluasi/data/clean/sap_dataset_builder"

results_dir <- file.path(output_dir, "sap_results_maximal_auto")

dir.create(
  results_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# ------------------------------------------------------------------------------
# Loading authoritative Script 03 samples --------------------------------------
# ------------------------------------------------------------------------------
sap_main <- readRDS(
  file.path(output_dir, "IADB_sap_observed_first_pass.rds")
) |>
  clean_names()

sap_after_funding <- readRDS(
  file.path(output_dir, "IADB_sap_attempted_after_funding.rds")
) |>
  clean_names()

sap_pp <- readRDS(
  file.path(output_dir, "IADB_sap_per_protocol.rds")
) |>
  clean_names()

sap_reviewed <- readRDS(
  file.path(output_dir, "IADB_sap_reviewed_submissions.rds")
) |>
  clean_names()

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Safe logical
as_logical_safe <- function(x) {
  case_when(
    is.logical(x) ~ x,
    as.character(x) %in% c("TRUE", "true", "1", "Yes", "yes") ~ TRUE,
    as.character(x) %in% c("FALSE", "false", "0", "No", "no") ~ FALSE,
    TRUE ~ FALSE
  )
}

# Safe missing columns
add_missing_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) {
      df[[cc]] <- NA
    }
  }
  
  df
}

# df analysis
prep_sap_model <- function(df) {
  df |>
    add_missing_cols(c(
      "unique_transaction_id",
      "confederate_match_key",
      "success",
      "kyc_score",
      "assigned_channel",
      "assigned_amount",
      "assigned_delivery",
      "channel_std",
      "amount",
      "delivery_std",
      "country",
      "country_clean",
      "country_schedule_clean",
      "treatment_adherent",
      "exclude_from_slot_level_sap"
    )) |>
    mutate(
      success = suppressWarnings(as.numeric(success)),
      kyc_score = suppressWarnings(as.numeric(kyc_score)),
      
      assigned_channel = as.character(assigned_channel),
      assigned_delivery = as.character(assigned_delivery),
      assigned_amount = suppressWarnings(as.numeric(assigned_amount)),
      
      channel_std = as.character(channel_std),
      delivery_std = as.character(delivery_std),
      amount = suppressWarnings(as.numeric(amount)),
      
      country = coalesce(
        as.character(country),
        as.character(country_clean),
        as.character(country_schedule_clean)
      ),
      
      treatment_adherent = as_logical_safe(treatment_adherent),
      exclude_from_slot_level_sap =
        as_logical_safe(exclude_from_slot_level_sap),
      
      MTO = as.numeric(assigned_channel == "MTOs"),
      Fintech = as.numeric(assigned_channel == "Fintech"),
      Crypto = as.numeric(assigned_channel == "Crypto"),
      Amount250 = as.numeric(assigned_amount == 250),
      Online = as.numeric(assigned_delivery == "Online"),
      
      observed_MTO = as.numeric(channel_std == "MTOs"),
      observed_Fintech = as.numeric(channel_std == "Fintech"),
      observed_Crypto = as.numeric(channel_std == "Crypto"),
      observed_Amount250 = as.numeric(amount == 250),
      observed_Online = as.numeric(delivery_std == "Online")
    )
}

# Empirical specifications with confederate FEs
safe_feols <- function(formula, data, vcov = ~ confederate_match_key) {
  outcome_name <- all.vars(formula)[1]
  y <- data[[outcome_name]]
  
  if (length(unique(na.omit(y))) <= 1) {
    message("Skipping model because outcome is constant: ", outcome_name)
    return(NULL)
  }
  
  tryCatch(
    feols(
      formula,
      data = data,
      vcov = vcov
    ),
    error = function(e) {
      message("Skipping model because estimation failed: ", outcome_name)
      message(e$message)
      return(NULL)
    }
  )
}

# ------------------------------------------------------------------------------
# Preparing samples ------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_main_m <- prep_sap_model(sap_main)
sap_after_funding_m <- prep_sap_model(sap_after_funding)
sap_pp_m <- prep_sap_model(sap_pp)
sap_reviewed_m <- prep_sap_model(sap_reviewed)

# ------------------------------------------------------------------------------
# Sample diagnostics -----------------------------------------------------------
# ------------------------------------------------------------------------------
sample_summary <- bind_rows(
  sap_main_m |>
    summarise(
      sample = "main_strict_slot_level",
      n = n(),
      n_unique_transactions = n_distinct(unique_transaction_id),
      duplicate_transaction_rows = n - n_unique_transactions,
      n_confederates = n_distinct(confederate_match_key),
      n_countries = n_distinct(country),
      mean_success = mean(success, na.rm = TRUE),
      mean_kyc = mean(kyc_score, na.rm = TRUE),
      excluded_from_slot_level_sap = sum(exclude_from_slot_level_sap, na.rm = TRUE)
    ),
  
  sap_after_funding_m |>
    summarise(
      sample = "attempted_after_funding",
      n = n(),
      n_unique_transactions = n_distinct(unique_transaction_id),
      duplicate_transaction_rows = n - n_unique_transactions,
      n_confederates = n_distinct(confederate_match_key),
      n_countries = n_distinct(country),
      mean_success = mean(success, na.rm = TRUE),
      mean_kyc = mean(kyc_score, na.rm = TRUE),
      excluded_from_slot_level_sap = sum(exclude_from_slot_level_sap, na.rm = TRUE)
    ),
  
  sap_pp_m |>
    summarise(
      sample = "per_protocol_strict_slot_level",
      n = n(),
      n_unique_transactions = n_distinct(unique_transaction_id),
      duplicate_transaction_rows = n - n_unique_transactions,
      n_confederates = n_distinct(confederate_match_key),
      n_countries = n_distinct(country),
      mean_success = mean(success, na.rm = TRUE),
      mean_kyc = mean(kyc_score, na.rm = TRUE),
      excluded_from_slot_level_sap = sum(exclude_from_slot_level_sap, na.rm = TRUE)
    ),
  
  sap_reviewed_m |>
    summarise(
      sample = "reviewed_submissions_preserved",
      n = n(),
      n_unique_transactions = n_distinct(unique_transaction_id),
      duplicate_transaction_rows = n - n_unique_transactions,
      n_confederates = n_distinct(confederate_match_key),
      n_countries = n_distinct(country),
      mean_success = mean(success, na.rm = TRUE),
      mean_kyc = mean(kyc_score, na.rm = TRUE),
      excluded_from_slot_level_sap = sum(exclude_from_slot_level_sap, na.rm = TRUE)
    )
)

main_by_channel <- sap_main_m |>
  group_by(assigned_channel) |>
  summarise(
    n = n(),
    n_confederates = n_distinct(confederate_match_key),
    success_mean = mean(success, na.rm = TRUE),
    kyc_mean = mean(kyc_score, na.rm = TRUE),
    kyc_sd = sd(kyc_score, na.rm = TRUE),
    protocol_deviations = sum(!treatment_adherent, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  sample_summary,
  file.path(results_dir, "sap_sample_summary.csv")
)

write_csv(
  main_by_channel,
  file.path(results_dir, "sap_main_by_channel.csv")
)

cat("\n=== Sample summary ===\n")
print(sample_summary)

cat("\n=== Main strict slot-level sample by assigned channel ===\n")
print(main_by_channel)

# ------------------------------------------------------------------------------
# Main models: strict slot-level sample -----------------------------------------
# ------------------------------------------------------------------------------
m_success_main <- safe_feols(
  success ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_main_m
)

m_kyc_main <- safe_feols(
  kyc_score ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_main_m
)

# ------------------------------------------------------------------------------
# Sensitivity: attempted after funding ------------------------------------------
# ------------------------------------------------------------------------------
m_success_after_funding <- safe_feols(
  success ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_after_funding_m
)

m_kyc_after_funding <- safe_feols(
  kyc_score ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_after_funding_m
)

# ------------------------------------------------------------------------------
# Sensitivity: per-protocol -----------------------------------------------------
# ------------------------------------------------------------------------------
m_success_pp <- safe_feols(
  success ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_pp_m
)

m_kyc_pp <- safe_feols(
  kyc_score ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_pp_m
)

# ------------------------------------------------------------------------------
# Sensitivity: reviewed submissions --------------------------------------------
# ------------------------------------------------------------------------------
m_success_reviewed <- safe_feols(
  success ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_reviewed_m
)

m_kyc_reviewed <- safe_feols(
  kyc_score ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_reviewed_m
)

# ------------------------------------------------------------------------------
# Collecting and exporting models ----------------------------------------------
# ------------------------------------------------------------------------------
model_list <- list(
  m_success_main = m_success_main,
  m_success_after_funding = m_success_after_funding,
  m_success_pp = m_success_pp,
  m_success_reviewed = m_success_reviewed,
  
  m_kyc_main = m_kyc_main,
  m_kyc_after_funding = m_kyc_after_funding,
  m_kyc_pp = m_kyc_pp,
  m_kyc_reviewed = m_kyc_reviewed
)

model_list_nonnull <- model_list[
  !vapply(model_list, is.null, logical(1))
]

skipped_models <- tibble(
  model = names(model_list)[vapply(model_list, is.null, logical(1))],
  reason = "Outcome is constant or model could not be estimated."
)

write_csv(
  skipped_models,
  file.path(results_dir, "sap_models_skipped.csv")
)

# All models in one table.
do.call(
  etable,
  c(
    model_list_nonnull,
    list(
      tex = FALSE,
      file = file.path(results_dir, "sap_success_kyc_main_and_sensitivity.txt")
    )
  )
)

do.call(
  etable,
  c(
    model_list_nonnull,
    list(
      tex = TRUE,
      file = file.path(results_dir, "sap_success_kyc_main_and_sensitivity.tex")
    )
  )
)

# Success-only table.
success_models <- model_list_nonnull[
  names(model_list_nonnull) %in% c(
    "m_success_main",
    "m_success_after_funding",
    "m_success_pp",
    "m_success_reviewed"
  )
]

do.call(
  etable,
  c(
    success_models,
    list(
      tex = FALSE,
      file = file.path(results_dir, "sap_success_models.txt")
    )
  )
)

do.call(
  etable,
  c(
    success_models,
    list(
      tex = TRUE,
      file = file.path(results_dir, "sap_success_models.tex")
    )
  )
)

# KYC-only table.
kyc_models <- model_list_nonnull[
  names(model_list_nonnull) %in% c(
    "m_kyc_main",
    "m_kyc_after_funding",
    "m_kyc_pp",
    "m_kyc_reviewed"
  )
]

do.call(
  etable,
  c(
    kyc_models,
    list(
      tex = FALSE,
      file = file.path(results_dir, "sap_kyc_models.txt")
    )
  )
)

do.call(
  etable,
  c(
    kyc_models,
    list(
      tex = TRUE,
      file = file.path(results_dir, "sap_kyc_models.tex")
    )
  )
)

saveRDS(
  model_list,
  file.path(results_dir, "sap_success_kyc_main_and_sensitivity.rds")
)

cat("\n=== Skipped models ===\n")
print(skipped_models)

cat("\n=== SAP models: main and sensitivity ===\n")
print(
  do.call(
    etable,
    c(
      model_list_nonnull,
      list(tex = FALSE)
    )
  )
)
