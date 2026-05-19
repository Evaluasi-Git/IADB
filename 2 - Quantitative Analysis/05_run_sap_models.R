# ==============================================================================
# IADB - Run SAP Models --------------------------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: May 18, 2026 -----------------------------------------------------------
# Purpose:
#   1. Run main first-pass SAP models using maximal-auto matched sample.
#   2. Run sensitivity models excluding manual-review-flagged observations.
#   3. Run per-protocol robustness.
#   4. Run conservative-sample robustness.
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
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Output directory
output_dir <- "D:/Evaluasi/data/clean/sap_dataset_builder"

results_dir <- file.path(output_dir, "sap_results_maximal_auto")

dir.create(results_dir, 
           showWarnings = FALSE, 
           recursive = TRUE)

# ------------------------------------------------------------------------------
# Loading samples --------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_main <- readRDS(
  file.path(output_dir, "IADB_sap_observed_maximal_auto.rds")
) |>
  clean_names()

sap_pp <- readRDS(
  file.path(output_dir, "IADB_sap_per_protocol_maximal_auto.rds")
) |>
  clean_names()

sap_conservative <- readRDS(
  file.path(output_dir, "IADB_sap_observed_first_pass.rds")
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

# Safe missing cols 
add_missing_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) {
      df[[cc]] <- NA
    }
  }
  
  df
}

# Preparing models
prep_sap_model <- function(df) {
  df |>
    add_missing_cols(c(
      "needs_manual_review_for_final",
      "protocol_deviation",
      "channel_std",
      "amount",
      "delivery_std",
      "country"
    )) |>
    mutate(
      success = as.numeric(success),
      kyc_score = as.numeric(kyc_score),
      
      assigned_channel = factor(
        assigned_channel,
        levels = c("Banks", "MTOs", "Fintech", "Crypto")
      ),
      
      assigned_delivery = factor(
        assigned_delivery,
        levels = c("In-person", "Online")
      ),
      
      assigned_amount = as.numeric(assigned_amount),
      
      MTO = as.numeric(assigned_channel == "MTOs"),
      Fintech = as.numeric(assigned_channel == "Fintech"),
      Crypto = as.numeric(assigned_channel == "Crypto"),
      Amount250 = as.numeric(assigned_amount == 250),
      Online = as.numeric(assigned_delivery == "Online"),
      
      observed_MTO = as.numeric(channel_std == "MTOs"),
      observed_Fintech = as.numeric(channel_std == "Fintech"),
      observed_Crypto = as.numeric(channel_std == "Crypto"),
      observed_Amount250 = as.numeric(as.numeric(amount) == 250),
      observed_Online = as.numeric(delivery_std == "Online"),
      
      needs_manual_review_for_final =
        as_logical_safe(needs_manual_review_for_final),
      
      protocol_deviation =
        as_logical_safe(protocol_deviation)
    )
}

sap_main_m <- prep_sap_model(sap_main)

sap_clean_m <- sap_main_m |>
  filter(!needs_manual_review_for_final)

sap_pp_m <- prep_sap_model(sap_pp)

sap_conservative_m <- prep_sap_model(sap_conservative)

# ------------------------------------------------------------------------------
# Sample diagnostics -----------------------------------------------------------
# ------------------------------------------------------------------------------
sample_summary <- tibble(
  sample = c(
    "main_maximal_auto",
    "main_excluding_manual_review_flags",
    "per_protocol_maximal_auto",
    "conservative_firstpass"
  ),
  n = c(
    nrow(sap_main_m),
    nrow(sap_clean_m),
    nrow(sap_pp_m),
    nrow(sap_conservative_m)
  ),
  n_confederates = c(
    n_distinct(sap_main_m$confederate_match_key),
    n_distinct(sap_clean_m$confederate_match_key),
    n_distinct(sap_pp_m$confederate_match_key),
    n_distinct(sap_conservative_m$confederate_match_key)
  ),
  n_countries = c(
    n_distinct(sap_main_m$country),
    n_distinct(sap_clean_m$country),
    n_distinct(sap_pp_m$country),
    n_distinct(sap_conservative_m$country)
  ),
  mean_success = c(
    mean(sap_main_m$success, na.rm = TRUE),
    mean(sap_clean_m$success, na.rm = TRUE),
    mean(sap_pp_m$success, na.rm = TRUE),
    mean(sap_conservative_m$success, na.rm = TRUE)
  ),
  mean_kyc = c(
    mean(sap_main_m$kyc_score, na.rm = TRUE),
    mean(sap_clean_m$kyc_score, na.rm = TRUE),
    mean(sap_pp_m$kyc_score, na.rm = TRUE),
    mean(sap_conservative_m$kyc_score, na.rm = TRUE)
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
    manual_review_flagged = sum(needs_manual_review_for_final, na.rm = TRUE),
    protocol_deviations = sum(protocol_deviation, na.rm = TRUE),
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

cat("\n=== Main sample by assigned channel ===\n")
print(main_by_channel)

# ------------------------------------------------------------------------------
# Safe FEOLS -------------------------------------------------------------------
# ------------------------------------------------------------------------------
safe_feols <- function(formula, data, vcov = ~ confederate_match_key) {
  outcome_name <- all.vars(formula)[1]
  y <- data[[outcome_name]]
  
  if (length(unique(na.omit(y))) <= 1) {
    message("Skipping model because outcome is constant: ", outcome_name)
    return(NULL)
  }
  
  feols(
    formula,
    data = data,
    vcov = vcov
  )
}

# ------------------------------------------------------------------------------
# Main models: maximal-auto sample ---------------------------------------------
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
# Sensitivity: exclude manual-review-flagged rows ------------------------------
# ------------------------------------------------------------------------------
m_success_clean <- safe_feols(
  success ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_clean_m
)

m_kyc_clean <- safe_feols(
  kyc_score ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_clean_m
)

# ------------------------------------------------------------------------------
# Sensitivity: per-protocol ----------------------------------------------------
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
# Sensitivity: conservative 155-row sample -------------------------------------
# ------------------------------------------------------------------------------
m_success_conservative <- safe_feols(
  success ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_conservative_m
)

m_kyc_conservative <- safe_feols(
  kyc_score ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key,
  data = sap_conservative_m
)

# ------------------------------------------------------------------------------
# Collecting and exporting models ----------------------------------------------
# ------------------------------------------------------------------------------
model_list <- list(
  m_success_main = m_success_main,
  m_success_clean = m_success_clean,
  m_success_pp = m_success_pp,
  m_success_conservative = m_success_conservative,
  m_kyc_main = m_kyc_main,
  m_kyc_clean = m_kyc_clean,
  m_kyc_pp = m_kyc_pp,
  m_kyc_conservative = m_kyc_conservative
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
