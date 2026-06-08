# ==============================================================================
# IADB - 04 Build Analysis Samples and Attrition Diagnostics -------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: May 19, 2026 -----------------------------------------------------------
# Minimal revisions implemented on June 1st
# Purpose:
#   1. Diagnose observation loss from SurveyCTO to SAP samples;
#   2. Build a maximal automated matched sample;
#   3. Keep one best SurveyCTO row per schedule slot;
#   4. Preserve protocol deviations for ITT analysis;
#   5. Keep the conservative 155-row sample as a sensitivity sample.
# ==============================================================================

# Cleaning my environment 
rm(list = ls())

# Managing memory
gc()

# ------------------------------------------------------------------------------
# Replication notes ------------------------------------------------------------
# ------------------------------------------------------------------------------
# Required inputs:
#   These files must already exist from Script 03:
#   - IADB_03_surveycto_schedule_matched_full_audit.csv
#   - IADB_sap_schedule_level_base.rds
#   - IADB_sap_observed_first_pass.rds
#   - IADB_sap_attempted_after_funding.rds
#   - IADB_sap_per_protocol.rds
#   - IADB_sap_reviewed_submissions.rds
#   - IADB_03_slot_level_duplicate_resolution.csv
#
# What to change before running:
#   - Update `output_dir` so it points to the local folder where Script 03 saved
#     the SAP dataset-builder outputs.
#   - This script reads Script 03 outputs from `output_dir` and also saves its
#     own outputs to the same folder.
#
# Example:
#   output_dir <- "C:/Users/YourName/Drive/IADB_outputs/data/clean/sap_dataset_builder"

# Requited packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(janitor)
  library(lubridate)
})

# Output directory
output_dir <- "D:/Evaluasi/data/clean/sap_dataset_builder"

# ------------------------------------------------------------------------------
# Load files -------------------------------------------------------------------
# ------------------------------------------------------------------------------
full_audit <- readr::read_csv(
  file.path(output_dir, "IADB_03_surveycto_schedule_matched_full_audit.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names()

sap_base_slot <- readRDS(
  file.path(output_dir, "IADB_sap_schedule_level_base.rds")
) |>
  janitor::clean_names()

sap_observed_slot <- readRDS(
  file.path(output_dir, "IADB_sap_observed_first_pass.rds")
) |>
  janitor::clean_names()

sap_attempted_after_funding <- readRDS(
  file.path(output_dir, "IADB_sap_attempted_after_funding.rds")
) |>
  janitor::clean_names()

sap_per_protocol_slot <- readRDS(
  file.path(output_dir, "IADB_sap_per_protocol.rds")
) |>
  janitor::clean_names()

sap_reviewed_submissions <- readRDS(
  file.path(output_dir, "IADB_sap_reviewed_submissions.rds")
) |>
  janitor::clean_names()

slot_duplicate_resolution <- readr::read_csv(
  file.path(output_dir, "IADB_03_slot_level_duplicate_resolution.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names()

# ------------------------------------------------------------------------------
# Core validation checks --------------------------------------------------------
# ------------------------------------------------------------------------------
check_unique_slots <- function(data, sample_name) {
  checks <- data |>
    summarise(
      sample = sample_name,
      n_rows = n(),
      n_unique_transactions = n_distinct(unique_transaction_id),
      missing_unique_transaction_id = sum(is.na(unique_transaction_id)),
      duplicate_transaction_rows = n_rows - n_unique_transactions
    )
  
  print(checks)
  
  if (checks$missing_unique_transaction_id > 0) {
    stop(sample_name, " has missing unique_transaction_id values.")
  }
  
  if (checks$duplicate_transaction_rows > 0) {
    stop(sample_name, " has duplicate unique_transaction_id values.")
  }
  
  checks
}

slot_base_checks <- check_unique_slots(
  sap_base_slot,
  "slot_level_schedule_base"
)

slot_observed_checks <- check_unique_slots(
  sap_observed_slot,
  "slot_level_observed"
)

slot_per_protocol_checks <- check_unique_slots(
  sap_per_protocol_slot,
  "slot_level_per_protocol"
)

# Reviewed-submissions sample is allowed to have duplicate unique_transaction_id
# because it preserves valid reviewed submissions excluded only from slot-level SAP.
reviewed_submission_checks <- sap_reviewed_submissions |>
  summarise(
    sample = "reviewed_submissions",
    n_rows = n(),
    n_unique_transactions = n_distinct(unique_transaction_id),
    missing_unique_transaction_id = sum(is.na(unique_transaction_id)),
    duplicate_transaction_rows = n_rows - n_unique_transactions,
    excluded_from_slot_level_sap =
      sum(exclude_from_slot_level_sap, na.rm = TRUE)
  )

print(reviewed_submission_checks)

# ------------------------------------------------------------------------------
# Duplicate-resolution diagnostics ---------------------------------------------
# ------------------------------------------------------------------------------
slot_duplicate_resolution_summary <- slot_duplicate_resolution |>
  count(slot_level_resolution_action, name = "n") |>
  arrange(desc(n))

print(slot_duplicate_resolution_summary)

# ------------------------------------------------------------------------------
# Attrition / sample comparison -------------------------------------------------
# ------------------------------------------------------------------------------
sample_comparison <- tibble(
  sample = c(
    "cleaned_surveycto_rows_after_script_03_review",
    "reviewed_submissions_preserved",
    "slot_level_observed",
    "slot_level_attempted_after_funding",
    "slot_level_per_protocol"
  ),
  n = c(
    nrow(full_audit),
    nrow(sap_reviewed_submissions),
    nrow(sap_observed_slot),
    nrow(sap_attempted_after_funding),
    nrow(sap_per_protocol_slot)
  ),
  n_unique_transactions = c(
    n_distinct(full_audit$survey_instance_id),
    n_distinct(sap_reviewed_submissions$unique_transaction_id),
    n_distinct(sap_observed_slot$unique_transaction_id),
    n_distinct(sap_attempted_after_funding$unique_transaction_id),
    n_distinct(sap_per_protocol_slot$unique_transaction_id)
  )
)

print(sample_comparison)

# ------------------------------------------------------------------------------
# Sample quality diagnostics ----------------------------------------------------
# ------------------------------------------------------------------------------
summarise_analysis_sample <- function(data, sample_name) {
  data |>
    summarise(
      sample = sample_name,
      n = n(),
      n_unique_transactions = n_distinct(unique_transaction_id),
      n_confederates = n_distinct(confederate_match_key, na.rm = TRUE),
      n_countries = n_distinct(country, na.rm = TRUE),
      
      missing_success = sum(is.na(success)),
      missing_kyc = sum(is.na(kyc_score)),
      
      completed_missing_cost_local =
        sum(success == 1 & is.na(cost_local), na.rm = TRUE),
      
      completed_missing_time =
        sum(success == 1 & is.na(time_hours), na.rm = TRUE),
      
      protocol_deviations =
        sum(!treatment_adherent, na.rm = TRUE),
      
      mean_success = mean(success, na.rm = TRUE),
      mean_kyc = mean(kyc_score, na.rm = TRUE),
      
      .groups = "drop"
    )
}

sample_quality_summary <- bind_rows(
  summarise_analysis_sample(
    sap_observed_slot,
    "slot_level_observed"
  ),
  summarise_analysis_sample(
    sap_attempted_after_funding,
    "slot_level_attempted_after_funding"
  ),
  summarise_analysis_sample(
    sap_per_protocol_slot,
    "slot_level_per_protocol"
  ),
  summarise_analysis_sample(
    sap_reviewed_submissions,
    "reviewed_submissions_preserved"
  )
)

print(sample_quality_summary)

# ------------------------------------------------------------------------------
# Backward-compatible aliases for downstream scripts ---------------------------
# ------------------------------------------------------------------------------
# After Script 03 revisions, the authoritative strict slot-level sample is already
# IADB_sap_observed_first_pass. We therefore save backward-compatible aliases.
sap_base_maximal_auto <- sap_base_slot
sap_observed_maximal_auto <- sap_observed_slot
sap_per_protocol_maximal_auto <- sap_per_protocol_slot

# No separate "needs review maximal auto" sample is needed anymore because Script 03
# already resolves/excludes rows and writes duplicate-resolution diagnostics.
sap_needs_review_maximal_auto <- sap_observed_slot |>
  filter(FALSE)

# ------------------------------------------------------------------------------
# Saving outputs ---------------------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(
  sap_base_maximal_auto,
  file.path(output_dir, "IADB_sap_schedule_level_base_maximal_auto.csv")
)

saveRDS(
  sap_base_maximal_auto,
  file.path(output_dir, "IADB_sap_schedule_level_base_maximal_auto.rds")
)

write_csv(
  sap_observed_maximal_auto,
  file.path(output_dir, "IADB_sap_observed_maximal_auto.csv")
)

saveRDS(
  sap_observed_maximal_auto,
  file.path(output_dir, "IADB_sap_observed_maximal_auto.rds")
)

write_csv(
  sap_per_protocol_maximal_auto,
  file.path(output_dir, "IADB_sap_per_protocol_maximal_auto.csv")
)

saveRDS(
  sap_per_protocol_maximal_auto,
  file.path(output_dir, "IADB_sap_per_protocol_maximal_auto.rds")
)

write_csv(
  sap_needs_review_maximal_auto,
  file.path(output_dir, "IADB_sap_maximal_auto_needs_manual_review.csv")
)

write_csv(
  sample_comparison,
  file.path(output_dir, "IADB_04_sample_comparison.csv")
)

write_csv(
  sample_quality_summary,
  file.path(output_dir, "IADB_04_sample_quality_summary.csv")
)

write_csv(
  slot_duplicate_resolution_summary,
  file.path(output_dir, "IADB_04_slot_duplicate_resolution_summary.csv")
)

cat("\nScript 04 completed successfully.\n")
cat("Strict slot-level observed sample rows: ", nrow(sap_observed_slot), "\n")
cat("Reviewed-submissions rows: ", nrow(sap_reviewed_submissions), "\n")
cat("Per-protocol slot-level rows: ", nrow(sap_per_protocol_slot), "\n")
