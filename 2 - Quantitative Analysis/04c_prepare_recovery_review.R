# ==============================================================================
# IADB - 04c Recovery Review Compatibility Check -------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Revised: June 2026
# Purpose:
#   1. Confirm that manual recovery review is no longer required;
#   2. Verify that Script 03 produced duplicate-resolution outputs;
#   3. Write empty compatibility outputs expected by older pipeline versions.
# ==============================================================================

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(janitor)
})

# ------------------------------------------------------------------------------
# Replication notes ------------------------------------------------------------
# ------------------------------------------------------------------------------
# Required inputs:
#   These files must already exist from Scripts 03 and 04b:
#   - IADB_03_slot_level_duplicate_resolution.csv
#   - IADB_sap_observed_first_pass.rds
#   - IADB_sap_reviewed_submissions.rds
#   - IADB_04b_duplicate_resolution_audit_summary.csv
#
# What to change before running:
#   - Update `output_dir` so it points to the local folder where Scripts 03 and
#     04b saved the SAP dataset-builder outputs.
#   - This script reads the required inputs from `output_dir` and saves its own
#     recovery-review compatibility files to the same folder.
#
# Example:
#   output_dir <- "C:/Users/YourName/Drive/IADB_outputs/data/clean/sap_dataset_builder"

output_dir <- "D:/Evaluasi/data/clean/sap_dataset_builder"

# ------------------------------------------------------------------------------
# Required Script 03 / 04b outputs ---------------------------------------------
# ------------------------------------------------------------------------------
required_paths <- c(
  file.path(output_dir, "IADB_03_slot_level_duplicate_resolution.csv"),
  file.path(output_dir, "IADB_sap_observed_first_pass.rds"),
  file.path(output_dir, "IADB_sap_reviewed_submissions.rds"),
  file.path(output_dir, "IADB_04b_duplicate_resolution_audit_summary.csv")
)

missing_paths <- required_paths[!file.exists(required_paths)]

if (length(missing_paths) > 0) {
  stop(
    "Script 04c requires the revised Script 03 and Script 04b outputs. Missing:\n",
    paste(missing_paths, collapse = "\n")
  )
}

slot_resolution <- read_csv(
  file.path(output_dir, "IADB_03_slot_level_duplicate_resolution.csv"),
  show_col_types = FALSE
) |>
  clean_names()

sap_observed <- readRDS(
  file.path(output_dir, "IADB_sap_observed_first_pass.rds")
) |>
  clean_names()

sap_reviewed <- readRDS(
  file.path(output_dir, "IADB_sap_reviewed_submissions.rds")
) |>
  clean_names()

audit_04b <- read_csv(
  file.path(output_dir, "IADB_04b_duplicate_resolution_audit_summary.csv"),
  show_col_types = FALSE
) |>
  clean_names()

# ------------------------------------------------------------------------------
# Compatibility outputs ---------------------------------------------------------
# ------------------------------------------------------------------------------
empty_template <- tibble(
  survey_instance_id = character(),
  suggested_action = character(),
  safe_to_auto_recover_candidate = logical(),
  original_matched_slot = character(),
  candidate_schedule_slot_id = character(),
  recovery_score = numeric(),
  recovery_action = character(),
  corrected_schedule_slot_id = character(),
  recovery_note = character()
)

write_csv(
  empty_template,
  file.path(output_dir, "IADB_04c_all_strong_recovery_candidates.csv")
)

write_csv(
  empty_template,
  file.path(output_dir, "IADB_04c_candidate_slot_pressure.csv")
)

write_csv(
  empty_template,
  file.path(output_dir, "IADB_04c_strong_recovery_candidates_for_review.csv")
)

write_csv(
  empty_template,
  file.path(output_dir, "IADB_04c_recovery_decisions_template.csv")
)

summary_04c <- tibble(
  item = c(
    "manual_recovery_review_needed",
    "strict_slot_level_observed_rows",
    "strict_slot_level_unique_transactions",
    "reviewed_submissions_rows",
    "reviewed_submissions_unique_transactions",
    "rows_excluded_only_from_slot_level_sap",
    "slot_resolution_rows",
    "strong_recovery_candidate_rows",
    "manual_decision_rows_to_review"
  ),
  n = c(
    0,
    nrow(sap_observed),
    n_distinct(sap_observed$unique_transaction_id),
    nrow(sap_reviewed),
    n_distinct(sap_reviewed$unique_transaction_id),
    sum(sap_reviewed$exclude_from_slot_level_sap, na.rm = TRUE),
    nrow(slot_resolution),
    0,
    0
  ),
  note = c(
    "No separate 04c recovery review is needed; Script 03 now handles duplicate-slot recovery/resolution.",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "Rows retained in reviewed-submissions but excluded from strict slot-level SAP.",
    "Rows involved in duplicate-slot resolution from Script 03.",
    "No additional strong recovery candidates generated by revised pipeline.",
    "No manual 04c decisions required."
  )
)

write_csv(
  summary_04c,
  file.path(output_dir, "IADB_04c_recovery_review_summary.csv")
)

cat("\nScript 04c completed successfully.\n")
cat("No separate recovery review is required under the revised pipeline.\n")
cat("Strict slot-level observed rows: ", nrow(sap_observed), "\n")
cat("Reviewed-submissions rows: ", nrow(sap_reviewed), "\n")
cat(
  "Rows excluded only from strict slot-level SAP: ",
  sum(sap_reviewed$exclude_from_slot_level_sap, na.rm = TRUE),
  "\n"
)
