# ==============================================================================
# IADB - 04b Duplicate Slot Resolution Audit -----------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: May 18, 2026 -----------------------------------------------------------
# Revised: June 2026 -----------------------------------------------------------
# Purpose:
#   1. Audit duplicate-slot resolution from Script 03;
#   2. Verify that the strict slot-level sample has one row per schedule slot;
#   3. Verify that duplicate-slot extras are retained in reviewed-submissions;
#   4. Document which rows were excluded only from the strict slot-level SAP sample.
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
  library(lubridate)
})

# ------------------------------------------------------------------------------
# Replication notes ------------------------------------------------------------
# ------------------------------------------------------------------------------
# Required inputs:
#   These files must already exist from Script 03:
#   - IADB_03_surveycto_schedule_matched_full_audit.csv
#   - IADB_sap_observed_first_pass.rds
#   - IADB_sap_reviewed_submissions.rds
#   - IADB_03_slot_level_duplicate_resolution.csv
#   - IADB_03_sap_merge_checks.csv
#
#   Optional inputs, if produced by Script 03:
#   - IADB_03_duplicate_slot_reassignment_log.csv
#   - IADB_03_final_duplicate_schedule_slots.csv
#
# What to change before running:
#   - Update `output_dir` so it points to the local folder where Script 03 saved
#     the SAP dataset-builder outputs.
#   - This script reads the required Script 03 outputs from `output_dir` and
#     saves its own duplicate-resolution audit files to the same folder.
#
# Example:
#   output_dir <- "C:/Users/YourName/Drive/IADB_outputs/data/clean/sap_dataset_builder"

# Output directory -------------------------------------------------------------
output_dir <- "D:/Evaluasi/data/clean/sap_dataset_builder"

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
as_logical_safe <- function(x) {
  case_when(
    is.logical(x) ~ x,
    as.character(x) %in% c("TRUE", "true", "1", "Yes", "yes") ~ TRUE,
    as.character(x) %in% c("FALSE", "false", "0", "No", "no") ~ FALSE,
    TRUE ~ FALSE
  )
}

add_missing_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) {
      df[[cc]] <- NA
    }
  }
  
  df
}

read_csv_if_exists <- function(path) {
  if (file.exists(path)) {
    read_csv(path, show_col_types = FALSE) |>
      clean_names()
  } else {
    tibble()
  }
}

# ------------------------------------------------------------------------------
# Required input paths ----------------------------------------------------------
# ------------------------------------------------------------------------------
full_audit_path <- file.path(
  output_dir,
  "IADB_03_surveycto_schedule_matched_full_audit.csv"
)

sap_observed_slot_path <- file.path(
  output_dir,
  "IADB_sap_observed_first_pass.rds"
)

sap_reviewed_submissions_path <- file.path(
  output_dir,
  "IADB_sap_reviewed_submissions.rds"
)

slot_resolution_path <- file.path(
  output_dir,
  "IADB_03_slot_level_duplicate_resolution.csv"
)

sap_merge_checks_path <- file.path(
  output_dir,
  "IADB_03_sap_merge_checks.csv"
)

duplicate_reassignment_log_path <- file.path(
  output_dir,
  "IADB_03_duplicate_slot_reassignment_log.csv"
)

final_duplicate_slots_path <- file.path(
  output_dir,
  "IADB_03_final_duplicate_schedule_slots.csv"
)

required_files <- c(
  full_audit_path,
  sap_observed_slot_path,
  sap_reviewed_submissions_path,
  slot_resolution_path,
  sap_merge_checks_path
)

missing_required <- required_files[!file.exists(required_files)]

if (length(missing_required) > 0) {
  stop(
    "Missing required Script 03 output files:\n",
    paste(missing_required, collapse = "\n")
  )
}

# ------------------------------------------------------------------------------
# Load Script 03 outputs -------------------------------------------------------
# ------------------------------------------------------------------------------
full_audit <- read_csv(
  full_audit_path,
  show_col_types = FALSE
) |>
  clean_names()

sap_observed_slot <- readRDS(
  sap_observed_slot_path
) |>
  clean_names()

sap_reviewed_submissions <- readRDS(
  sap_reviewed_submissions_path
) |>
  clean_names()

slot_resolution <- read_csv(
  slot_resolution_path,
  show_col_types = FALSE
) |>
  clean_names()

sap_merge_checks <- read_csv(
  sap_merge_checks_path,
  show_col_types = FALSE
) |>
  clean_names()

duplicate_reassignment_log <- read_csv_if_exists(
  duplicate_reassignment_log_path
)

final_duplicate_slots <- read_csv_if_exists(
  final_duplicate_slots_path
)

# ------------------------------------------------------------------------------
# Prepare reviewed-submissions sample ------------------------------------------
# ------------------------------------------------------------------------------
sap_reviewed_submissions <- sap_reviewed_submissions |>
  add_missing_cols(c(
    "exclude_from_slot_level_sap",
    "unique_transaction_id",
    "survey_instance_id",
    "confederate_match_key",
    "survey_transaction_id_raw",
    "survey_transaction_id_parsed",
    "submission_datetime",
    "transaction_date",
    "channel_std",
    "amount",
    "delivery_std",
    "transaction_outcome_label",
    "success",
    "kyc_score",
    "match_action",
    "slot_level_resolution_action",
    "slot_level_duplicate_rank"
  )) |>
  mutate(
    exclude_from_slot_level_sap =
      as_logical_safe(exclude_from_slot_level_sap),
    
    submission_datetime =
      ymd_hms(submission_datetime, quiet = TRUE),
    
    transaction_date =
      as.Date(transaction_date),
    
    amount =
      suppressWarnings(as.numeric(amount)),
    
    success =
      suppressWarnings(as.numeric(success)),
    
    kyc_score =
      suppressWarnings(as.numeric(kyc_score))
  )

slot_resolution <- slot_resolution |>
  add_missing_cols(c(
    "unique_transaction_id",
    "survey_instance_id",
    "confederate_match_key",
    "survey_transaction_id_raw",
    "survey_transaction_id_parsed",
    "submission_datetime",
    "transaction_date",
    "channel_std",
    "amount",
    "delivery_std",
    "transaction_outcome_label",
    "success",
    "kyc_score",
    "match_action",
    "slot_level_duplicate_rank",
    "slot_level_resolution_action"
  )) |>
  mutate(
    submission_datetime =
      ymd_hms(submission_datetime, quiet = TRUE),
    
    transaction_date =
      as.Date(transaction_date),
    
    amount =
      suppressWarnings(as.numeric(amount)),
    
    success =
      suppressWarnings(as.numeric(success)),
    
    kyc_score =
      suppressWarnings(as.numeric(kyc_score)),
    
    slot_level_duplicate_rank =
      suppressWarnings(as.integer(slot_level_duplicate_rank))
  )

# ------------------------------------------------------------------------------
# Strict slot-level checks ------------------------------------------------------
# ------------------------------------------------------------------------------
strict_slot_checks <- sap_observed_slot |>
  summarise(
    sample = "strict_slot_level_observed",
    n_rows = n(),
    n_unique_transactions = n_distinct(unique_transaction_id),
    missing_unique_transaction_id = sum(is.na(unique_transaction_id)),
    duplicate_transaction_rows =
      n_rows - n_unique_transactions,
    missing_success = sum(is.na(success)),
    missing_kyc = sum(is.na(kyc_score))
  )

print(strict_slot_checks)

if (strict_slot_checks$missing_unique_transaction_id > 0) {
  stop("Strict slot-level observed sample has missing unique_transaction_id values.")
}

if (strict_slot_checks$duplicate_transaction_rows > 0) {
  stop("Strict slot-level observed sample still has duplicate unique_transaction_id values.")
}

if (strict_slot_checks$missing_success > 0) {
  stop("Strict slot-level observed sample has missing success values.")
}

if (strict_slot_checks$missing_kyc > 0) {
  stop("Strict slot-level observed sample has missing KYC values.")
}

# ------------------------------------------------------------------------------
# Reviewed-submissions preservation checks -------------------------------------
# ------------------------------------------------------------------------------
reviewed_submission_checks <- sap_reviewed_submissions |>
  summarise(
    sample = "reviewed_submissions",
    n_rows = n(),
    n_unique_transactions = n_distinct(unique_transaction_id),
    missing_unique_transaction_id = sum(is.na(unique_transaction_id)),
    duplicate_transaction_rows =
      n_rows - n_unique_transactions,
    excluded_from_slot_level_sap =
      sum(exclude_from_slot_level_sap, na.rm = TRUE)
  )

print(reviewed_submission_checks)

sample_preservation_check <- tibble(
  item = c(
    "strict_slot_level_observed_rows",
    "reviewed_submissions_rows",
    "reviewed_minus_strict_rows",
    "reviewed_duplicate_transaction_rows",
    "reviewed_rows_excluded_from_slot_level_sap"
  ),
  n = c(
    nrow(sap_observed_slot),
    nrow(sap_reviewed_submissions),
    nrow(sap_reviewed_submissions) - nrow(sap_observed_slot),
    reviewed_submission_checks$duplicate_transaction_rows,
    reviewed_submission_checks$excluded_from_slot_level_sap
  )
) |>
  mutate(
    check_note = case_when(
      item == "reviewed_minus_strict_rows" ~
        "Should equal reviewed_rows_excluded_from_slot_level_sap.",
      item == "reviewed_duplicate_transaction_rows" ~
        "Expected in reviewed-submissions because duplicate-slot extras are preserved.",
      TRUE ~ NA_character_
    )
  )

print(sample_preservation_check)

if (
  reviewed_submission_checks$excluded_from_slot_level_sap !=
  nrow(sap_reviewed_submissions) - nrow(sap_observed_slot)
) {
  stop(
    "Mismatch: rows excluded from slot-level SAP do not equal reviewed-minus-strict sample difference."
  )
}

# ------------------------------------------------------------------------------
# Duplicate-resolution summaries -----------------------------------------------
# ------------------------------------------------------------------------------
slot_resolution_summary <- slot_resolution |>
  count(slot_level_resolution_action, name = "n") |>
  arrange(desc(n))

print(slot_resolution_summary)

duplicate_slot_group_summary <- sap_reviewed_submissions |>
  group_by(unique_transaction_id) |>
  summarise(
    n_reviewed_rows_for_slot = n(),
    n_kept_in_slot_level =
      sum(!exclude_from_slot_level_sap, na.rm = TRUE),
    n_excluded_from_slot_level =
      sum(exclude_from_slot_level_sap, na.rm = TRUE),
    
    confederate_match_key =
      first(confederate_match_key),
    
    survey_instances =
      paste(unique(survey_instance_id), collapse = " | "),
    
    raw_transaction_ids =
      paste(unique(na.omit(survey_transaction_id_raw)), collapse = " | "),
    
    channels =
      paste(unique(na.omit(channel_std)), collapse = " | "),
    
    amounts =
      paste(unique(na.omit(as.character(amount))), collapse = " | "),
    
    deliveries =
      paste(unique(na.omit(delivery_std)), collapse = " | "),
    
    outcomes =
      paste(unique(na.omit(transaction_outcome_label)), collapse = " | "),
    
    n_channels =
      n_distinct(channel_std, na.rm = TRUE),
    
    n_amounts =
      n_distinct(amount, na.rm = TRUE),
    
    n_deliveries =
      n_distinct(delivery_std, na.rm = TRUE),
    
    n_outcomes =
      n_distinct(transaction_outcome_label, na.rm = TRUE),
    
    n_success_values =
      n_distinct(success, na.rm = TRUE),
    
    n_kyc_values =
      n_distinct(kyc_score, na.rm = TRUE),
    
    has_channel_conflict =
      n_channels > 1,
    
    has_amount_conflict =
      n_amounts > 1,
    
    has_delivery_conflict =
      n_deliveries > 1,
    
    has_outcome_conflict =
      n_outcomes > 1 | n_success_values > 1,
    
    has_kyc_conflict =
      n_kyc_values > 1,
    
    has_substantive_conflict =
      has_channel_conflict |
      has_amount_conflict |
      has_delivery_conflict |
      has_outcome_conflict,
    
    .groups = "drop"
  ) |>
  filter(n_reviewed_rows_for_slot > 1) |>
  arrange(desc(n_reviewed_rows_for_slot), unique_transaction_id)

print(duplicate_slot_group_summary)

slot_level_excluded_duplicate_rows <- sap_reviewed_submissions |>
  filter(exclude_from_slot_level_sap) |>
  arrange(
    unique_transaction_id,
    submission_datetime
  ) |>
  select(
    any_of(c(
      "unique_transaction_id",
      "survey_instance_id",
      "confederate_match_key",
      "survey_transaction_id_raw",
      "survey_transaction_id_parsed",
      "submission_datetime",
      "transaction_date",
      "channel_std",
      "amount",
      "delivery_std",
      "transaction_outcome_label",
      "success",
      "kyc_score",
      "match_action",
      "slot_level_duplicate_rank",
      "slot_level_resolution_action",
      "duplicate_recovery_status",
      "duplicate_recovery_old_slot",
      "duplicate_recovery_new_slot",
      "duplicate_recovery_note",
      "manual_note"
    ))
  )

slot_level_kept_duplicate_rows <- sap_reviewed_submissions |>
  filter(
    unique_transaction_id %in%
      duplicate_slot_group_summary$unique_transaction_id,
    !exclude_from_slot_level_sap
  ) |>
  arrange(
    unique_transaction_id,
    submission_datetime
  ) |>
  select(
    any_of(c(
      "unique_transaction_id",
      "survey_instance_id",
      "confederate_match_key",
      "survey_transaction_id_raw",
      "survey_transaction_id_parsed",
      "submission_datetime",
      "transaction_date",
      "channel_std",
      "amount",
      "delivery_std",
      "transaction_outcome_label",
      "success",
      "kyc_score",
      "match_action",
      "slot_level_duplicate_rank",
      "slot_level_resolution_action",
      "duplicate_recovery_status",
      "duplicate_recovery_old_slot",
      "duplicate_recovery_new_slot",
      "duplicate_recovery_note",
      "manual_note"
    ))
  )

# ------------------------------------------------------------------------------
# Recovery reassignment diagnostics --------------------------------------------
# ------------------------------------------------------------------------------
if (nrow(duplicate_reassignment_log) > 0) {
  duplicate_reassignment_summary <- duplicate_reassignment_log |>
    summarise(
      n_reassigned_rows = n(),
      n_old_slots = n_distinct(old_slot),
      n_new_slots = n_distinct(new_slot),
      parsed_id_reassignments =
        sum(parsed_id_match, na.rm = TRUE),
      same_treatment_cell_reassignments =
        sum(treatment_cell_match, na.rm = TRUE),
      median_date_distance =
        median(date_distance, na.rm = TRUE),
      median_order_distance =
        median(order_distance, na.rm = TRUE)
    )
} else {
  duplicate_reassignment_summary <- tibble(
    n_reassigned_rows = 0,
    n_old_slots = 0,
    n_new_slots = 0,
    parsed_id_reassignments = 0,
    same_treatment_cell_reassignments = 0,
    median_date_distance = NA_real_,
    median_order_distance = NA_real_
  )
}

print(duplicate_reassignment_summary)

# ------------------------------------------------------------------------------
# Final audit summary -----------------------------------------------------------
# ------------------------------------------------------------------------------
duplicate_resolution_audit_summary <- tibble(
  item = c(
    "script03_full_audit_rows",
    "strict_slot_level_observed_rows",
    "strict_slot_level_unique_transactions",
    "reviewed_submissions_rows",
    "reviewed_submissions_unique_transactions",
    "reviewed_duplicate_extras",
    "rows_excluded_only_from_slot_level_sap",
    "duplicate_slot_groups_in_reviewed_submissions",
    "duplicate_groups_with_substantive_conflict",
    "duplicate_groups_with_kyc_conflict",
    "script03_reassigned_duplicate_rows"
  ),
  n = c(
    nrow(full_audit),
    nrow(sap_observed_slot),
    n_distinct(sap_observed_slot$unique_transaction_id),
    nrow(sap_reviewed_submissions),
    n_distinct(sap_reviewed_submissions$unique_transaction_id),
    nrow(sap_reviewed_submissions) -
      n_distinct(sap_reviewed_submissions$unique_transaction_id),
    sum(sap_reviewed_submissions$exclude_from_slot_level_sap, na.rm = TRUE),
    nrow(duplicate_slot_group_summary),
    sum(duplicate_slot_group_summary$has_substantive_conflict, na.rm = TRUE),
    sum(duplicate_slot_group_summary$has_kyc_conflict, na.rm = TRUE),
    duplicate_reassignment_summary$n_reassigned_rows
  )
)

print(duplicate_resolution_audit_summary, n = Inf)

# ------------------------------------------------------------------------------
# Save outputs -----------------------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(
  strict_slot_checks,
  file.path(output_dir, "IADB_04b_strict_slot_level_checks.csv")
)

write_csv(
  reviewed_submission_checks,
  file.path(output_dir, "IADB_04b_reviewed_submission_checks.csv")
)

write_csv(
  sample_preservation_check,
  file.path(output_dir, "IADB_04b_sample_preservation_check.csv")
)

write_csv(
  slot_resolution_summary,
  file.path(output_dir, "IADB_04b_slot_resolution_summary.csv")
)

write_csv(
  duplicate_slot_group_summary,
  file.path(output_dir, "IADB_04b_duplicate_slot_group_summary.csv")
)

write_csv(
  slot_level_excluded_duplicate_rows,
  file.path(output_dir, "IADB_04b_slot_level_excluded_duplicate_rows.csv")
)

write_csv(
  slot_level_kept_duplicate_rows,
  file.path(output_dir, "IADB_04b_slot_level_kept_duplicate_rows.csv")
)

write_csv(
  duplicate_reassignment_summary,
  file.path(output_dir, "IADB_04b_duplicate_reassignment_summary.csv")
)

write_csv(
  duplicate_resolution_audit_summary,
  file.path(output_dir, "IADB_04b_duplicate_resolution_audit_summary.csv")
)

cat("\nScript 04b completed successfully.\n")
cat("Strict slot-level observed rows: ", nrow(sap_observed_slot), "\n")
cat("Reviewed-submissions rows: ", nrow(sap_reviewed_submissions), "\n")
cat(
  "Rows excluded only from strict slot-level SAP: ",
  sum(sap_reviewed_submissions$exclude_from_slot_level_sap, na.rm = TRUE),
  "\n"
)
