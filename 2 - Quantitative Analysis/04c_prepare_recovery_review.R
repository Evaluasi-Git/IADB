# ==============================================================================
# IADB - 04c Prepare Strong Recovery Candidates for Review ----------------------
# Author: Cedric Antunes (Evaluasi)
# Date: May 18, 2026
# Purpose:
#   1. Load top recovery candidates from duplicate-slot recovery audit;
#   2. Keep strong recovery candidates;
#   3. Select the top candidate unused schedule slot for each SurveyCTO row;
#   4. Diagnose whether candidate slots are targeted by multiple rows;
#   5. Export a small manual decision template for recovery review.
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
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------

output_dir <- "D:/Evaluasi/data/clean/sap_dataset_builder"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# Accept either 04a or 04b naming, depending on how the previous script was saved.
top_recovery_candidates_path_04a <- file.path(
  output_dir,
  "IADB_04a_top_recovery_candidates.csv"
)

top_recovery_candidates_path_04b <- file.path(
  output_dir,
  "IADB_04b_top_recovery_candidates.csv"
)

top_recovery_candidates_path <- case_when(
  file.exists(top_recovery_candidates_path_04a) ~ top_recovery_candidates_path_04a,
  file.exists(top_recovery_candidates_path_04b) ~ top_recovery_candidates_path_04b,
  TRUE ~ NA_character_
)

if (is.na(top_recovery_candidates_path)) {
  stop(
    "No top recovery candidate file found. Expected either:\n",
    top_recovery_candidates_path_04a, "\n",
    top_recovery_candidates_path_04b
  )
}

cat("\nReading top recovery candidates from:\n")
cat(top_recovery_candidates_path, "\n")

# ------------------------------------------------------------------------------
# Load top recovery candidates --------------------------------------------------
# ------------------------------------------------------------------------------

top_recovery_candidates <- read_csv(
  top_recovery_candidates_path,
  show_col_types = FALSE
) |>
  clean_names()

# ------------------------------------------------------------------------------
# Basic safety checks -----------------------------------------------------------
# ------------------------------------------------------------------------------

required_cols <- c(
  "survey_instance_id",
  "original_matched_slot",
  "candidate_schedule_slot_id",
  "recovery_score",
  "confederate_match_key",
  "submission_datetime",
  "transaction_date",
  "survey_transaction_id_raw",
  "survey_transaction_id_parsed",
  "survey_channel",
  "survey_amount",
  "survey_delivery",
  "transaction_outcome_label",
  "success",
  "kyc_score",
  "assigned_channel",
  "assigned_amount",
  "assigned_delivery",
  "assigned_transaction_id",
  "assigned_order",
  "assigned_date",
  "sent_datetime",
  "funds_sent",
  "channel_match",
  "amount_match",
  "delivery_match",
  "parsed_id_match",
  "date_distance"
)

missing_cols <- setdiff(required_cols, names(top_recovery_candidates))

if (length(missing_cols) > 0) {
  stop(
    "The top recovery candidates file is missing required columns:\n",
    paste(missing_cols, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# Keep only strong recovery candidates ------------------------------------------
# ------------------------------------------------------------------------------

strong_candidates <- top_recovery_candidates |>
  mutate(
    recovery_score = suppressWarnings(as.numeric(recovery_score)),
    date_distance = suppressWarnings(as.numeric(date_distance))
  ) |>
  filter(recovery_score >= 140) |>
  arrange(
    survey_instance_id,
    desc(recovery_score),
    date_distance
  ) |>
  group_by(survey_instance_id) |>
  mutate(
    candidate_rank = row_number(),
    top_score = first(recovery_score),
    second_score = nth(recovery_score, 2),
    score_margin = top_score - second_score,
    n_strong_candidates_for_row = n()
  ) |>
  ungroup()

# Save all strong candidates, not only the top-ranked candidate.
write_csv(
  strong_candidates,
  file.path(output_dir, "IADB_04c_all_strong_recovery_candidates.csv")
)

# ------------------------------------------------------------------------------
# Candidate slot pressure -------------------------------------------------------
# ------------------------------------------------------------------------------

candidate_slot_pressure <- strong_candidates |>
  filter(candidate_rank == 1) |>
  count(
    candidate_schedule_slot_id,
    name = "n_rows_targeting_slot"
  ) |>
  arrange(desc(n_rows_targeting_slot))

write_csv(
  candidate_slot_pressure,
  file.path(output_dir, "IADB_04c_candidate_slot_pressure.csv")
)

# ------------------------------------------------------------------------------
# Build review file: one top candidate per SurveyCTO row ------------------------
# ------------------------------------------------------------------------------

strong_review <- strong_candidates |>
  filter(candidate_rank == 1) |>
  left_join(
    candidate_slot_pressure,
    by = "candidate_schedule_slot_id"
  ) |>
  mutate(
    safe_to_auto_recover_candidate = case_when(
      n_strong_candidates_for_row == 1 &
        n_rows_targeting_slot == 1 &
        recovery_score >= 140 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    suggested_action = case_when(
      safe_to_auto_recover_candidate ~
        "review_then_recover_to_candidate_slot",
      TRUE ~
        "manual_review_required"
    )
  ) |>
  select(
    survey_instance_id,
    suggested_action,
    safe_to_auto_recover_candidate,
    
    original_matched_slot,
    candidate_schedule_slot_id,
    recovery_score,
    score_margin,
    n_strong_candidates_for_row,
    n_rows_targeting_slot,
    
    confederate_match_key,
    submission_datetime,
    transaction_date,
    
    survey_transaction_id_raw,
    survey_transaction_id_parsed,
    
    survey_channel,
    survey_amount,
    survey_delivery,
    transaction_outcome_label,
    success,
    kyc_score,
    
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    assigned_transaction_id,
    assigned_order,
    assigned_date,
    sent_datetime,
    funds_sent,
    
    channel_match,
    amount_match,
    delivery_match,
    parsed_id_match,
    date_distance
  )

write_csv(
  strong_review,
  file.path(output_dir, "IADB_04c_strong_recovery_candidates_for_review.csv")
)

# ------------------------------------------------------------------------------
# Manual decision template ------------------------------------------------------
# ------------------------------------------------------------------------------

recovery_decisions_template <- strong_review |>
  transmute(
    survey_instance_id,
    suggested_action,
    safe_to_auto_recover_candidate,
    
    original_matched_slot,
    candidate_schedule_slot_id,
    recovery_score,
    score_margin,
    n_strong_candidates_for_row,
    n_rows_targeting_slot,
    
    confederate_match_key,
    
    survey_transaction_id_raw,
    survey_transaction_id_parsed,
    survey_channel,
    survey_amount,
    survey_delivery,
    transaction_outcome_label,
    success,
    kyc_score,
    
    assigned_transaction_id,
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    assigned_order,
    assigned_date,
    
    channel_match,
    amount_match,
    delivery_match,
    parsed_id_match,
    date_distance,
    
    # Fill manually.
    recovery_action = NA_character_,
    
    # Allowed recovery_action values:
    # "recover_to_candidate_slot"
    # "do_not_recover_true_duplicate"
    # "do_not_recover_uncertain"
    # "needs_further_review"
    
    corrected_schedule_slot_id = candidate_schedule_slot_id,
    recovery_note = NA_character_
  )

write_csv(
  recovery_decisions_template,
  file.path(output_dir, "IADB_04c_recovery_decisions_template.csv")
)

# ------------------------------------------------------------------------------
# Summary ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

summary_04c <- tibble(
  item = c(
    "strong_recovery_candidate_rows",
    "unique_survey_rows_with_strong_candidate",
    "safe_to_auto_recover_candidates",
    "candidate_slots_targeted_more_than_once",
    "manual_decision_rows_to_review"
  ),
  n = c(
    nrow(strong_candidates),
    n_distinct(strong_candidates$survey_instance_id),
    sum(strong_review$safe_to_auto_recover_candidate, na.rm = TRUE),
    sum(candidate_slot_pressure$n_rows_targeting_slot > 1, na.rm = TRUE),
    nrow(recovery_decisions_template)
  )
)

write_csv(
  summary_04c,
  file.path(output_dir, "IADB_04c_recovery_review_summary.csv")
)

cat("\n=== 04c Recovery review summary ===\n")
print(summary_04c, n = Inf)

# ------------------------------------------------------------------------------
# Notes for manual review -------------------------------------------------------
# ------------------------------------------------------------------------------
# Fill only two columns in IADB_04c_recovery_decisions_template.csv:
#
#   recovery_action
#   recovery_note
#
# Use:
#
#   recover_to_candidate_slot
#     when the row clearly represents a distinct transaction and the candidate
#     unused schedule slot is credible.
#
#   do_not_recover_true_duplicate
#     when the row appears to be a duplicate/resubmission of the original matched
#     transaction.
#
#   do_not_recover_uncertain
#     when the candidate slot is plausible but evidence is insufficient.
#
#   needs_further_review
#     when receipts, payment logs, or confederate clarification are needed.
#
# Leave corrected_schedule_slot_id as candidate_schedule_slot_id only if choosing:
#
#   recover_to_candidate_slot
# ==============================================================================
