# ==============================================================================
# IADB - 04b Duplicate Slot Recovery Audit -------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: May 18, 2026 -----------------------------------------------------------
# Minimal revisions implemented on June 1st
# Purpose:
#   1. Check the gap between SurveyCTO rows and unique transaction-slot rows;
#   2. Identify true duplicates/resubmissions;
#   3. Identify duplicate-slot groups with substantive conflicts;
#   4. Search for possible unused schedule slots that could recover observations;
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

# Output directoru
output_dir <- "D:/Evaluasi/data/clean/sap_dataset_builder"

# ------------------------------------------------------------------------------
# Loading matched full audit and maximal-auto schedule-level sample ------------
# ------------------------------------------------------------------------------
full_audit <- read_csv(
  file.path(output_dir, "IADB_03_surveycto_schedule_matched_full_audit.csv"),
  show_col_types = FALSE
) |>
  clean_names()

sap_base_maximal <- readRDS(
  file.path(output_dir, "IADB_sap_schedule_level_base_maximal_auto.rds")
) |>
  clean_names()

sap_observed_maximal <- readRDS(
  file.path(output_dir, "IADB_sap_observed_maximal_auto.rds")
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
      df[[cc]] <- NA_character_
    }
  }
  
  df
}

to_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

to_date_safe <- function(x) {
  suppressWarnings(as.Date(x))
}

# ------------------------------------------------------------------------------
# Preparing audit data ---------------------------------------------------------
# ------------------------------------------------------------------------------
needed_audit_cols <- c(
  "matched_to_schedule",
  "submission_datetime",
  "success",
  "kyc_score",
  "amount",
  "assigned_amount",
  "best_assigned_amount",
  "reviewed_by_team_num",
  "best_schedule_slot_id",
  "survey_channel",
  "survey_amount",
  "survey_delivery",
  "channel_std",
  "delivery_std",
  "best_channel_match",
  "best_amount_match",
  "best_delivery_match",
  "best_parsed_id_match",
  "best_assigned_channel",
  "best_assigned_delivery",
  "best_assigned_transaction_id",
  "best_assigned_order",
  "best_assigned_date",
  "transaction_outcome_label",
  "match_confidence",
  "best_date_distance_assigned",
  "best_order_distance",
  "survey_instance_id",
  "confederate_match_key",
  "transaction_date",
  "survey_transaction_id_raw",
  "survey_transaction_id_parsed",
  "exclude_from_sap",
  "schedule_slot_id_final",
  "unique_transaction_id",
  "match_action"
)

audit <- full_audit |>
  add_missing_cols(needed_audit_cols) |>
  mutate(
    matched_to_schedule = as_logical_safe(matched_to_schedule),
    
    submission_datetime = ymd_hms(submission_datetime, quiet = TRUE),
    
    success = to_num(success),
    kyc_score = to_num(kyc_score),
    amount = to_num(amount),
    assigned_amount = to_num(assigned_amount),
    best_assigned_amount = to_num(best_assigned_amount),
    reviewed_by_team_num = to_num(reviewed_by_team_num),
    
    exclude_from_sap = as_logical_safe(exclude_from_sap),
    
    schedule_slot_id_final = na_if(schedule_slot_id_final, ""),
    unique_transaction_id = na_if(unique_transaction_id, ""),
    
    unique_transaction_id = case_when(
      exclude_from_sap ~ NA_character_,
      !is.na(unique_transaction_id) ~ unique_transaction_id,
      !is.na(schedule_slot_id_final) ~ schedule_slot_id_final,
      TRUE ~ NA_character_
    ),
    
    # Full audit usually has channel_std/amount/delivery_std rather than
    # survey_channel/survey_amount/survey_delivery. This makes the script robust.
    survey_channel = coalesce(
      as.character(survey_channel),
      as.character(channel_std)
    ),
    
    survey_amount = coalesce(
      to_num(survey_amount),
      to_num(amount)
    ),
    
    survey_delivery = coalesce(
      as.character(survey_delivery),
      as.character(delivery_std)
    ),
    
    best_channel_match = as_logical_safe(best_channel_match),
    best_amount_match = as_logical_safe(best_amount_match),
    best_delivery_match = as_logical_safe(best_delivery_match),
    best_parsed_id_match = as_logical_safe(best_parsed_id_match),
    
    best_date_distance_assigned_num = to_num(best_date_distance_assigned),
    best_order_distance_num = to_num(best_order_distance)
  )

matched_audit <- audit |>
  filter(
    !exclude_from_sap,
    matched_to_schedule,
    !is.na(unique_transaction_id)
  )

# ------------------------------------------------------------------------------
# Slot-level duplicate classification ------------------------------------------
# ------------------------------------------------------------------------------
duplicate_slot_diagnostics <- matched_audit |>
  group_by(unique_transaction_id) |>
  summarise(
    n_submissions_for_slot = n(),
    
    confederate_match_key = first(confederate_match_key),
    
    assigned_channel = first(best_assigned_channel),
    assigned_amount = first(best_assigned_amount),
    assigned_delivery = first(best_assigned_delivery),
    assigned_transaction_id = first(best_assigned_transaction_id),
    assigned_order = first(best_assigned_order),
    assigned_date = first(best_assigned_date),
    
    n_survey_channels = n_distinct(survey_channel, na.rm = TRUE),
    n_survey_amounts = n_distinct(survey_amount, na.rm = TRUE),
    n_survey_deliveries = n_distinct(survey_delivery, na.rm = TRUE),
    n_outcomes = n_distinct(transaction_outcome_label, na.rm = TRUE),
    n_success_values = n_distinct(success, na.rm = TRUE),
    n_kyc_values = n_distinct(kyc_score, na.rm = TRUE),
    
    survey_channels = paste(sort(unique(na.omit(survey_channel))), collapse = " | "),
    survey_amounts = paste(sort(unique(na.omit(as.character(survey_amount)))), collapse = " | "),
    survey_deliveries = paste(sort(unique(na.omit(survey_delivery))), collapse = " | "),
    outcomes = paste(sort(unique(na.omit(transaction_outcome_label))), collapse = " | "),
    
    first_submission = min(submission_datetime, na.rm = TRUE),
    last_submission = max(submission_datetime, na.rm = TRUE),
    
    any_reviewed = any(reviewed_by_team_num == 1, na.rm = TRUE),
    
    same_channel = n_survey_channels <= 1,
    same_amount = n_survey_amounts <= 1,
    same_delivery = n_survey_deliveries <= 1,
    same_outcome = n_outcomes <= 1 & n_success_values <= 1,
    
    true_duplicate_like =
      n_submissions_for_slot > 1 &
      same_channel &
      same_amount &
      same_delivery &
      same_outcome,
    
    substantive_conflict =
      n_submissions_for_slot > 1 &
      (
        !same_channel |
          !same_amount |
          !same_delivery |
          !same_outcome
      ),
    
    .groups = "drop"
  ) |>
  filter(n_submissions_for_slot > 1) |>
  arrange(desc(substantive_conflict), desc(n_submissions_for_slot))

write_csv(
  duplicate_slot_diagnostics,
  file.path(output_dir, "IADB_04b_duplicate_slot_diagnostics.csv")
)

# ------------------------------------------------------------------------------
# Identifying non-selected rows in duplicate groups ----------------------------
# ------------------------------------------------------------------------------
duplicate_rows_ranked <- matched_audit |>
  semi_join(
    duplicate_slot_diagnostics |> select(unique_transaction_id),
    by = "unique_transaction_id"
  ) |>
  mutate(
    treatment_exact_match =
      best_channel_match & best_amount_match & best_delivery_match,
    
    has_success_kyc =
      !is.na(success) & !is.na(kyc_score),
    
    confidence_rank = case_when(
      match_confidence == "high" ~ 3,
      match_confidence == "medium" ~ 2,
      match_confidence == "low_manual_review" ~ 1,
      TRUE ~ 0
    )
  ) |>
  arrange(
    unique_transaction_id,
    desc(replace_na(reviewed_by_team_num, 0)),
    desc(confidence_rank),
    desc(treatment_exact_match),
    desc(best_parsed_id_match),
    desc(has_success_kyc),
    best_date_distance_assigned_num,
    best_order_distance_num,
    desc(submission_datetime)
  ) |>
  group_by(unique_transaction_id) |>
  mutate(selected_rank_within_slot = row_number()) |>
  ungroup() |>
  left_join(
    duplicate_slot_diagnostics |>
      select(
        unique_transaction_id,
        true_duplicate_like,
        substantive_conflict
      ),
    by = "unique_transaction_id"
  )

nonselected_duplicate_rows <- duplicate_rows_ranked |>
  filter(selected_rank_within_slot > 1)

write_csv(
  duplicate_rows_ranked,
  file.path(output_dir, "IADB_04b_duplicate_rows_ranked.csv")
)

write_csv(
  nonselected_duplicate_rows,
  file.path(output_dir, "IADB_04b_nonselected_duplicate_rows.csv")
)

# ------------------------------------------------------------------------------
# Searching for unused candidate schedule slots for conflicted duplicate rows --
# ------------------------------------------------------------------------------
used_slots <- sap_observed_maximal |>
  distinct(unique_transaction_id) |>
  pull(unique_transaction_id)

unused_schedule_slots <- sap_base_maximal |>
  filter(!unique_transaction_id %in% used_slots) |>
  select(
    candidate_schedule_slot_id = unique_transaction_id,
    confederate_match_key,
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    assigned_transaction_id,
    assigned_order,
    assigned_date,
    sent_datetime,
    funds_sent
  ) |>
  mutate(
    assigned_amount = to_num(assigned_amount),
    assigned_date = to_date_safe(assigned_date)
  )

conflicted_nonselected <- nonselected_duplicate_rows |>
  filter(substantive_conflict)

if (nrow(conflicted_nonselected) > 0) {
  recovery_candidates <- conflicted_nonselected |>
    select(
      survey_instance_id,
      original_matched_slot = unique_transaction_id,
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
      kyc_score
    ) |>
    mutate(
      transaction_date = to_date_safe(transaction_date),
      survey_amount = to_num(survey_amount)
    ) |>
    left_join(
      unused_schedule_slots,
      by = "confederate_match_key",
      relationship = "many-to-many"
    ) |>
    mutate(
      channel_match = survey_channel == assigned_channel,
      amount_match = survey_amount == assigned_amount,
      delivery_match = survey_delivery == assigned_delivery,
      
      date_distance =
        abs(as.numeric(transaction_date - assigned_date)),
      
      parsed_id_match =
        !is.na(survey_transaction_id_parsed) &
        survey_transaction_id_parsed == assigned_transaction_id,
      
      recovery_score =
        100 * as.numeric(replace_na(parsed_id_match, FALSE)) +
        70  * as.numeric(replace_na(channel_match, FALSE)) +
        45  * as.numeric(replace_na(amount_match, FALSE)) +
        40  * as.numeric(replace_na(delivery_match, FALSE)) -
        2   * replace_na(date_distance, 60)
    ) |>
    filter(
      channel_match | amount_match | delivery_match | parsed_id_match
    ) |>
    arrange(
      survey_instance_id,
      desc(recovery_score),
      date_distance
    )
  
  top_recovery_candidates <- recovery_candidates |>
    group_by(survey_instance_id) |>
    arrange(desc(recovery_score), .by_group = TRUE) |>
    slice_head(n = 5) |>
    ungroup()
} else {
  recovery_candidates <- tibble()
  top_recovery_candidates <- tibble()
}

write_csv(
  top_recovery_candidates,
  file.path(output_dir, "IADB_04b_top_recovery_candidates.csv")
)

# ------------------------------------------------------------------------------
# Summaries --------------------------------------------------------------------
# ------------------------------------------------------------------------------
recovery_summary <- tibble(
  item = c(
    "cleaned_surveycto_rows",
    "matched_surveycto_rows",
    "unique_matched_schedule_slots",
    "duplicate_submissions_compressed",
    "duplicate_slots_total",
    "true_duplicate_like_slots",
    "substantive_conflict_slots",
    "nonselected_duplicate_rows",
    "nonselected_true_duplicate_like_rows",
    "nonselected_conflicted_rows",
    "conflicted_rows_with_any_recovery_candidate",
    "conflicted_rows_with_strong_recovery_candidate"
  ),
  n = c(
    nrow(full_audit),
    nrow(matched_audit),
    n_distinct(matched_audit$unique_transaction_id),
    nrow(matched_audit) - n_distinct(matched_audit$unique_transaction_id),
    nrow(duplicate_slot_diagnostics),
    sum(duplicate_slot_diagnostics$true_duplicate_like, na.rm = TRUE),
    sum(duplicate_slot_diagnostics$substantive_conflict, na.rm = TRUE),
    nrow(nonselected_duplicate_rows),
    sum(nonselected_duplicate_rows$true_duplicate_like, na.rm = TRUE),
    sum(nonselected_duplicate_rows$substantive_conflict, na.rm = TRUE),
    n_distinct(top_recovery_candidates$survey_instance_id),
    n_distinct(
      top_recovery_candidates$survey_instance_id[
        top_recovery_candidates$recovery_score >= 140
      ]
    )
  )
)

print(recovery_summary, n = Inf)

write_csv(
  recovery_summary,
  file.path(output_dir, "IADB_04b_recovery_summary.csv")
)
