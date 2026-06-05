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
full_audit <- read_csv(
  file.path(output_dir, "IADB_03_surveycto_schedule_matched_full_audit.csv"),
  show_col_types = FALSE
) |>
  clean_names()

sap_base_conservative <- readRDS(
  file.path(output_dir, "IADB_sap_schedule_level_base.rds")
) |>
  clean_names()

sap_observed_conservative <- readRDS(
  file.path(output_dir, "IADB_sap_observed_first_pass.rds")
) |>
  clean_names()

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Safe reading logical
as_logical_safe <- function(x) {
  case_when(
    is.logical(x) ~ x,
    as.character(x) %in% c("TRUE", "true", "1", "Yes", "yes") ~ TRUE,
    as.character(x) %in% c("FALSE", "false", "0", "No", "no") ~ FALSE,
    TRUE ~ FALSE
  )
}

# Mannual revision hierarchy
confidence_rank <- function(x) {
  case_when(
    x == "high" ~ 3,
    x == "medium" ~ 2,
    x == "low_manual_review" ~ 1,
    TRUE ~ 0
  )
}

# ------------------------------------------------------------------------------
# Preparing full audit ---------------------------------------------------------
# ------------------------------------------------------------------------------
audit_prepped <- full_audit |>
  mutate(
    submission_datetime = ymd_hms(submission_datetime, quiet = TRUE),
    
    matched_to_schedule = as_logical_safe(matched_to_schedule),
    
    flag_multiple_rows_same_slot =
      as_logical_safe(flag_multiple_rows_same_slot),
    
    typed_id_conflicts_with_best_match =
      as_logical_safe(typed_id_conflicts_with_best_match),
    
    best_channel_match = as_logical_safe(best_channel_match),
    best_amount_match = as_logical_safe(best_amount_match),
    best_delivery_match = as_logical_safe(best_delivery_match),
    best_country_match = as_logical_safe(best_country_match),
    best_parsed_id_match = as_logical_safe(best_parsed_id_match),
    
    reviewed_by_team_num =
      suppressWarnings(as.numeric(reviewed_by_team_num)),
    
    success = suppressWarnings(as.numeric(success)),
    kyc_score = suppressWarnings(as.numeric(kyc_score)),
    cost_local = suppressWarnings(as.numeric(cost_local)),
    time_hours = suppressWarnings(as.numeric(time_hours)),
    
    confidence_rank = confidence_rank(match_confidence),
    
    treatment_exact_match =
      best_channel_match & best_amount_match & best_delivery_match,
    
    has_success_kyc =
      !is.na(success) & !is.na(kyc_score),
    
    #unique_transaction_id = best_schedule_slot_id
    exclude_from_sap = as_logical_safe(exclude_from_sap),
    
    schedule_slot_id_final = na_if(schedule_slot_id_final, ""),
    unique_transaction_id = na_if(unique_transaction_id, ""),
    
    unique_transaction_id = case_when(
      exclude_from_sap ~ NA_character_,
      !is.na(unique_transaction_id) ~ unique_transaction_id,
      !is.na(schedule_slot_id_final) ~ schedule_slot_id_final,
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------------------------
# Slot-level conflict diagnostics ----------------------------------------------
# ------------------------------------------------------------------------------
slot_conflicts <- audit_prepped |>
  filter(matched_to_schedule, !is.na(unique_transaction_id)) |>
  group_by(unique_transaction_id) |>
  summarise(
    n_rows_for_slot = n(),
    
    n_channels = n_distinct(channel_std, na.rm = TRUE),
    n_amounts = n_distinct(amount, na.rm = TRUE),
    n_deliveries = n_distinct(delivery_std, na.rm = TRUE),
    n_outcomes = n_distinct(transaction_outcome_label, na.rm = TRUE),
    n_success_values = n_distinct(success, na.rm = TRUE),
    n_kyc_values = n_distinct(kyc_score, na.rm = TRUE),
    
    slot_has_channel_conflict = n_channels > 1,
    slot_has_amount_conflict = n_amounts > 1,
    slot_has_delivery_conflict = n_deliveries > 1,
    slot_has_outcome_conflict = n_outcomes > 1 | n_success_values > 1,
    slot_has_kyc_conflict = n_kyc_values > 1,
    
    slot_has_substantive_conflict =
      slot_has_channel_conflict |
      slot_has_amount_conflict |
      slot_has_delivery_conflict |
      slot_has_outcome_conflict,
    
    .groups = "drop"
  )

#write_csv(
#  slot_conflicts,
#  file.path(output_dir, "IADB_04_slot_conflict_diagnostics.csv")
#)

# ------------------------------------------------------------------------------
# Selecting best row per schedule slot -----------------------------------------
# ------------------------------------------------------------------------------
maximal_selected <- audit_prepped |>
  filter(
    !exclude_from_sap,
    matched_to_schedule,
    !is.na(unique_transaction_id)
  ) |>
  left_join(slot_conflicts, by = "unique_transaction_id") |>
  arrange(
    unique_transaction_id,
    
    # Highest priority: manually/team-reviewed rows.
    desc(replace_na(reviewed_by_team_num, 0)),
    
    # Then better automated match quality.
    desc(confidence_rank),
    desc(treatment_exact_match),
    desc(best_parsed_id_match),
    desc(has_success_kyc),
    
    # Then lower date/order distance.
    best_date_distance_assigned,
    best_order_distance,
    
    # Then latest submission.
    desc(submission_datetime)
  ) |>
  group_by(unique_transaction_id) |>
  mutate(
    selected_rank_within_slot = row_number()
  ) |>
  filter(selected_rank_within_slot == 1) |>
  ungroup() |>
  mutate(
    sample_maximal_auto = TRUE,
    
    needs_manual_review_for_final = case_when(
      is.na(confederate_match_key) | confederate_match_key == "" ~ TRUE,
      slot_has_substantive_conflict ~ TRUE,
      match_confidence == "low_manual_review" & !treatment_exact_match ~ TRUE,
      TRUE ~ FALSE
    ),
    
    protocol_deviation = case_when(
      !best_channel_match | !best_amount_match | !best_delivery_match ~ TRUE,
      TRUE ~ FALSE
    ),
    
    treatment_adherent = !protocol_deviation,
    
    match_action_maximal = case_when(
      needs_manual_review_for_final ~ "kept_flagged_for_manual_review",
      protocol_deviation ~ "kept_protocol_deviation",
      TRUE ~ "kept_auto"
    )
  )

# ------------------------------------------------------------------------------
# Creating schedule-level maximal SAP base -------------------------------------
# ------------------------------------------------------------------------------
schedule_only <- sap_base_conservative |>
  select(
    unique_transaction_id,
    confederate_match_key,
    payment_confederate_name_raw,
    payment_confederate_id_raw,
    assigned_transaction_id,
    assigned_order,
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    assigned_date,
    assigned_week,
    phase,
    country_schedule_clean,
    send_by_datetime,
    sent_datetime,
    payment_status_clean,
    payment_ref,
    payment_method,
    payment_notes,
    funds_sent
  )

survey_maximal_for_join <- maximal_selected |>
  select(
    unique_transaction_id,
    survey_instance_id,
    survey_row_id,
    submission_datetime,
    transaction_date,
    transaction_start_datetime,
    scorecard_completed_datetime,
    survey_confederate_id_raw,
    survey_transaction_id_raw,
    survey_transaction_uid_raw,
    survey_transaction_id_parsed,
    survey_transaction_order_parsed,
    
    channel_std,
    amount,
    delivery_std,
    country_clean,
    
    transaction_outcome_label,
    success,
    kyc_score,
    cost_local,
    total_cost_without_time_local,
    time_hours,
    transaction_duration_hours,
    interaction_time_hours,
    
    reviewed_by_team,
    reviewed_by_team_num,
    data_quality_flag,
    
    match_confidence,
    best_match_score,
    match_margin,
    n_candidates,
    
    best_channel_match,
    best_amount_match,
    best_delivery_match,
    best_country_match,
    best_parsed_id_match,
    
    protocol_deviation,
    treatment_adherent,
    needs_manual_review_for_final,
    match_action_maximal,
    
    n_rows_for_slot,
    slot_has_substantive_conflict,
    slot_has_channel_conflict,
    slot_has_amount_conflict,
    slot_has_delivery_conflict,
    slot_has_outcome_conflict,
    slot_has_kyc_conflict
  )

sap_base_maximal_auto <- schedule_only |>
  left_join(
    survey_maximal_for_join,
    by = "unique_transaction_id"
  ) |>
  mutate(
    attempted = !is.na(survey_instance_id),
    
    funds_sent_before_attempt = case_when(
      attempted & !is.na(sent_datetime) & !is.na(submission_datetime) ~
        sent_datetime <= submission_datetime,
      TRUE ~ NA
    ),
    
    execution_status = case_when(
      attempted & funds_sent & funds_sent_before_attempt ~
        "attempted_after_funding",
      attempted & funds_sent & !funds_sent_before_attempt ~
        "attempted_before_recorded_funding",
      attempted & !funds_sent ~
        "attempted_without_recorded_funding",
      !attempted & funds_sent ~
        "funded_not_attempted",
      !attempted & !funds_sent ~
        "not_funded_not_attempted",
      TRUE ~ "unclassified"
    ),
    
    country = coalesce(country_clean, country_schedule_clean),
    
    MTO = as.numeric(assigned_channel == "MTOs"),
    Fintech = as.numeric(assigned_channel == "Fintech"),
    Crypto = as.numeric(assigned_channel == "Crypto"),
    Amount250 = as.numeric(assigned_amount == 250),
    Online = as.numeric(assigned_delivery == "Online"),
    
    observed_MTO = as.numeric(channel_std == "MTOs"),
    observed_Fintech = as.numeric(channel_std == "Fintech"),
    observed_Crypto = as.numeric(channel_std == "Crypto"),
    observed_Amount250 = as.numeric(amount == 250),
    observed_Online = as.numeric(delivery_std == "Online"),
    
    sample_success = attempted & !is.na(success),
    sample_kyc = attempted & !is.na(kyc_score),
    sample_cost_local = attempted & success == 1 & !is.na(cost_local),
    sample_time = attempted & success == 1 & !is.na(time_hours),
    sample_per_protocol = attempted & treatment_adherent,
    sample_needs_manual_review = attempted & needs_manual_review_for_final
  )

sap_observed_maximal_auto <- sap_base_maximal_auto |>
  filter(attempted)

sap_per_protocol_maximal_auto <- sap_base_maximal_auto |>
  filter(attempted, treatment_adherent)

sap_needs_review_maximal_auto <- sap_base_maximal_auto |>
  filter(attempted, needs_manual_review_for_final)

# ------------------------------------------------------------------------------
# Attrition comparison ---------------------------------------------------------
# ------------------------------------------------------------------------------
sample_comparison <- tibble(
  sample = c(
    "cleaned_surveycto_rows",
    "full_audit_matched_to_schedule",
    "maximal_auto_observed",
    "maximal_auto_per_protocol",
    "conservative_observed"
  ),
  n = c(
    nrow(full_audit),
    sum(audit_prepped$matched_to_schedule, na.rm = TRUE),
    nrow(sap_observed_maximal_auto),
    nrow(sap_per_protocol_maximal_auto),
    nrow(sap_observed_conservative)
  ),
  n_unique_transactions = c(
    n_distinct(full_audit$survey_instance_id),
    n_distinct(audit_prepped$unique_transaction_id[audit_prepped$matched_to_schedule], na.rm = TRUE),
    n_distinct(sap_observed_maximal_auto$unique_transaction_id),
    n_distinct(sap_per_protocol_maximal_auto$unique_transaction_id),
    n_distinct(sap_observed_conservative$unique_transaction_id)
  )
)

sample_quality_summary <- sap_observed_maximal_auto |>
  summarise(
    n = n(),
    n_confederates = n_distinct(confederate_match_key),
    n_countries = n_distinct(country),
    missing_success = sum(is.na(success)),
    missing_kyc = sum(is.na(kyc_score)),
    needs_manual_review_for_final = sum(needs_manual_review_for_final, na.rm = TRUE),
    protocol_deviations = sum(protocol_deviation, na.rm = TRUE),
    slots_with_substantive_conflict = sum(slot_has_substantive_conflict, na.rm = TRUE),
    mean_success = mean(success, na.rm = TRUE),
    mean_kyc = mean(kyc_score, na.rm = TRUE)
  )

print(sample_comparison)
print(sample_quality_summary)

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
