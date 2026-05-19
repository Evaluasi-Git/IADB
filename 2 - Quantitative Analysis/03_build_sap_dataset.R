# ==============================================================================
# IADB - 03 Build SAP Dataset --------------------------------------------------
# Author: Cedric Antunes (EValuasi) --------------------------------------------
# Date: May 16, 2026 -----------------------------------------------------------
# Purpose:
#   1. Load cleaned SurveyCTO data;
#   2. Load enriched randomized/payment schedule;
#   3. Match observed SurveyCTO attempts to randomized schedule slots;
#   4. Create final unique_transaction_id = schedule_slot_id;
#   5. Diagnose unmatched rows, duplicates, protocol deviations, and bad completions;
#   6. Build schedule-level SAP denominator and observed-attempt SAP sample.
#
# Inputs:
#   data/clean/IADB_surveycto_clean_may16.csv
#   data/clean/sap_dataset_builder/IADB_payment_schedule_enriched_with_randomization.csv
#
# Main outputs:
#   IADB_sap_schedule_level_base.csv/.rds
#   IADB_sap_observed_first_pass.csv/.rds
#   IADB_manual_schedule_match_review_to_complete.csv
#   IADB_bad_survey_completion_review.csv
# ==============================================================================

# Cleaning my environment 
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(readr)
  library(stringr)
  library(stringi)
  library(here)
})

# ------------------------------------------------------------------------------
# Paramenters ------------------------------------------------------------------
# ------------------------------------------------------------------------------
# For replication, tweak as needed!
survey_path <- here(
  "data",
  "clean",
  "IADB_surveycto_clean_may16.csv"
)

payment_schedule_path <- here(
  "data",
  "clean",
  "sap_dataset_builder",
  "IADB_payment_schedule_enriched_with_randomization.csv"
)

output_dir <- here(
  "data",
  "clean",
  "sap_dataset_builder"
)

# Create output directory 
dir.create(output_dir, 
           showWarnings = FALSE, 
           recursive = TRUE)

# First-pass mode:
#   TRUE  = accept high/medium matches automatically and exclude ambiguous rows.
#   FALSE = require completed manual review file before final SAP dataset.
SKIP_MANUAL_REVIEW <- FALSE

# For first pass
# For a stricter first pass, use c("high").
AUTO_ACCEPT_LEVELS <- c("high", "medium")

# Matching tolerances.
MAX_ASSIGNED_DATE_DISTANCE_DAYS <- 60
MAX_SENT_DATE_DISTANCE_DAYS <- 60

# Manual review files.
manual_review_to_complete_path <- file.path(
  output_dir,
  "IADB_manual_schedule_match_review_to_complete.csv"
)

manual_review_completed_path <- file.path(
  output_dir,
  "IADB_manual_schedule_match_review_completed.csv"
)

# Confederate crosswalk.
# If SurveyCTO confederate IDs and schedule confederate names do not match.
crosswalk_path <- here(
  "data",
  "manual",
  "IADB_confederate_crosswalk.csv"
)

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Enconding
normalize_key <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_squish() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "")
}

# Harmonization
standardize_confederate_id <- function(x) {
  normalize_key(x)
}

# Harmonization
standardize_channel <- function(x) {
  x_clean <- normalize_key(x)
  
  case_when(
    x_clean %in% c("bank", "banks", "conventional_bank") ~ "Banks",
    x_clean %in% c("mts", "mto", "mtos", "money_transfer", "western_union") ~ "MTOs",
    x_clean %in% c("fintech", "paypal", "wise") ~ "Fintech",
    x_clean %in% c("crypto", "coinbase", "binance") ~ "Crypto",
    is.na(x_clean) | x_clean == "" ~ NA_character_,
    TRUE ~ as.character(x)
  )
}

# Harmonization
standardize_delivery <- function(x) {
  x_clean <- normalize_key(x)
  
  case_when(
    x_clean %in% c("online", "1") ~ "Online",
    x_clean %in% c("in_person", "inperson", "0") ~ "In-person",
    is.na(x_clean) | x_clean == "" ~ NA_character_,
    TRUE ~ as.character(x)
  )
}

# Harmonization
standardize_country <- function(x) {
  x_clean <- normalize_key(x)
  
  case_when(
    x_clean %in% c("brazil", "brasil") ~ "brazil",
    x_clean == "argentina" ~ "argentina",
    x_clean == "peru" ~ "peru",
    x_clean == "mexico" ~ "mexico",
    x_clean == "colombia" ~ "colombia",
    x_clean == "guatemala" ~ "guatemala",
    x_clean %in% c("el_salvador", "elsalvador") ~ "el_salvador",
    x_clean == "jamaica" ~ "jamaica",
    x_clean == "chile" ~ "chile",
    x_clean == "ecuador" ~ "ecuador",
    x_clean == "nicaragua" ~ "nicaragua",
    x_clean %in% c("costa_rica", "costarica") ~ "costa_rica",
    x_clean == "panama" ~ "panama",
    TRUE ~ x_clean
  )
}

# Harmonization
parse_datetime_flexible <- function(x) {
  parse_date_time(
    as.character(x),
    orders = c(
      "ymd HMS", "ymd HM", "ymd",
      "mdy HMS", "mdy HM", "mdy",
      "dmy HMS", "dmy HM", "dmy",
      "Ymd HMS", "Ymd HM",
      "dmY HMS", "dmY HM"
    ),
    quiet = TRUE
  )
}

parse_date_flexible <- function(x) {
  as.Date(parse_datetime_flexible(x))
}

parse_number_safe <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

# Yes/No safe
to_yesno_num <- function(x) {
  x_clean <- normalize_key(x)
  
  case_when(
    x_clean %in% c("1", "yes", "true", "sim", "si") ~ 1,
    x_clean %in% c("0", "no", "false") ~ 0,
    TRUE ~ NA_real_
  )
}

# Safe logical
to_logical_safe <- function(x) {
  x_clean <- normalize_key(x)
  
  case_when(
    x_clean %in% c("true", "t", "1", "yes", "sim", "si") ~ TRUE,
    x_clean %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ NA
  )
}

# Text harmonization
extract_assigned_txid <- function(x) {
  purrr::map_chr(x, function(z) {
    z_raw <- as.character(z)
    z_clean <- normalize_key(z_raw)
    
    if (is.na(z_clean) || z_clean == "") {
      return(NA_character_)
    }
    
    tx_n <- NA_integer_
    
    # Standard T formats: T01, T001, T0001, T0012, T12.
    if (str_detect(z_clean, "^t_?0*\\d{1,4}$")) {
      tx_n <- as.integer(str_extract(z_clean, "\\d+"))
    }
    
    # E formats: E001, E0001.
    if (is.na(tx_n) && str_detect(z_clean, "^e_?0*\\d{1,4}$")) {
      tx_n <- as.integer(str_extract(z_clean, "\\d+"))
    }
    
    # ELIA formats: ELIA 006, elia006.
    if (is.na(tx_n) && str_detect(z_clean, "^elia_?0*\\d{1,4}$")) {
      tx_n <- as.integer(str_extract(z_clean, "\\d+"))
    }
    
    # transaction[12], [2026-03-26].
    raw_lower <- str_to_lower(str_squish(z_raw))
    
    if (is.na(tx_n) && str_detect(raw_lower, "^transaction\\s*\\[\\s*\\d{1,2}\\s*\\]")) {
      m <- str_match(raw_lower, "^transaction\\s*\\[\\s*(\\d{1,2})\\s*\\]")
      tx_n <- as.integer(m[1, 2])
    }
    
    # Transaction 8, 18/03/2026.
    # Does not parse "Transaction, 08/03/2026" as transaction 8.
    if (is.na(tx_n) && str_detect(raw_lower, "^transaction\\s+\\d{1,2}\\s*,")) {
      m <- str_match(raw_lower, "^transaction\\s+(\\d{1,2})\\s*,")
      tx_n <- as.integer(m[1, 2])
    }
    
    if (!is.na(tx_n) && tx_n >= 1 && tx_n <= 40) {
      return(sprintf("T%03d", tx_n))
    }
    
    NA_character_
  })
}

txid_to_order <- function(x) {
  parse_number_safe(x)
}

logical_score <- function(x) {
  as.numeric(replace_na(x, FALSE))
}

add_missing_cols <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  
  if (length(missing_cols) > 0) {
    data[missing_cols] <- NA_character_
  }
  
  data
}

# ------------------------------------------------------------------------------
# Loading cleaned SurveyCTO data -----------------------------------------------
# ------------------------------------------------------------------------------
survey_raw <- read_csv(
  survey_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  clean_names()

expected_survey_cols <- c(
  "instance_id", "key",
  "submission_datetime",
  "transaction_date",
  "transaction_start_datetime",
  "scorecard_completed_datetime",
  "transaction_id",
  "transaction_uid",
  "confederate_id",
  "country_clean",
  "channel",
  "delivery",
  "amount",
  "transaction_outcome_label",
  "success",
  "kyc_score",
  "cost_local",
  "total_cost_without_time_local",
  "time_hours",
  "transaction_duration_hours",
  "interaction_time_hours",
  "reviewed_by_team",
  "data_quality_flag",
  "j_comments",
  "k1_field_notes",
  "k2_red_flags",
  "k3_strengths",
  "k4_questions_uncertainties",
  "k5_overall_impressions"
)

survey_raw <- survey_raw |>
  add_missing_cols(expected_survey_cols)

survey <- survey_raw |>
  mutate(
    survey_row_id = row_number(),
    
    survey_instance_id = case_when(
      !is.na(instance_id) & instance_id != "" ~ instance_id,
      !is.na(key) & key != "" ~ key,
      TRUE ~ paste0("survey_row_", survey_row_id)
    ),
    
    survey_confederate_id_raw = confederate_id,
    confederate_id_key = standardize_confederate_id(confederate_id),
    
    survey_transaction_id_raw = transaction_id,
    survey_transaction_uid_raw = transaction_uid,
    
    survey_transaction_id_parsed =
      extract_assigned_txid(survey_transaction_id_raw),
    
    survey_transaction_order_parsed =
      txid_to_order(survey_transaction_id_parsed),
    
    submission_datetime =
      parse_datetime_flexible(submission_datetime),
    
    transaction_date =
      parse_date_flexible(transaction_date),
    
    transaction_start_datetime =
      parse_datetime_flexible(transaction_start_datetime),
    
    scorecard_completed_datetime =
      parse_datetime_flexible(scorecard_completed_datetime),
    
    country_clean = standardize_country(country_clean),
    channel_std = standardize_channel(channel),
    delivery_std = standardize_delivery(delivery),
    
    amount = parse_number_safe(amount),
    success = parse_number_safe(success),
    kyc_score = parse_number_safe(kyc_score),
    cost_local = parse_number_safe(cost_local),
    total_cost_without_time_local =
      parse_number_safe(total_cost_without_time_local),
    time_hours = parse_number_safe(time_hours),
    transaction_duration_hours =
      parse_number_safe(transaction_duration_hours),
    interaction_time_hours =
      parse_number_safe(interaction_time_hours),
    
    reviewed_by_team_num = to_yesno_num(reviewed_by_team)
  )

# ------------------------------------------------------------------------------
# [Robustnes] confederate crosswalk --------------------------------------------
# ------------------------------------------------------------------------------
# Built-in crosswalk for known SurveyCTO/schedule naming discrepancies.
# Left side = SurveyCTO cleaned confederate_id_key.
# Right side = enriched schedule confederate_match_key.

default_crosswalk <- tibble::tribble(
  ~survey_confederate_id_key,      ~schedule_confederate_id_key,
  
  # Colombia / Laura
  "laura_tabares_pena",            "laura_isabel_tabares_pena",
  
  # Colombia / Norma
  "norma_diaz",                    "norma_c_diaz_medina",
  
  # Colombia / Maria
  "maria_florez",                  "maria_carolina_rojas_florez",
  
  # Mexico / Elia
  "elia_sauri",                    "elia_s",
  
  # Brazil / Adriana
  "adriana_jannotti",              "adriana_souza",
  
  # Possible variants, harmless if absent
  "adriana_jannoti",               "adriana_souza",
  "maria_carolina_florez",         "maria_carolina_rojas_florez",
  "maria_rojas_florez",            "maria_carolina_rojas_florez",
  "norma_c_diaz",                  "norma_c_diaz_medina"
)

if (file.exists(crosswalk_path)) {
  external_crosswalk <- read_csv(
    crosswalk_path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) |>
    clean_names() |>
    transmute(
      survey_confederate_id_key =
        standardize_confederate_id(survey_confederate_id_key),
      schedule_confederate_id_key =
        standardize_confederate_id(schedule_confederate_id_key)
    )
  
  confederate_crosswalk <- bind_rows(
    default_crosswalk,
    external_crosswalk
  ) |>
    distinct(survey_confederate_id_key, .keep_all = TRUE)
} else {
  confederate_crosswalk <- default_crosswalk
}

survey <- survey |>
  left_join(
    confederate_crosswalk,
    by = c("confederate_id_key" = "survey_confederate_id_key")
  ) |>
  mutate(
    confederate_match_key = coalesce(
      schedule_confederate_id_key,
      confederate_id_key
    )
  ) |>
  select(-schedule_confederate_id_key)

# ------------------------------------------------------------------------------
# Loading enriched payment/randomization schedule ------------------------------
# ------------------------------------------------------------------------------
payment_schedule_raw <- read_csv(
  payment_schedule_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  clean_names()

expected_schedule_cols <- c(
  "schedule_slot_id",
  "unique_transaction_id",
  "confederate_match_key",
  "payment_confederate_name_raw",
  "payment_confederate_id_raw",
  "country_payment_clean",
  "country_schedule_clean",
  "assigned_channel",
  "assigned_amount",
  "assigned_delivery",
  "transaction_order",
  "assigned_transaction_id",
  "payment_approximate_date",
  "payment_assigned_week",
  "payment_phase",
  "randomized_approximate_date",
  "randomized_assigned_week",
  "randomized_phase",
  "payment_send_by_datetime",
  "payment_sent_datetime",
  "payment_status_clean",
  "payment_ref",
  "payment_method",
  "payment_notes",
  "funds_sent",
  "do_not_send_money",
  "schedule_row_status",
  "randomization_match_confidence"
)

payment_schedule_raw <- payment_schedule_raw |>
  add_missing_cols(expected_schedule_cols)

payment_schedule <- payment_schedule_raw |>
  mutate(
    confederate_match_key =
      standardize_confederate_id(confederate_match_key),
    
    schedule_slot_id = schedule_slot_id,
    unique_transaction_id = schedule_slot_id,
    
    assigned_channel = standardize_channel(assigned_channel),
    assigned_amount = parse_number_safe(assigned_amount),
    assigned_delivery = standardize_delivery(assigned_delivery),
    
    assigned_order = as.integer(parse_number_safe(transaction_order)),
    assigned_transaction_id = assigned_transaction_id,
    
    country_schedule_clean = standardize_country(
      coalesce(country_schedule_clean, country_payment_clean)
    ),
    
    assigned_date = parse_date_flexible(
      coalesce(payment_approximate_date, randomized_approximate_date)
    ),
    
    assigned_week = as.integer(parse_number_safe(
      coalesce(payment_assigned_week, randomized_assigned_week)
    )),
    
    phase = as.integer(parse_number_safe(
      coalesce(payment_phase, randomized_phase)
    )),
    
    send_by_datetime =
      parse_datetime_flexible(payment_send_by_datetime),
    
    sent_datetime =
      parse_datetime_flexible(payment_sent_datetime),
    
    payment_status_clean =
      str_to_upper(str_squish(payment_status_clean)),
    
    funds_sent = case_when(
      !is.na(to_logical_safe(funds_sent)) ~ to_logical_safe(funds_sent),
      payment_status_clean %in% c("SENT", "PAID", "TRANSFERRED", "COMPLETED") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    do_not_send_money = case_when(
      !is.na(to_logical_safe(do_not_send_money)) ~ to_logical_safe(do_not_send_money),
      schedule_row_status == "operational_do_not_send" ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  filter(
    !do_not_send_money,
    !is.na(schedule_slot_id),
    !is.na(assigned_channel),
    !is.na(assigned_amount),
    !is.na(assigned_delivery)
  )

# ------------------------------------------------------------------------------
# Schedule diagnostics ---------------------------------------------------------
# ------------------------------------------------------------------------------
schedule_checks <- payment_schedule |>
  summarise(
    n_schedule_slots = n(),
    n_unique_schedule_slots = n_distinct(schedule_slot_id),
    n_confederates = n_distinct(confederate_match_key),
    missing_schedule_slot_id = sum(is.na(schedule_slot_id)),
    missing_assigned_channel = sum(is.na(assigned_channel)),
    missing_assigned_amount = sum(is.na(assigned_amount)),
    missing_assigned_delivery = sum(is.na(assigned_delivery)),
    missing_assigned_order = sum(is.na(assigned_order)),
    n_funds_sent = sum(funds_sent, na.rm = TRUE),
    n_funds_not_sent = sum(!funds_sent, na.rm = TRUE)
  )

cat("\n=== Schedule checks ===\n")
print(schedule_checks)

write_csv(
  schedule_checks,
  file.path(output_dir, "IADB_03_schedule_checks.csv")
)

stopifnot(schedule_checks$n_schedule_slots == schedule_checks$n_unique_schedule_slots)
stopifnot(schedule_checks$missing_schedule_slot_id == 0)
stopifnot(schedule_checks$missing_assigned_channel == 0)
stopifnot(schedule_checks$missing_assigned_amount == 0)
stopifnot(schedule_checks$missing_assigned_delivery == 0)

# ------------------------------------------------------------------------------
# Confederate coverage diagnostics ---------------------------------------------
# ------------------------------------------------------------------------------
confederate_match_diagnostics <- survey |>
  distinct(confederate_match_key) |>
  mutate(in_survey = TRUE) |>
  full_join(
    payment_schedule |>
      distinct(confederate_match_key) |>
      mutate(in_schedule = TRUE),
    by = "confederate_match_key"
  ) |>
  mutate(
    in_survey = replace_na(in_survey, FALSE),
    in_schedule = replace_na(in_schedule, FALSE)
  ) |>
  arrange(in_schedule, in_survey, confederate_match_key)

cat("\n=== Confederate match diagnostics ===\n")
print(confederate_match_diagnostics, n = Inf)

# ------------------------------------------------------------------------------
# Candidate matching: SurveyCTO attempts to schedule slots ---------------------
# ------------------------------------------------------------------------------
candidate_matches <- survey |>
  select(
    survey_instance_id,
    survey_row_id,
    submission_datetime,
    transaction_date,
    transaction_start_datetime,
    scorecard_completed_datetime,
    confederate_match_key,
    confederate_id_key,
    survey_confederate_id_raw,
    survey_transaction_id_raw,
    survey_transaction_uid_raw,
    survey_transaction_id_parsed,
    survey_transaction_order_parsed,
    country_clean,
    channel_std,
    amount,
    delivery_std,
    transaction_outcome_label,
    success,
    kyc_score,
    cost_local,
    time_hours,
    transaction_duration_hours,
    reviewed_by_team,
    reviewed_by_team_num,
    data_quality_flag
  ) |>
  left_join(
    payment_schedule,
    by = "confederate_match_key",
    relationship = "many-to-many"
  ) |>
  mutate(
    date_distance_assigned =
      abs(as.numeric(transaction_date - assigned_date)),
    
    date_distance_sent =
      abs(as.numeric(as.Date(submission_datetime) - as.Date(sent_datetime))),
    
    amount_match = amount == assigned_amount,
    delivery_match = delivery_std == assigned_delivery,
    channel_match = channel_std == assigned_channel,
    country_match = country_clean == country_schedule_clean,
    
    funds_sent_before_submission =
      funds_sent &
      !is.na(sent_datetime) &
      !is.na(submission_datetime) &
      sent_datetime <= submission_datetime,
    
    sent_to_submission_days =
      as.numeric(difftime(submission_datetime, sent_datetime, units = "days")),
    
    parsed_id_match = case_when(
      !is.na(survey_transaction_id_parsed) &
        survey_transaction_id_parsed == assigned_transaction_id ~ TRUE,
      TRUE ~ FALSE
    ),
    
    order_distance = abs(
      survey_transaction_order_parsed - assigned_order
    ),
    
    plausible_time_or_id = case_when(
      parsed_id_match ~ TRUE,
      !is.na(date_distance_assigned) &
        date_distance_assigned <= MAX_ASSIGNED_DATE_DISTANCE_DAYS ~ TRUE,
      !is.na(date_distance_sent) &
        date_distance_sent <= MAX_SENT_DATE_DISTANCE_DAYS ~ TRUE,
      TRUE ~ FALSE
    ),
    
    candidate_allowed = case_when(
      parsed_id_match ~ TRUE,
      
      amount_match &
        delivery_match &
        plausible_time_or_id ~ TRUE,
      
      channel_match &
        amount_match &
        plausible_time_or_id ~ TRUE,
      
      channel_match &
        delivery_match &
        plausible_time_or_id ~ TRUE,
      
      TRUE ~ FALSE
    ),
    
    match_score =
      120 * logical_score(parsed_id_match) +
      70  * logical_score(channel_match) +
      45  * logical_score(amount_match) +
      40  * logical_score(delivery_match) +
      10  * logical_score(country_match) +
      20  * logical_score(funds_sent_before_submission) -
      1.5 * replace_na(date_distance_assigned, 45) -
      0.5 * abs(replace_na(sent_to_submission_days, 45)) -
      4   * replace_na(order_distance, 0)
  ) |>
  filter(candidate_allowed) |>
  arrange(
    survey_instance_id,
    desc(match_score),
    date_distance_assigned,
    date_distance_sent
  )

candidate_top <- candidate_matches |>
  group_by(survey_instance_id) |>
  arrange(desc(match_score), .by_group = TRUE) |>
  mutate(candidate_rank = row_number()) |>
  filter(candidate_rank <= 5) |>
  ungroup()

# ------------------------------------------------------------------------------
# Best match and confidence ----------------------------------------------------
# ------------------------------------------------------------------------------
best_matches <- candidate_top |>
  group_by(survey_instance_id) |>
  arrange(desc(match_score), .by_group = TRUE) |>
  summarise(
    best_schedule_slot_id = first(schedule_slot_id),
    best_assigned_transaction_id = first(assigned_transaction_id),
    best_assigned_order = first(assigned_order),
    best_assigned_channel = first(assigned_channel),
    best_assigned_amount = first(assigned_amount),
    best_assigned_delivery = first(assigned_delivery),
    best_assigned_date = first(assigned_date),
    best_sent_datetime = first(sent_datetime),
    best_funds_sent = first(funds_sent),
    best_funds_sent_before_submission = first(funds_sent_before_submission),
    
    best_channel_match = first(channel_match),
    best_amount_match = first(amount_match),
    best_delivery_match = first(delivery_match),
    best_country_match = first(country_match),
    best_parsed_id_match = first(parsed_id_match),
    best_date_distance_assigned = first(date_distance_assigned),
    best_date_distance_sent = first(date_distance_sent),
    best_order_distance = first(order_distance),
    
    best_match_score = first(match_score),
    second_match_score = nth(match_score, 2),
    match_margin = best_match_score - second_match_score,
    n_candidates = n(),
    
    .groups = "drop"
  ) |>
  mutate(
    match_confidence = case_when(
      best_parsed_id_match &
        best_channel_match &
        best_amount_match &
        best_delivery_match ~ "high",
      
      n_candidates == 1 &
        best_match_score >= 120 ~ "high",
      
      !is.na(match_margin) &
        match_margin >= 35 &
        best_match_score >= 120 ~ "high",
      
      n_candidates == 1 &
        best_match_score >= 85 ~ "medium",
      
      !is.na(match_margin) &
        match_margin >= 20 &
        best_match_score >= 85 ~ "medium",
      
      TRUE ~ "low_manual_review"
    )
  )

write_csv(
  best_matches,
  file.path(output_dir, "IADB_03_best_schedule_matches.csv")
)

# ------------------------------------------------------------------------------
# Manual review file -----------------------------------------------------------
# ------------------------------------------------------------------------------
survey_with_matches <- survey |>
  left_join(best_matches, by = "survey_instance_id") |>
  mutate(
    matched_to_schedule = !is.na(best_schedule_slot_id),
    
    typed_id_conflicts_with_best_match = case_when(
      !is.na(survey_transaction_id_parsed) &
        !is.na(best_assigned_transaction_id) &
        survey_transaction_id_parsed != best_assigned_transaction_id ~ TRUE,
      TRUE ~ FALSE
    ),
    
    observed_vs_assigned_channel_mismatch = case_when(
      matched_to_schedule & !is.na(best_channel_match) ~ !best_channel_match,
      TRUE ~ FALSE
    ),
    
    observed_vs_assigned_amount_mismatch = case_when(
      matched_to_schedule & !is.na(best_amount_match) ~ !best_amount_match,
      TRUE ~ FALSE
    ),
    
    observed_vs_assigned_delivery_mismatch = case_when(
      matched_to_schedule & !is.na(best_delivery_match) ~ !best_delivery_match,
      TRUE ~ FALSE
    )
  )

slot_match_counts <- survey_with_matches |>
  filter(!is.na(best_schedule_slot_id)) |>
  count(
    best_schedule_slot_id,
    name = "n_survey_rows_matched_to_slot"
  )

survey_with_matches <- survey_with_matches |>
  left_join(slot_match_counts, by = "best_schedule_slot_id") |>
  mutate(
    n_survey_rows_matched_to_slot =
      replace_na(n_survey_rows_matched_to_slot, 0),
    flag_multiple_rows_same_slot =
      n_survey_rows_matched_to_slot > 1
  )

manual_match_review <- survey_with_matches |>
  filter(
    !matched_to_schedule |
      match_confidence == "low_manual_review" |
      typed_id_conflicts_with_best_match |
      flag_multiple_rows_same_slot |
      observed_vs_assigned_amount_mismatch |
      observed_vs_assigned_delivery_mismatch |
      is.na(confederate_match_key) |
      confederate_match_key == ""
  ) |>
  arrange(confederate_match_key, survey_transaction_id_raw, submission_datetime) |>
  transmute(
    survey_instance_id,
    submission_datetime,
    transaction_date,
    transaction_start_datetime,
    scorecard_completed_datetime,
    
    confederate_match_key,
    survey_confederate_id_raw,
    survey_transaction_id_raw,
    survey_transaction_id_parsed,
    survey_transaction_uid_raw,
    
    survey_channel = channel_std,
    survey_amount = amount,
    survey_delivery = delivery_std,
    survey_country = country_clean,
    survey_outcome = transaction_outcome_label,
    success,
    kyc_score,
    reviewed_by_team,
    reviewed_by_team_num,
    data_quality_flag,
    
    best_schedule_slot_id,
    best_assigned_transaction_id,
    best_assigned_order,
    best_assigned_channel,
    best_assigned_amount,
    best_assigned_delivery,
    best_assigned_date,
    best_sent_datetime,
    best_funds_sent,
    best_funds_sent_before_submission,
    
    best_channel_match,
    best_amount_match,
    best_delivery_match,
    best_country_match,
    best_parsed_id_match,
    best_date_distance_assigned,
    best_date_distance_sent,
    best_order_distance,
    best_match_score,
    second_match_score,
    match_margin,
    n_candidates,
    match_confidence,
    
    typed_id_conflicts_with_best_match,
    flag_multiple_rows_same_slot,
    n_survey_rows_matched_to_slot,
    observed_vs_assigned_channel_mismatch,
    observed_vs_assigned_amount_mismatch,
    observed_vs_assigned_delivery_mismatch,
    
    # Manual fields to complete if running final SAP.
    match_action = NA_character_,
    
    # Allowed actions:
    # "accept_best_match"
    # "assign_different_schedule_slot"
    # "drop_true_duplicate"
    # "exclude_unresolved"
    # "keep_observed_unassigned"
    
    corrected_schedule_slot_id = NA_character_,
    source_of_decision = NA_character_,
    manual_note = NA_character_
  )

write_csv(
  manual_match_review,
  manual_review_to_complete_path
)

# Storing the set of rows that require manual review
manual_review_ids <- manual_match_review |>
  distinct(survey_instance_id) |>
  mutate(requires_manual_review = TRUE)

# ------------------------------------------------------------------------------
# Applying manual review, or first-pass automated decision ---------------------
# ------------------------------------------------------------------------------
if (!SKIP_MANUAL_REVIEW && !file.exists(manual_review_completed_path)) {
  stop(
    "Manual review is required but file not found:\n",
    manual_review_completed_path,
    "\nComplete IADB_manual_schedule_match_review_to_complete.csv and save it as IADB_manual_schedule_match_review_completed.csv, or set SKIP_MANUAL_REVIEW = TRUE for a first pass."
  )
}

if (file.exists(manual_review_completed_path)) {
  manual_match_completed <- read_csv(
    manual_review_completed_path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) |>
    clean_names() |>
    transmute(
      survey_instance_id,
      match_action = str_squish(match_action),
      corrected_schedule_slot_id = str_squish(corrected_schedule_slot_id),
      source_of_decision,
      manual_note
    )
} else {
  manual_match_completed <- tibble(
    survey_instance_id = character(),
    match_action = character(),
    corrected_schedule_slot_id = character(),
    source_of_decision = character(),
    manual_note = character()
  )
}

# If running final/manual mode, every row requiring review must have a decision.
if (!SKIP_MANUAL_REVIEW) {
  incomplete_manual_review <- manual_review_ids |>
    left_join(manual_match_completed, by = "survey_instance_id") |>
    filter(is.na(match_action) | match_action == "")
  
  if (nrow(incomplete_manual_review) > 0) {
    write_csv(
      incomplete_manual_review,
      file.path(output_dir, "IADB_03_incomplete_manual_review_rows.csv")
    )
    
    stop(
      "Manual review file exists, but some rows requiring review have no match_action. ",
      "Review IADB_03_incomplete_manual_review_rows.csv."
    )
  }
}

survey_matched <- survey_with_matches |>
  left_join(manual_review_ids, by = "survey_instance_id") |>
  mutate(
    requires_manual_review = replace_na(requires_manual_review, FALSE)
  ) |>
  left_join(manual_match_completed, by = "survey_instance_id") |>
  mutate(
    auto_accept =
      matched_to_schedule &
      match_confidence %in% AUTO_ACCEPT_LEVELS,
    
    match_action = case_when(
      !is.na(match_action) & match_action != "" ~ match_action,
      
      SKIP_MANUAL_REVIEW & auto_accept ~
        "accept_best_match",
      
      SKIP_MANUAL_REVIEW & !auto_accept ~
        "exclude_unresolved",
      
      !SKIP_MANUAL_REVIEW & !requires_manual_review & matched_to_schedule ~
        "accept_best_match",
      
      TRUE ~ "exclude_unresolved"
    ),
    
    schedule_slot_id_final = case_when(
      match_action == "assign_different_schedule_slot" &
        !is.na(corrected_schedule_slot_id) &
        corrected_schedule_slot_id != "" ~ corrected_schedule_slot_id,
      
      match_action == "accept_best_match" &
        !is.na(best_schedule_slot_id) ~ best_schedule_slot_id,
      
      TRUE ~ NA_character_
    ),
    
    exclude_from_sap = match_action %in% c(
      "drop_true_duplicate",
      "exclude_unresolved",
      "keep_observed_unassigned"
    ),
    
    observed_unassigned = match_action == "keep_observed_unassigned",
    
    unique_transaction_id = schedule_slot_id_final
  )

# Validate corrected manual slot IDs if any.
bad_manual_slots <- survey_matched |>
  filter(
    match_action == "assign_different_schedule_slot",
    !is.na(corrected_schedule_slot_id),
    !(corrected_schedule_slot_id %in% payment_schedule$schedule_slot_id)
  )

if (nrow(bad_manual_slots) > 0) {
  write_csv(
    bad_manual_slots,
    file.path(output_dir, "IADB_03_bad_manual_slot_ids.csv")
  )
  
  stop("Some corrected_schedule_slot_id values do not exist in the schedule. Review IADB_03_bad_manual_slot_ids.csv.")
}

# ------------------------------------------------------------------------------
# Attaching final schedule variables -------------------------------------------
# ------------------------------------------------------------------------------
schedule_final_vars <- payment_schedule |>
  select(
    schedule_slot_id,
    confederate_match_key_schedule = confederate_match_key,
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

survey_matched <- survey_matched |>
  left_join(
    schedule_final_vars,
    by = c("schedule_slot_id_final" = "schedule_slot_id")
  ) |>
  mutate(
    funds_sent_before_attempt = case_when(
      !is.na(sent_datetime) & !is.na(submission_datetime) ~
        sent_datetime <= submission_datetime,
      TRUE ~ NA
    ),
    
    days_from_sent_to_attempt = as.numeric(
      difftime(submission_datetime, sent_datetime, units = "days")
    ),
    
    funding_delay_days = as.numeric(
      difftime(sent_datetime, send_by_datetime, units = "days")
    ),
    
    channel_adherent = case_when(
      !is.na(assigned_channel) & !is.na(channel_std) ~
        channel_std == assigned_channel,
      TRUE ~ NA
    ),
    
    amount_adherent = case_when(
      !is.na(assigned_amount) & !is.na(amount) ~
        amount == assigned_amount,
      TRUE ~ NA
    ),
    
    delivery_adherent = case_when(
      !is.na(assigned_delivery) & !is.na(delivery_std) ~
        delivery_std == assigned_delivery,
      TRUE ~ NA
    ),
    
    treatment_adherent = case_when(
      !is.na(channel_adherent) &
        !is.na(amount_adherent) &
        !is.na(delivery_adherent) ~
        channel_adherent & amount_adherent & delivery_adherent,
      TRUE ~ NA
    ),
    
    order_deviation = case_when(
      !is.na(survey_transaction_id_parsed) &
        !is.na(assigned_transaction_id) &
        survey_transaction_id_parsed != assigned_transaction_id ~ TRUE,
      TRUE ~ FALSE
    ),
    
    execution_status_attempt = case_when(
      !is.na(submission_datetime) & funds_sent_before_attempt ~
        "attempted_after_funding",
      
      !is.na(submission_datetime) & funds_sent & !funds_sent_before_attempt ~
        "attempted_before_recorded_funding",
      
      !is.na(submission_datetime) & !funds_sent ~
        "attempted_without_recorded_funding",
      
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------------------------
# Final duplicate schedule-slot diagnostics ------------------------------------
# ------------------------------------------------------------------------------
final_duplicate_slots <- survey_matched |>
  filter(!exclude_from_sap, !is.na(unique_transaction_id)) |>
  group_by(unique_transaction_id) |>
  summarise(
    n_rows = n(),
    survey_instances = paste(unique(survey_instance_id), collapse = " | "),
    raw_transaction_ids = paste(unique(survey_transaction_id_raw), collapse = " | "),
    channels = paste(unique(channel_std), collapse = " | "),
    amounts = paste(unique(amount), collapse = " | "),
    deliveries = paste(unique(delivery_std), collapse = " | "),
    outcomes = paste(unique(transaction_outcome_label), collapse = " | "),
    reviewed_values = paste(unique(reviewed_by_team), collapse = " | "),
    first_submission = min(submission_datetime, na.rm = TRUE),
    last_submission = max(submission_datetime, na.rm = TRUE),
    
    has_channel_conflict = n_distinct(channel_std, na.rm = TRUE) > 1,
    has_amount_conflict = n_distinct(amount, na.rm = TRUE) > 1,
    has_delivery_conflict = n_distinct(delivery_std, na.rm = TRUE) > 1,
    has_outcome_conflict = n_distinct(transaction_outcome_label, na.rm = TRUE) > 1,
    
    .groups = "drop"
  ) |>
  filter(n_rows > 1)

write_csv(
  final_duplicate_slots,
  file.path(output_dir, "IADB_03_final_duplicate_schedule_slots.csv")
)

conflicting_slots <- final_duplicate_slots |>
  filter(
    has_channel_conflict |
      has_amount_conflict |
      has_delivery_conflict |
      has_outcome_conflict
  )

if (nrow(conflicting_slots) > 0 && SKIP_MANUAL_REVIEW) {
  warning(
    "Conflicting duplicate schedule slots remain. ",
    "Because SKIP_MANUAL_REVIEW = TRUE, these slots will be excluded from the first-pass SAP dataset."
  )
  
  survey_matched <- survey_matched |>
    mutate(
      exclude_from_sap =
        exclude_from_sap |
        unique_transaction_id %in% conflicting_slots$unique_transaction_id
    )
}

if (nrow(conflicting_slots) > 0 && !SKIP_MANUAL_REVIEW) {
  stop(
    "Conflicting duplicate schedule slots remain. Review IADB_03_final_duplicate_schedule_slots.csv."
  )
}

# ------------------------------------------------------------------------------
# Deduplicating true resubmissions ---------------------------------------------
# ------------------------------------------------------------------------------
survey_dedup <- survey_matched |>
  filter(!exclude_from_sap, !is.na(unique_transaction_id)) |>
  arrange(
    unique_transaction_id,
    desc(reviewed_by_team_num),
    desc(submission_datetime)
  ) |>
  group_by(unique_transaction_id) |>
  slice(1) |>
  ungroup()

dedup_checks <- survey_dedup |>
  summarise(
    n_rows = n(),
    n_unique_transaction_id = n_distinct(unique_transaction_id),
    missing_unique_transaction_id = sum(is.na(unique_transaction_id))
  )

cat("\n=== Dedup checks ===\n")
print(dedup_checks)

stopifnot(dedup_checks$n_rows == dedup_checks$n_unique_transaction_id)
stopifnot(dedup_checks$missing_unique_transaction_id == 0)

# ------------------------------------------------------------------------------
# Saving excluded/unmatched SurveyCTO rows -------------------------------------
# ------------------------------------------------------------------------------
survey_excluded <- survey_matched |>
  filter(exclude_from_sap | is.na(unique_transaction_id))

unmatched_survey_rows <- survey_matched |>
  filter(!matched_to_schedule | is.na(best_schedule_slot_id))

write_csv(
  survey_excluded,
  file.path(output_dir, "IADB_03_survey_rows_excluded_from_sap_firstpass.csv")
)

write_csv(
  unmatched_survey_rows,
  file.path(output_dir, "IADB_03_unmatched_survey_rows.csv")
)

# ------------------------------------------------------------------------------
# Building schedule-level SAP denominator --------------------------------------
# ------------------------------------------------------------------------------
flag_cols <- names(survey_dedup)[str_detect(names(survey_dedup), "^flag_")]

survey_dedup_for_join <- survey_dedup |>
  select(
    any_of(c(
      "unique_transaction_id",
      "survey_instance_id",
      "survey_row_id",
      "submission_datetime",
      "transaction_date",
      "transaction_start_datetime",
      "scorecard_completed_datetime",
      "survey_confederate_id_raw",
      "survey_transaction_id_raw",
      "survey_transaction_uid_raw",
      "survey_transaction_id_parsed",
      "survey_transaction_order_parsed",
      
      "channel_std",
      "amount",
      "delivery_std",
      "country_clean",
      
      "transaction_outcome_label",
      "success",
      "kyc_score",
      "cost_local",
      "total_cost_without_time_local",
      "time_hours",
      "transaction_duration_hours",
      "interaction_time_hours",
      
      "reviewed_by_team",
      "reviewed_by_team_num",
      "data_quality_flag",
      "match_action",
      "source_of_decision",
      "manual_note",
      
      "treatment_adherent",
      "channel_adherent",
      "amount_adherent",
      "delivery_adherent",
      "order_deviation",
      "days_from_sent_to_attempt",
      "funding_delay_days",
      "execution_status_attempt",
      
      "j_comments",
      "k1_field_notes",
      "k2_red_flags",
      "k3_strengths",
      "k4_questions_uncertainties",
      "k5_overall_impressions"
    )),
    all_of(flag_cols)
  )

schedule_for_sap <- payment_schedule |>
  transmute(
    unique_transaction_id = schedule_slot_id,
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

sap_base <- schedule_for_sap |>
  left_join(
    survey_dedup_for_join,
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
    
    channel_adherent = case_when(
      attempted ~ channel_std == assigned_channel,
      TRUE ~ NA
    ),
    
    amount_adherent = case_when(
      attempted ~ amount == assigned_amount,
      TRUE ~ NA
    ),
    
    delivery_adherent = case_when(
      attempted ~ delivery_std == assigned_delivery,
      TRUE ~ NA
    ),
    
    treatment_adherent = case_when(
      attempted ~ channel_adherent & amount_adherent & delivery_adherent,
      TRUE ~ FALSE
    ),
    
    country = coalesce(country_clean, country_schedule_clean),
    
    # Assigned-treatment indicators for ITT / SAP.
    MTO = as.numeric(assigned_channel == "MTOs"),
    Fintech = as.numeric(assigned_channel == "Fintech"),
    Crypto = as.numeric(assigned_channel == "Crypto"),
    Amount250 = as.numeric(assigned_amount == 250),
    Online = as.numeric(assigned_delivery == "Online"),
    
    # Observed/as-treated indicators for robustness only.
    observed_MTO = as.numeric(channel_std == "MTOs"),
    observed_Fintech = as.numeric(channel_std == "Fintech"),
    observed_Crypto = as.numeric(channel_std == "Crypto"),
    observed_Amount250 = as.numeric(amount == 250),
    observed_Online = as.numeric(delivery_std == "Online"),
    
    # Analysis sample flags.
    sample_attempted = attempted,
    sample_success = attempted & !is.na(success),
    sample_kyc = attempted & !is.na(kyc_score),
    sample_cost_local = attempted & success == 1 & !is.na(cost_local),
    sample_time = attempted & success == 1 & !is.na(time_hours),
    sample_transaction_duration =
      attempted & !is.na(transaction_duration_hours),
    sample_per_protocol =
      attempted & treatment_adherent,
    sample_attempted_after_funding =
      execution_status == "attempted_after_funding"
  )

# ------------------------------------------------------------------------------
# SAP merge diagnostics --------------------------------------------------------
# ------------------------------------------------------------------------------
sap_merge_checks <- sap_base |>
  summarise(
    n_assigned_slots = n(),
    n_unique_transactions = n_distinct(unique_transaction_id),
    n_confederates = n_distinct(confederate_match_key),
    
    n_attempted = sum(attempted, na.rm = TRUE),
    n_not_attempted = sum(!attempted, na.rm = TRUE),
    
    n_funds_sent = sum(funds_sent, na.rm = TRUE),
    n_funds_not_sent = sum(!funds_sent, na.rm = TRUE),
    
    n_attempted_after_funding =
      sum(execution_status == "attempted_after_funding", na.rm = TRUE),
    
    n_attempted_before_recorded_funding =
      sum(execution_status == "attempted_before_recorded_funding", na.rm = TRUE),
    
    n_attempted_without_recorded_funding =
      sum(execution_status == "attempted_without_recorded_funding", na.rm = TRUE),
    
    n_funded_not_attempted =
      sum(execution_status == "funded_not_attempted", na.rm = TRUE),
    
    n_not_funded_not_attempted =
      sum(execution_status == "not_funded_not_attempted", na.rm = TRUE),
    
    n_channel_mismatch =
      sum(attempted & !channel_adherent, na.rm = TRUE),
    
    n_amount_mismatch =
      sum(attempted & !amount_adherent, na.rm = TRUE),
    
    n_delivery_mismatch =
      sum(attempted & !delivery_adherent, na.rm = TRUE),
    
    n_success_sample = sum(sample_success, na.rm = TRUE),
    n_kyc_sample = sum(sample_kyc, na.rm = TRUE),
    n_cost_local_sample = sum(sample_cost_local, na.rm = TRUE),
    n_time_sample = sum(sample_time, na.rm = TRUE),
    
    pct_attempted = 100 * n_attempted / n_assigned_slots
  )

cat("\n=== SAP merge checks ===\n")
print(sap_merge_checks)

write_csv(
  sap_merge_checks,
  file.path(output_dir, "IADB_03_sap_merge_checks.csv")
)

# ------------------------------------------------------------------------------
# Output diagnostics: non-attempts and protocol deviations ---------------------
# ------------------------------------------------------------------------------
non_attempts <- sap_base |>
  filter(!attempted)

protocol_deviations <- sap_base |>
  filter(
    attempted,
    !channel_adherent | !amount_adherent | !delivery_adherent
  ) |>
  select(
    unique_transaction_id,
    confederate_match_key,
    assigned_transaction_id,
    assigned_order,
    assigned_date,
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    channel_std,
    amount,
    delivery_std,
    survey_transaction_id_raw,
    submission_datetime,
    transaction_date,
    transaction_outcome_label,
    success,
    treatment_adherent,
    channel_adherent,
    amount_adherent,
    delivery_adherent,
    order_deviation,
    match_action,
    data_quality_flag
  )

write_csv(
  non_attempts,
  file.path(output_dir, "IADB_03_non_attempted_schedule_slots.csv")
)

write_csv(
  protocol_deviations,
  file.path(output_dir, "IADB_03_protocol_deviations.csv")
)

# ------------------------------------------------------------------------------
# Bad / incomplete survey completion review ------------------------------------
# ------------------------------------------------------------------------------
bad_survey_completion_review <- sap_base |>
  filter(
    attempted,
    is.na(data_quality_flag) |
      data_quality_flag != "OK" |
      is.na(success) |
      is.na(kyc_score) |
      (success == 1 & is.na(cost_local)) |
      (success == 1 & is.na(time_hours)) |
      if_any(starts_with("flag_negative"), ~ .x %in% TRUE) |
      if_any(starts_with("flag_extreme"), ~ .x %in% TRUE) |
      if_any(starts_with("flag_invalid"), ~ .x %in% TRUE)
  ) |>
  select(
    unique_transaction_id,
    survey_instance_id,
    confederate_match_key,
    assigned_transaction_id,
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    channel_std,
    amount,
    delivery_std,
    transaction_outcome_label,
    success,
    kyc_score,
    cost_local,
    time_hours,
    transaction_duration_hours,
    data_quality_flag,
    survey_transaction_id_raw,
    submission_datetime,
    transaction_date,
    reviewed_by_team,
    j_comments,
    k1_field_notes,
    k2_red_flags,
    k4_questions_uncertainties,
    starts_with("flag_")
  ) |>
  mutate(
    manual_review_action = NA_character_,
    manual_review_note = NA_character_
  )

write_csv(
  bad_survey_completion_review,
  file.path(output_dir, "IADB_03_bad_survey_completion_review.csv")
)

# ------------------------------------------------------------------------------
# First-pass SAP observed-attempt sample ---------------------------------------
# ------------------------------------------------------------------------------
sap_observed <- sap_base |>
  filter(attempted)

sap_attempted_after_funding <- sap_base |>
  filter(execution_status == "attempted_after_funding")

sap_per_protocol <- sap_base |>
  filter(attempted, treatment_adherent)

pre_sap_checks <- sap_observed |>
  summarise(
    n = n(),
    n_confederates = n_distinct(confederate_match_key),
    n_unique_transactions = n_distinct(unique_transaction_id),
    
    missing_unique_transaction_id = sum(is.na(unique_transaction_id)),
    missing_success = sum(is.na(success)),
    missing_kyc = sum(is.na(kyc_score)),
    missing_assigned_channel = sum(is.na(assigned_channel)),
    missing_assigned_amount = sum(is.na(assigned_amount)),
    missing_assigned_delivery = sum(is.na(assigned_delivery)),
    
    completed_missing_cost_local =
      sum(success == 1 & is.na(cost_local), na.rm = TRUE),
    
    completed_missing_time =
      sum(success == 1 & is.na(time_hours), na.rm = TRUE),
    
    n_protocol_deviations =
      sum(!treatment_adherent, na.rm = TRUE)
  )

cat("\n=== Pre-SAP observed-attempt checks ===\n")
print(pre_sap_checks)

stopifnot(pre_sap_checks$missing_unique_transaction_id == 0)
stopifnot(pre_sap_checks$n == pre_sap_checks$n_unique_transactions)
stopifnot(pre_sap_checks$missing_success == 0)
stopifnot(pre_sap_checks$missing_kyc == 0)
stopifnot(pre_sap_checks$missing_assigned_channel == 0)
stopifnot(pre_sap_checks$missing_assigned_amount == 0)
stopifnot(pre_sap_checks$missing_assigned_delivery == 0)

# ------------------------------------------------------------------------------
# Saving final outputs ---------------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(
  survey_matched,
  file.path(output_dir, "IADB_03_surveycto_schedule_matched_full_audit.csv")
)

write_csv(
  survey_dedup,
  file.path(output_dir, "IADB_03_surveycto_schedule_matched_dedup.csv")
)

saveRDS(
  survey_dedup,
  file.path(output_dir, "IADB_03_surveycto_schedule_matched_dedup.rds")
)

write_csv(
  sap_base,
  file.path(output_dir, "IADB_sap_schedule_level_base.csv")
)

saveRDS(
  sap_base,
  file.path(output_dir, "IADB_sap_schedule_level_base.rds")
)

write_csv(
  sap_observed,
  file.path(output_dir, "IADB_sap_observed_first_pass.csv")
)

saveRDS(
  sap_observed,
  file.path(output_dir, "IADB_sap_observed_first_pass.rds")
)

write_csv(
  sap_attempted_after_funding,
  file.path(output_dir, "IADB_sap_attempted_after_funding.csv")
)

saveRDS(
  sap_attempted_after_funding,
  file.path(output_dir, "IADB_sap_attempted_after_funding.rds")
)

write_csv(
  sap_per_protocol,
  file.path(output_dir, "IADB_sap_per_protocol.csv")
)

saveRDS(
  sap_per_protocol,
  file.path(output_dir, "IADB_sap_per_protocol.rds")
)

cat("\nSaved SAP dataset outputs to:\n")
cat(output_dir, "\n")

cat("\nKey outputs:\n")
cat(file.path(output_dir, "IADB_sap_schedule_level_base.rds"), "\n")
cat(file.path(output_dir, "IADB_sap_observed_first_pass.rds"), "\n")
cat(file.path(output_dir, "IADB_manual_schedule_match_review_to_complete.csv"), "\n")
cat(file.path(output_dir, "IADB_03_bad_survey_completion_review.csv"), "\n")

if (SKIP_MANUAL_REVIEW) {
  cat("\nNOTE: SKIP_MANUAL_REVIEW = TRUE.\n")
  cat("This is a first-pass automated SAP dataset.\n")
  cat("Ambiguous/unmatched SurveyCTO rows were excluded.\n")
  cat("For final SAP, complete IADB_manual_schedule_match_review_to_complete.csv,\n")
  cat("save it as IADB_manual_schedule_match_review_completed.csv,\n")
  cat("and rerun with SKIP_MANUAL_REVIEW = FALSE.\n")
}
