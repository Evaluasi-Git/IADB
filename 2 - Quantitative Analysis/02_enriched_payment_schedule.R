# ==============================================================================
# IADB - Build Enriched Payment/Randomization Schedule -------------------------
# Author: Cedric Antunes (Evaluasi)
# Date: May 11, 2026
# Purpose:
#   1. Load individual randomized transaction schedules;
#   2. Stack and standardize them;
#   3. Load internal payment-tracking schedule;
#   4. Match payment-tracking rows to randomized schedule slots;
#   5. Recover randomized channel, transaction order, amount, and delivery;
#   6. Export enriched schedule for SAP dataset builder.
#
# Key output:
#   data/clean/sap_dataset_builder/IADB_payment_schedule_enriched_with_randomization.csv
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
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Payment tracking (Cedric and Kathery access only)
payment_tracking_path <- here(
  "data",
  "raw",
  "[IADB] - Internal Payment Tracking - Payment Schedule.csv"
)

# Output
output_dir <- here("data", "clean", "sap_dataset_builder")

# Create outcome directory (uncomment if needed)
#dir.create(output_dir, 
#           showWarnings = FALSE, 
#           recursive = TRUE)

randomized_schedule_files <- c(
  here("IADB", "data", "randomization", "master_schedule_feb13.csv"),
  here("IADB", "data", "randomization", "master_schedule_feb26.csv"),
  here("IADB", "data", "randomization", "master_schedule_mar12.csv"),
  here("IADB", "data", "randomization", "master_schedule_mar30_may15.csv"),
  here("IADB", "data", "randomization", "master_schedule_apr24_may25.csv")
)

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Encoding
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

# Safe date
parse_datetime_flexible <- function(x) {
  parse_date_time(
    as.character(x),
    orders = c(
      "ymd HMS", "ymd HM", "ymd",
      "mdy HMS", "mdy HM", "mdy",
      "dmy HMS", "dmy HM", "dmy"
    ),
    quiet = TRUE
  )
}

parse_date_flexible <- function(x) {
  as.Date(parse_datetime_flexible(x))
}

parse_number_safe <- function(x) {
  readr::parse_number(as.character(x))
}

detect_text <- function(x, pattern) {
  replace_na(
    str_detect(str_to_lower(as.character(x)), pattern),
    FALSE
  )
}

logical_score <- function(x) {
  as.numeric(replace_na(x, FALSE))
}

# ------------------------------------------------------------------------------
# Loading and stacking randomized schedules ------------------------------------
# ------------------------------------------------------------------------------
randomized_slots_raw <- randomized_schedule_files |>
  set_names(basename(randomized_schedule_files)) |>
  map_dfr(
    ~ read_csv(
      .x,
      show_col_types = FALSE,
      col_types = cols(.default = col_character())
    ) |>
      clean_names(),
    .id = "schedule_file"
  )

randomized_slots <- randomized_slots_raw |>
  mutate(
    schedule_file_date = case_when(
      str_detect(schedule_file, "feb13") ~ as.Date("2026-02-13"),
      str_detect(schedule_file, "feb26") ~ as.Date("2026-02-26"),
      str_detect(schedule_file, "mar12") ~ as.Date("2026-03-12"),
      str_detect(schedule_file, "mar30") ~ as.Date("2026-03-30"),
      str_detect(schedule_file, "apr24") ~ as.Date("2026-04-24"),
      TRUE ~ as.Date(NA)
    ),
    
    schedule_confederate_name_raw = confederate_name,
    
    # Manual fixes (Cedric's sloppiness here):
    # Earlier schedule versions labelled Laura's confederate name as
    # "Colombia".
    schedule_confederate_name_clean = case_when(
      normalize_key(confederate_name) == "colombia" ~
        "Laura Isabel Tabares Pena",
      TRUE ~ confederate_name
    ),
    
    confederate_match_key =
      standardize_confederate_id(schedule_confederate_name_clean),
    
    country_schedule_clean = standardize_country(country),
    
    # Renaming all randomized-schedule timing/order fields before any join.
    # This prevents .x/.y suffix problems.
    randomized_transaction_order = as.integer(transaction_order),
    randomized_block_10 = as.integer(block_10),
    randomized_phase = as.integer(phase),
    randomized_assigned_week = as.integer(assigned_week),
    randomized_approximate_date = parse_date_flexible(approximate_date),
    
    assigned_channel = standardize_channel(channel),
    assigned_amount = parse_number_safe(amount_usd),
    assigned_delivery = standardize_delivery(delivery_method),
    
    assigned_transaction_id = sprintf("T%03d", randomized_transaction_order),
    
    randomized_slot_key = paste(
      confederate_match_key,
      randomized_transaction_order,
      assigned_channel,
      assigned_amount,
      assigned_delivery,
      randomized_approximate_date,
      sep = "_"
    )
  )

# Removing exact duplicate randomized slots.
# This handles cases such as Feb13 and Feb26 being identical.
randomized_slots_dedup <- randomized_slots |>
  arrange(
    confederate_match_key,
    randomized_transaction_order,
    desc(schedule_file_date)
  ) |>
  distinct(
    randomized_slot_key,
    .keep_all = TRUE
  )

# Diagnostics for randomized schedules
randomized_schedule_checks <- randomized_slots_dedup |>
  summarise(
    n_randomized_slots = n(),
    n_confederates = n_distinct(confederate_match_key),
    missing_confederate = sum(is.na(confederate_match_key) | confederate_match_key == ""),
    missing_transaction_order = sum(is.na(randomized_transaction_order)),
    missing_channel = sum(is.na(assigned_channel)),
    missing_amount = sum(is.na(assigned_amount)),
    missing_delivery = sum(is.na(assigned_delivery)),
    missing_approximate_date = sum(is.na(randomized_approximate_date))
  )

cat("\n=== Randomized schedule checks ===\n")
print(randomized_schedule_checks)

# ------------------------------------------------------------------------------
# Loading payment-tracking schedule --------------------------------------------
# ------------------------------------------------------------------------------
payment_tracking_raw <- read_csv(
  payment_tracking_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  clean_names()

cat("\n=== Payment-tracking columns ===\n")
print(names(payment_tracking_raw))

payment_tracking <- payment_tracking_raw |>
  mutate(
    payment_tracking_row_id = row_number(),
    
    payment_confederate_name_raw = confederate_name,
    payment_confederate_id_raw = confederate_id,
    
    confederate_match_key = standardize_confederate_id(confederate_name),
    
    country_payment_clean = standardize_country(country),
    
    amount_usd_raw = amount_usd,
    delivery_method_raw = delivery_method,
    
    do_not_send_money =
      # Kathery's notes to Cedric 
      detect_text(amount_usd_raw, "don't send|do not send") |
      detect_text(delivery_method_raw, "don't send|do not send"),
    
    payment_amount_raw_parsed =
      suppressWarnings(parse_number_safe(amount_usd_raw)),
    
    payment_amount = case_when(
      do_not_send_money ~ NA_real_,
      TRUE ~ payment_amount_raw_parsed
    ),
    
    payment_delivery = case_when(
      do_not_send_money ~ NA_character_,
      TRUE ~ standardize_delivery(delivery_method_raw)
    ),
    
    payment_assigned_week = as.integer(assigned_week),
    payment_phase = as.integer(phase),
    payment_approximate_date = parse_date_flexible(approximate_date),
    
    payment_transaction_datetime =
      parse_datetime_flexible(transaction_datetime),
    
    payment_send_by_date = parse_date_flexible(send_by_date),
    payment_send_by_datetime = parse_datetime_flexible(send_by_datetime),
    payment_sent_datetime = parse_datetime_flexible(sent_datetime),
    
    payment_status_clean = str_to_upper(str_squish(payment_status)),
    
    funds_sent = payment_status_clean %in% c(
      "SENT", "PAID", "TRANSFERRED", "COMPLETED"
    ),
    
    payment_ref = payment_ref,
    payment_method = payment_method,
    payment_notes = notes
  ) |>
  filter(
    !is.na(confederate_match_key),
    confederate_match_key != "",
    payment_confederate_id_raw != "DROPPED"
  )

# Diagnostics for payment tracking
payment_tracking_checks <- payment_tracking |>
  summarise(
    n_payment_rows = n(),
    n_confederates = n_distinct(confederate_match_key),
    n_do_not_send_money = sum(do_not_send_money, na.rm = TRUE),
    missing_confederate = sum(is.na(confederate_match_key) | confederate_match_key == ""),
    missing_amount = sum(is.na(payment_amount) & !do_not_send_money),
    missing_delivery = sum(is.na(payment_delivery) & !do_not_send_money),
    missing_approximate_date = sum(is.na(payment_approximate_date)),
    n_funds_sent = sum(funds_sent, na.rm = TRUE),
    n_funds_not_sent = sum(!funds_sent, na.rm = TRUE)
  )

cat("\n=== Payment-tracking checks ===\n")
print(payment_tracking_checks)

# ------------------------------------------------------------------------------
# Exact one-to-one match: payment rows to randomized slots ---------------------
# ------------------------------------------------------------------------------
# Rationale:
#   The payment-tracking schedule was generated from the randomized schedules,
#   but it does not contain assigned_channel. For most confederates, the exact
#   matching fields uniquely identify a randomized slot. However, late-recruited
#   replacement confederates were assigned compressed schedules that allowed up to
#   two transactions on the same day. Therefore, some distinct randomized
#   transactions share the same observable matching stratum:
#
#     confederate + country + amount + delivery + week + phase + approximate_date
#
#   These are not duplicate transactions. They are separate assigned transactions
#   that happen to be observationally identical in the payment-tracking file once
#   assigned_channel is absent.
#
#   If each payment row is scored independently, two rows in the same stratum may
#   select the same randomized slot. To prevent this, I match within exact strata
#   and pair the kth payment row to the kth randomized slot using row order within
#   the stratum. This preserves the number of assigned transactions and prevents
#   duplicate assignment of the same randomized slot.
#
# Key assumption:
#   Within each exact stratum, the payment-tracking file preserves the same row
#   order as the randomized schedule. This assumption is only needed for strata
#   with more than one row, which arise from compressed late-entry schedules.
#
# Late-entry confederates with compressed schedules:
#   mey_uehara, diego_chambi, amanda_zabeu, erick_menendez,
#   aixa_meli, gabriela_villalta
payment_exact <- payment_tracking |>
  filter(!do_not_send_money) |>
  transmute(
    payment_tracking_row_id,
    confederate_match_key,
    country_key = country_payment_clean,
    amount_key = payment_amount,
    delivery_key = payment_delivery,
    week_key = payment_assigned_week,
    phase_key = payment_phase,
    date_key = payment_approximate_date,
    
    payment_confederate_name_raw,
    payment_confederate_id_raw,
    country,
    amount_usd_raw,
    delivery_method_raw,
    payment_amount,
    payment_delivery,
    payment_assigned_week,
    payment_phase,
    payment_approximate_date,
    payment_transaction_datetime,
    payment_send_by_date,
    payment_send_by_datetime,
    payment_sent_datetime,
    payment_status,
    payment_status_clean,
    funds_sent,
    payment_ref,
    payment_method,
    payment_notes
  ) |>
  arrange(
    confederate_match_key,
    date_key,
    week_key,
    phase_key,
    amount_key,
    delivery_key,
    payment_tracking_row_id
  ) |>
  group_by(
    confederate_match_key,
    country_key,
    amount_key,
    delivery_key,
    week_key,
    phase_key,
    date_key
  ) |>
  mutate(payment_row_in_stratum = row_number()) |>
  ungroup()

randomized_exact <- randomized_slots_dedup |>
  transmute(
    confederate_match_key,
    country_key = country_schedule_clean,
    amount_key = assigned_amount,
    delivery_key = assigned_delivery,
    week_key = randomized_assigned_week,
    phase_key = randomized_phase,
    date_key = randomized_approximate_date,
    
    matched_schedule_file = schedule_file,
    matched_schedule_file_date = schedule_file_date,
    
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    transaction_order = randomized_transaction_order,
    assigned_transaction_id,
    randomized_approximate_date,
    randomized_assigned_week,
    randomized_phase,
    randomized_block_10,
    country_schedule_clean
  ) |>
  arrange(
    confederate_match_key,
    date_key,
    week_key,
    phase_key,
    amount_key,
    delivery_key,
    transaction_order
  ) |>
  group_by(
    confederate_match_key,
    country_key,
    amount_key,
    delivery_key,
    week_key,
    phase_key,
    date_key
  ) |>
  mutate(randomized_row_in_stratum = row_number()) |>
  ungroup()

# ------------------------------------------------------------------------------
# Diagnostic: payment/randomized exact-strata count check ----------------------
# ------------------------------------------------------------------------------
late_entry_confederates <- c(
  "mey_uehara",
  "diego_chambi",
  "amanda_zabeu",
  "erick_menendez",
  "aixa_meli",
  "gabriela_villalta"
)

payment_strata_counts <- payment_exact |>
  count(
    confederate_match_key,
    country_key,
    amount_key,
    delivery_key,
    week_key,
    phase_key,
    date_key,
    name = "n_payment_rows"
  )

randomized_strata_counts <- randomized_exact |>
  count(
    confederate_match_key,
    country_key,
    amount_key,
    delivery_key,
    week_key,
    phase_key,
    date_key,
    name = "n_randomized_slots"
  )

strata_count_check <- payment_strata_counts |>
  left_join(
    randomized_strata_counts,
    by = c(
      "confederate_match_key",
      "country_key",
      "amount_key",
      "delivery_key",
      "week_key",
      "phase_key",
      "date_key"
    )
  ) |>
  mutate(
    n_randomized_slots = replace_na(n_randomized_slots, 0L),
    
    repeated_payment_stratum = n_payment_rows > 1,
    
    insufficient_randomized_slots =
      n_randomized_slots < n_payment_rows,
    
    repeated_stratum_expected =
      !repeated_payment_stratum |
      confederate_match_key %in% late_entry_confederates
  )

critical_strata_count_mismatches <- strata_count_check |>
  filter(insufficient_randomized_slots)

unexpected_repeated_payment_strata <- strata_count_check |>
  filter(
    repeated_payment_stratum,
    !repeated_stratum_expected
  )

repeated_payment_strata <- strata_count_check |>
  filter(repeated_payment_stratum)

strata_count_summary <- tibble(
  n_payment_strata = nrow(strata_count_check),
  n_repeated_payment_strata = sum(strata_count_check$repeated_payment_stratum),
  n_critical_strata_count_mismatches = nrow(critical_strata_count_mismatches),
  n_unexpected_repeated_payment_strata = nrow(unexpected_repeated_payment_strata),
  max_payment_rows_in_stratum = max(strata_count_check$n_payment_rows, na.rm = TRUE)
)

cat("\n=== Payment/randomized exact-strata count summary ===\n")
print(strata_count_summary)

cat("\n=== Repeated payment strata by confederate ===\n")
print(
  repeated_payment_strata |>
    count(confederate_match_key, name = "n_repeated_strata") |>
    arrange(desc(n_repeated_strata)),
  n = Inf
)

if (nrow(critical_strata_count_mismatches) > 0) {
  stop(
    "Some payment strata have fewer randomized slots than payment rows. ",
    "Review IADB_payment_critical_strata_count_mismatches.csv."
  )
}

if (nrow(unexpected_repeated_payment_strata) > 0) {
  warning(
    "Repeated payment strata found outside expected late-entry confederates. ",
    "Review IADB_payment_unexpected_repeated_exact_strata.csv."
  )
}

# ------------------------------------------------------------------------------
# Join -------------------------------------------------------------------------
# ------------------------------------------------------------------------------
payment_randomization_exact_match <- payment_exact |>
  left_join(
    randomized_exact,
    by = c(
      "confederate_match_key",
      "country_key",
      "amount_key",
      "delivery_key",
      "week_key",
      "phase_key",
      "date_key",
      "payment_row_in_stratum" = "randomized_row_in_stratum"
    )
  ) |>
  mutate(
    randomization_match_method = "exact_stratum_one_to_one",
    
    randomization_match_confidence = case_when(
      !is.na(transaction_order) ~ "high",
      TRUE ~ "unmatched"
    ),
    
    best_candidate_score = NA_real_,
    second_candidate_score = NA_real_,
    candidate_margin = NA_real_,
    n_candidates = NA_integer_
  )

# ------------------------------------------------------------------------------
# Diagnostics for exact one-to-one matching ------------------------------------
# ------------------------------------------------------------------------------
exact_match_checks <- payment_randomization_exact_match |>
  summarise(
    n_payment_rows = n(),
    n_matched = sum(!is.na(transaction_order), na.rm = TRUE),
    n_unmatched = sum(is.na(transaction_order), na.rm = TRUE),
    n_unique_schedule_slots = n_distinct(
      paste0(confederate_match_key, "_S", sprintf("%03d", transaction_order)),
      na.rm = TRUE
    )
  )

print(exact_match_checks)

exact_duplicate_slots <- payment_randomization_exact_match |>
  filter(!is.na(transaction_order)) |>
  mutate(
    schedule_slot_id = paste0(
      confederate_match_key,
      "_S",
      sprintf("%03d", transaction_order)
    )
  ) |>
  count(schedule_slot_id, sort = TRUE) |>
  filter(n > 1)

print(exact_duplicate_slots, n = Inf)

#write_csv(
#  exact_match_checks,
#  file.path(output_dir, "IADB_exact_match_checks.csv")
#)

#write_csv(
#  exact_duplicate_slots,
#  file.path(output_dir, "IADB_exact_match_duplicate_slots.csv")
#)

# ------------------------------------------------------------------------------
# Selecting best exact randomized match ----------------------------------------
# ------------------------------------------------------------------------------
payment_randomization_best <- payment_randomization_exact_match |>
  select(
    payment_tracking_row_id,
    
    matched_schedule_file,
    matched_schedule_file_date,
    
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    transaction_order,
    assigned_transaction_id,
    
    randomized_approximate_date,
    randomized_assigned_week,
    randomized_phase,
    randomized_block_10,
    country_schedule_clean,
    
    randomization_match_method,
    randomization_match_confidence,
    best_candidate_score,
    second_candidate_score,
    candidate_margin,
    n_candidates
  )

# ------------------------------------------------------------------------------
# Building enriched payment/randomization schedule -----------------------------
# ------------------------------------------------------------------------------
payment_schedule_enriched <- payment_tracking |>
  left_join(
    payment_randomization_best,
    by = "payment_tracking_row_id"
  ) |>
  mutate(
    schedule_slot_id = case_when(
      !is.na(transaction_order) ~ paste0(
        confederate_match_key,
        "_S",
        sprintf("%03d", transaction_order)
      ),
      TRUE ~ NA_character_
    ),
    
    unique_transaction_id = schedule_slot_id,
    
    has_randomized_channel = !is.na(assigned_channel),
    
    schedule_row_status = case_when(
      do_not_send_money ~ "operational_do_not_send",
      is.na(schedule_slot_id) ~ "unmatched_payment_row",
      randomization_match_confidence == "low_review" ~ "matched_low_confidence",
      TRUE ~ "matched"
    )
  )

# ------------------------------------------------------------------------------
# Diagnostics/sanity checks ----------------------------------------------------
# ------------------------------------------------------------------------------
enriched_schedule_checks <- payment_schedule_enriched |>
  summarise(
    n_rows = n(),
    n_confederates = n_distinct(confederate_match_key),
    
    n_matched_to_randomization =
      sum(!is.na(schedule_slot_id), na.rm = TRUE),
    
    n_unmatched_to_randomization =
      sum(is.na(schedule_slot_id), na.rm = TRUE),
    
    n_missing_assigned_channel =
      sum(is.na(assigned_channel), na.rm = TRUE),
    
    n_do_not_send_money =
      sum(do_not_send_money, na.rm = TRUE),
    
    n_low_confidence_matches =
      sum(randomization_match_confidence == "low_review", na.rm = TRUE),
    
    n_high_confidence_matches =
      sum(randomization_match_confidence == "high", na.rm = TRUE),
    
    n_medium_confidence_matches =
      sum(randomization_match_confidence == "medium", na.rm = TRUE),
    
    n_funds_sent = sum(funds_sent, na.rm = TRUE),
    n_funds_not_sent = sum(!funds_sent, na.rm = TRUE)
  )

cat("\n=== Enriched schedule checks ===\n")
print(enriched_schedule_checks)

enriched_schedule_by_status <- payment_schedule_enriched |>
  count(schedule_row_status, randomization_match_confidence, sort = TRUE)

cat("\n=== Enriched schedule by status ===\n")
print(enriched_schedule_by_status)

duplicate_schedule_slots <- payment_schedule_enriched |>
  filter(!is.na(schedule_slot_id)) |>
  count(schedule_slot_id, sort = TRUE) |>
  filter(n > 1)

cat("\n=== Duplicate schedule slots ===\n")
print(duplicate_schedule_slots, n = Inf)

# Export diagnostics
write_csv(
  enriched_schedule_checks,
  file.path(output_dir, "IADB_enriched_schedule_checks.csv")
)

write_csv(
  enriched_schedule_by_status,
  file.path(output_dir, "IADB_enriched_schedule_by_status.csv")
)

write_csv(
  duplicate_schedule_slots,
  file.path(output_dir, "IADB_enriched_schedule_duplicate_slots.csv")
)

# ------------------------------------------------------------------------------
# Exporting rows needing review ------------------------------------------------
# ------------------------------------------------------------------------------
enriched_schedule_review <- payment_schedule_enriched |>
  filter(
    do_not_send_money |
      is.na(schedule_slot_id) |
      randomization_match_confidence == "low_review" |
      schedule_slot_id %in% duplicate_schedule_slots$schedule_slot_id
  ) |>
  select(
    payment_tracking_row_id,
    
    payment_confederate_name_raw,
    payment_confederate_id_raw,
    confederate_match_key,
    
    country,
    country_payment_clean,
    amount_usd_raw,
    delivery_method_raw,
    payment_amount,
    payment_delivery,
    
    payment_assigned_week,
    payment_phase,
    payment_approximate_date,
    
    payment_status,
    payment_status_clean,
    funds_sent,
    payment_sent_datetime,
    payment_ref,
    payment_method,
    payment_notes,
    
    matched_schedule_file,
    matched_schedule_file_date,
    
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    transaction_order,
    assigned_transaction_id,
    
    randomized_approximate_date,
    randomized_assigned_week,
    randomized_phase,
    randomized_block_10,
    
    randomization_match_confidence,
    best_candidate_score,
    second_candidate_score,
    candidate_margin,
    n_candidates,
    
    schedule_slot_id,
    schedule_row_status,
    do_not_send_money
  )

write_csv(
  enriched_schedule_review,
  file.path(output_dir, "IADB_enriched_schedule_review_needed.csv")
)

# ------------------------------------------------------------------------------
# Saving enriched schedule -----------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(
  payment_schedule_enriched,
  file.path(output_dir, "IADB_payment_schedule_enriched_with_randomization.csv")
)

saveRDS(
  payment_schedule_enriched,
  file.path(output_dir, "IADB_payment_schedule_enriched_with_randomization.rds")
)
