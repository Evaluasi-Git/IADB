# Title: IADB - Raw SurveyCTO data cleaning ------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: April, 2026 ------------------------------------------------------------

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
})

# ------------------------------------------------------------------------------
# Loading raw data -------------------------------------------------------------
# ------------------------------------------------------------------------------

raw_path <- "D:/Users/cedric/Downloads/IADB_Survey_WIDE_apr23.csv"

raw <- read_csv(raw_path, show_col_types = FALSE) |>
  clean_names()

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Safe numeric reading ---------------------------------------------------------
to_num <- function(x) {
  parse_number(as.character(x), locale = locale(decimal_mark = ".", grouping_mark = ","))
}

# Safe yes/no ------------------------------------------------------------------
to_yesno <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  case_when(
    x %in% c("1", "yes", "sim", "sí", "si", "true") ~ 1,
    x %in% c("0", "no", "false") ~ 0,
    TRUE ~ NA_real_
  )
}

# Cleaning strings -------------------------------------------------------------
clean_text <- function(x) {
  str_to_lower(str_squish(as.character(x)))
}

# Safe date reading ------------------------------------------------------------
safe_ymd_hms <- function(x) {
  parse_date_time(
    x,
    orders = c("ymd HMS", "ymd HM", "ymd", "mdy HMS", "mdy HM", "dmy HMS", "dmy HM"),
    quiet = TRUE
  )
}

# ------------------------------------------------------------------------------
# Country-level lookup for time-cost calculation -------------------------------
# ------------------------------------------------------------------------------
min_wage_lookup <- tribble(
  ~country_clean, ~hourly_earnings_ppp, ~wage_year, ~wage_source_note,
  "brazil",    8.12, 2025,  "ILOSTAT average hourly earnings of employees in PPP$",
  "brasil",    8.12, 2025,  "ILOSTAT average hourly earnings of employees in PPP$",
  "chile",     12.71, 2024, "ILOSTAT average hourly earnings of employees in PPP$",
  "colombia",  6.01, 2025,  "ILOSTAT average hourly earnings of employees in PPP$",
  "ecuador",   7.71, 2025,  "ILOSTAT average hourly earnings of employees in PPP$",
  "mexico",    5.29, 2025,  "ILOSTAT average hourly earnings of employees in PPP$",
  "nicaragua", 2.55, 2012,  "ILOSTAT average hourly earnings of employees in PPP$",
  "panama",    10.33, 2024, "ILOSTAT average hourly earnings of employees in PPP$",
  "peru",      6.13, 2025,  "ILOSTAT average hourly earnings of employees in PPP$"
)

# ------------------------------------------------------------------------------
# Main cleaning pipeline -------------------------------------------------------
# ------------------------------------------------------------------------------
df_clean <- raw |>
  mutate(
    # Dates and identifiers
    submission_datetime = safe_ymd_hms(submission_date),
    start_datetime      = safe_ymd_hms(starttime),
    end_datetime        = safe_ymd_hms(endtime),
    transaction_date_clean = ymd(transaction_date, quiet = TRUE),
    transaction_time_clean = as.character(transaction_time),
    # Country
    country_clean = clean_text(transaction_country),
    # Confederates identifier
    confederate_id = case_when(
      !is.na(confederate_id) & confederate_id != "" ~ as.character(confederate_id),
      !is.na(enumerator_id) & enumerator_id != "" ~ as.character(enumerator_id),
      !is.na(enumerator_name) & enumerator_name != "" ~ as.character(enumerator_name),
      TRUE ~ NA_character_
    ),
    # Numeric SurveyCTO codes
    institution_type_num    = as.numeric(institution_type),
    transaction_method_num  = as.numeric(transaction_method),
    transaction_outcome_num = as.numeric(transaction_outcome),
    channel = case_when(
      institution_type_num == 0 ~ "Banks",
      institution_type_num == 1 ~ "MTOs",
      institution_type_num == 2 ~ "Fintech",
      institution_type_num == 3 ~ "Crypto",
      TRUE ~ NA_character_
    ),
    MTO     = as.numeric(channel == "MTOs"),
    Fintech = as.numeric(channel == "Fintech"),
    Crypto  = as.numeric(channel == "Crypto"),
    # Delivery method
    delivery = case_when(
      transaction_method_num == 0 ~ "In-person",
      transaction_method_num == 1 ~ "Online",
      TRUE ~ NA_character_
    ),
    Online = as.numeric(delivery == "Online"),
    # Transaction outcome
    transaction_outcome_label = case_when(
      transaction_outcome_num == 0 ~ "Completed",
      transaction_outcome_num == 1 ~ "Rejected",
      transaction_outcome_num == 2 ~ "Incomplete",
      transaction_outcome_num == 3 ~ "Abandoned",
      TRUE ~ NA_character_
    ),
    success = case_when(
      transaction_outcome_num == 0 ~ 1,
      transaction_outcome_num %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    # Transaction amount
    amount = to_num(transaction_amount),
    Amount250 = as.numeric(amount == 250),
    # KYC score
    kyc_score_reported = to_num(kyc_score_0_3),
    kyc_doc_govid_num       = to_yesno(kyc_doc_govid),
    kyc_doc_address_num     = to_yesno(kyc_doc_address),
    kyc_doc_add_id_num      = to_yesno(kyc_doc_additional_id),
    kyc_doc_biometrics_num  = to_yesno(kyc_doc_biometrics),
    kyc_doc_taxid_num       = to_yesno(kyc_doc_taxid),
    kyc_doc_sourcefunds_num = to_yesno(kyc_doc_sourcefunds),
    kyc_source_asked_num    = to_yesno(kyc_asked_sourcefunds),
    kyc_score_constructed = case_when(
      kyc_doc_govid_num == 1 &
        (kyc_doc_address_num == 1 | kyc_doc_taxid_num == 1) &
        (kyc_doc_sourcefunds_num == 1 |
           kyc_source_asked_num == 1 |
           kyc_doc_biometrics_num == 1) ~ 3,
      kyc_doc_govid_num == 1 &
        (kyc_doc_address_num == 1 |
           kyc_doc_taxid_num == 1 |
           kyc_doc_add_id_num == 1) ~ 2,
      kyc_doc_govid_num == 1 ~ 1,
      kyc_doc_govid_num == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    kyc_score = coalesce(kyc_score_reported, kyc_score_constructed),
    # Time variables
    tx_initiated_datetime = safe_ymd_hms(tx_initiated_ddt),
    tx_received_datetime  = safe_ymd_hms(j2_received_datetime),
    tx_settlement_hours_num = to_num(tx_settlement_hours),
    exact_minutes_num       = to_num(i1_exact_minutes),
    duration_minutes_num    = to_num(duration_minutes),
    travel_time_minutes_num  = to_num(travel_time_minutes),
    waiting_time_minutes_num = to_num(waiting_time_minutes),
    service_time_minutes_num = to_num(service_time_minutes),
    time_hours = case_when(
      !is.na(tx_settlement_hours_num) ~ tx_settlement_hours_num,
      !is.na(tx_initiated_datetime) & !is.na(tx_received_datetime) ~
        as.numeric(difftime(tx_received_datetime, tx_initiated_datetime, units = "hours")),
      !is.na(exact_minutes_num) ~ exact_minutes_num / 60,
      !is.na(duration_minutes_num) ~ duration_minutes_num / 60,
      TRUE ~ NA_real_
    ),
    interaction_time_hours = case_when(
      !is.na(travel_time_minutes_num) |
        !is.na(waiting_time_minutes_num) |
        !is.na(service_time_minutes_num) ~
        rowSums(
          across(
            c(travel_time_minutes_num,
              waiting_time_minutes_num,
              service_time_minutes_num),
            ~ replace_na(.x, 0)
          )
        ) / 60,
      TRUE ~ NA_real_
    ),
    # Cost variables
    direct_fees = to_num(cost_total_fee_amount),
    additional_fees = to_num(cost_additional_total),
    doc_cost = to_num(doc_cost_amount),
    travel_cost = to_num(transport_cost),
    data_airtime_cost = to_num(data_airtime_cost),
    other_out_of_pocket_cost = to_num(other_out_of_pocket_cost),
    direct_fees = replace_na(direct_fees, 0),
    additional_fees = replace_na(additional_fees, 0),
    doc_cost = replace_na(doc_cost, 0),
    travel_cost = replace_na(travel_cost, 0),
    data_airtime_cost = replace_na(data_airtime_cost, 0),
    other_out_of_pocket_cost = replace_na(other_out_of_pocket_cost, 0),
    cost = direct_fees + additional_fees,
    total_cost_without_time =
      cost +
      doc_cost +
      travel_cost +
      data_airtime_cost +
      other_out_of_pocket_cost
  ) |>
  left_join(min_wage_lookup, by = "country_clean") |>
  mutate(
    time_cost = case_when(
      !is.na(time_hours) & !is.na(hourly_earnings_ppp) ~ time_hours * hourly_earnings_ppp,
      TRUE ~ NA_real_
    ),
    interaction_time_cost = case_when(
      !is.na(interaction_time_hours) & !is.na(hourly_earnings_ppp) ~
        interaction_time_hours * hourly_earnings_ppp,
      TRUE ~ NA_real_
    ),
    total_cost =
      total_cost_without_time +
      replace_na(time_cost, 0),
    total_interaction_cost =
      total_cost_without_time +
      replace_na(interaction_time_cost, 0),
    # --------------------------------------------------------------------------
    # Sanity checks: Missingness and quality flags -----------------------------
    # --------------------------------------------------------------------------
    # Transactions
    missing_channel = is.na(channel),
    missing_success = is.na(success),
    missing_kyc = is.na(kyc_score),
    missing_cost = is.na(cost),
    missing_time = is.na(time_hours),
    # Time
    flag_negative_time = !is.na(time_hours) & time_hours < 0,
    flag_negative_cost = !is.na(cost) & cost < 0,
    flag_failed_but_has_settlement_time = success == 0 & !is.na(tx_settlement_hours_num),
    flag_completed_but_no_received_confirmation =
      success == 1 & !is.na(j2_confirmed_received) & to_yesno(j2_confirmed_received) == 0,
    data_quality_flag = case_when(
      missing_channel ~ "Missing channel",
      missing_success ~ "Missing success outcome",
      flag_negative_time ~ "Negative transaction time",
      flag_negative_cost ~ "Negative cost",
      flag_completed_but_no_received_confirmation ~ "Completed but not confirmed received",
      TRUE ~ "OK"
    )
  )

# ------------------------------------------------------------------------------
# Final dataframe: ready for analysis ------------------------------------------
# ------------------------------------------------------------------------------
analysis_df <- df_clean |>
  select(
    # IDs
    submission_datetime,
    start_datetime,
    end_datetime,
    transaction_id,
    transaction_date_clean,
    transaction_time_clean,
    confederate_id,
    enumerator,
    enumerator_name,
    enumerator_id,
    transaction_country,
    country_clean,
    # -------------------
    # Treatment variables
    # -------------------
    # Channel
    institution_type,
    institution_type_num,
    institution_name,
    channel,
    MTO,
    Fintech,
    Crypto,
    # Transaction method
    transaction_method,
    transaction_method_num,
    delivery,
    Online,
    # Transaction amount
    amount,
    Amount250,
    # Outcome
    transaction_outcome,
    transaction_outcome_num,
    transaction_outcome_label,
    success,
    # KYC
    kyc_score,
    kyc_score_reported,
    kyc_score_constructed,
    documents_required,
    starts_with("documents_required_"),
    starts_with("kyc_doc_"),
    identity_document_comments,
    kyc_asked_sourcefunds,
    # Time
    tx_initiated_datetime,
    tx_received_datetime,
    tx_settlement_hours,
    tx_settlement_hours_num,
    exact_minutes_num,
    duration_minutes_num,
    time_hours,
    interaction_time_hours,
    # Cost
    direct_fees,
    additional_fees,
    cost,
    doc_cost,
    travel_cost,
    data_airtime_cost,
    other_out_of_pocket_cost,
    total_cost_without_time,
    total_cost,
    total_interaction_cost,
    time_cost,
    interaction_time_cost,
    # Flags
    data_quality_flag,
    missing_channel,
    missing_success,
    missing_kyc,
    missing_cost,
    missing_time,
    instance_id,
    formdef_version,
    key
  ) |>
  rename(
    transaction_date = transaction_date_clean,
    transaction_time = transaction_time_clean,
    country = transaction_country
  )

# ------------------------------------------------------------------------------
# Sanity checks ----------------------------------------------------------------
# ------------------------------------------------------------------------------
diagnostics <- list(
  n_rows = nrow(analysis_df),
  n_confederates = n_distinct(analysis_df$confederate_id, na.rm = TRUE),
  channel_counts = analysis_df |>
    count(channel),
  country_counts = analysis_df |>
    count(country_clean),
  method_counts = analysis_df |>
    count(delivery),
  amount_counts = analysis_df |>
    count(amount),
  outcome_counts = analysis_df |>
    count(transaction_outcome_num, transaction_outcome_label, success),
  missing_summary = analysis_df |>
    summarise(
      missing_channel = sum(is.na(channel)),
      missing_success = sum(is.na(success)),
      missing_kyc = sum(is.na(kyc_score)),
      missing_cost = sum(is.na(cost)),
      missing_time = sum(is.na(time_hours))
    ),
  quality_flags = analysis_df |>
    count(data_quality_flag),
  balance_by_channel = analysis_df |>
    group_by(channel) |>
    summarise(
      n = n(),
      n_confederates = n_distinct(confederate_id),
      n_countries = n_distinct(country_clean),
      mean_amount = mean(amount, na.rm = TRUE),
      pct_250 = 100 * mean(Amount250, na.rm = TRUE),
      pct_online = 100 * mean(Online, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      mean_kyc = mean(kyc_score, na.rm = TRUE),
      mean_cost = mean(cost, na.rm = TRUE),
      mean_time_hours = mean(time_hours, na.rm = TRUE),
      .groups = "drop"
    )
)

print(diagnostics)

# ------------------------------------------------------------------------------
# Saving the data --------------------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(analysis_df, "IADB_analysis_ready_apr23.csv")
saveRDS(analysis_df, "IADB_analysis_ready_apr23.rds")

write_csv(diagnostics$balance_by_channel, 
          "IADB_balance_by_channel_apr23.csv")
