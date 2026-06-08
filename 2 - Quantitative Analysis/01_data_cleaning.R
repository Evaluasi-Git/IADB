# ==============================================================================
# IADB - Raw SurveyCTO data cleaning -------------------------------------------
# Author: Cedric Antunes (Evaluasi)
# Date: May 11, 2026
# Revisions: June, 2026
# Minor revisions implemented on June 1st
# Objectives:
#   1. Clean raw SurveyCTO data
#   2. Construct treatment variables and outcomes
#   3. Preserve local-currency cost variables without pretending they are USD
#   4. Create transaction_uid = confederate_id + transaction_id
#   5. Create diagnostics and quality flags
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
  library(here)
})

# ------------------------------------------------------------------------------
# Replication notes ------------------------------------------------------------
# ------------------------------------------------------------------------------
# Required input:
#   - IADB_Survey_WIDE_june1.csv
#     Raw SurveyCTO export used as the input for this cleaning script.
#
# What to change before running:
#   - Update `raw_path` so it points to the local location of
#     `IADB_Survey_WIDE_june1.csv` on your computer.
#   - Update `output_dir` so it points to the folder where you want the cleaned
#     outputs from this script to be saved.
#
# Example:
#   raw_path <- "C:/Users/YourName/Drive/IADB_inputs/IADB_Survey_WIDE_june1.csv"
#   output_dir <- "C:/Users/YourName/Drive/IADB_outputs/data/clean"

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Input
raw_path <- here("IADB_Survey_WIDE_june1.csv")

# Output
output_dir <- here("data", "clean")

# Create local directory (uncomment if not needed)
dir.create(output_dir, 
           showWarnings = FALSE, 
           recursive = TRUE)

# ------------------------------------------------------------------------------
# Loading raw SurveyCTO data --------------------------------------------------- 
# ------------------------------------------------------------------------------
raw_data <- read_csv(
  raw_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  clean_names()

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Missing cols, if needed
add_missing_cols <- function(data, cols) {
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    data[missing_cols] <- NA_character_
  }
  data
}

# Text cleaning
clean_text <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  str_to_lower(x)
}

# Safe yes/no
to_yesno <- function(x) {
  x <- clean_text(x)
  case_when(
    x %in% c("1", "yes", "y", "sim", "sí", "si", "true", "t") ~ 1,
    x %in% c("0", "no", "n", "não", "false", "f") ~ 0,
    TRUE ~ NA_real_
  )
}

# Safe nummeric parsing
parse_one_number <- function(z) {
  z <- as.character(z)
  z <- str_squish(z)
  
  if (is.na(z) || z == "") return(NA_real_)
  
  comma_pos <- str_locate_all(z, ",")[[1]]
  dot_pos   <- str_locate_all(z, "\\.")[[1]]
  
  has_comma <- nrow(comma_pos) > 0
  has_dot   <- nrow(dot_pos) > 0
  
  if (has_comma && has_dot) {
    last_comma <- max(comma_pos[, 1])
    last_dot   <- max(dot_pos[, 1])
    
    if (last_comma > last_dot) {
      return(parse_number(
        z,
        locale = locale(decimal_mark = ",", grouping_mark = ".")
      ))
    } else {
      return(parse_number(
        z,
        locale = locale(decimal_mark = ".", grouping_mark = ",")
      ))
    }
  }
  
  if (has_comma && !has_dot) {
    digits_after_comma <- str_extract(z, "(?<=,)\\d+$")
    
    if (!is.na(digits_after_comma) && nchar(digits_after_comma) <= 2) {
      return(parse_number(
        z,
        locale = locale(decimal_mark = ",", grouping_mark = ".")
      ))
    } else {
      return(parse_number(
        z,
        locale = locale(decimal_mark = ".", grouping_mark = ",")
      ))
    }
  }
  
  parse_number(
    z,
    locale = locale(decimal_mark = ".", grouping_mark = ",")
  )
}

to_num <- function(x) {
  map_dbl(x, parse_one_number)
}

# Safe date 
safe_ymd_hms <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  
  parse_date_time(
    x,
    orders = c(
      "ymd HMS", "ymd HM", "ymd",
      "mdy HMS", "mdy HM", "mdy",
      "dmy HMS", "dmy HM", "dmy",
      "HMS", "HM"
    ),
    quiet = TRUE
  )
}

safe_date <- function(x) {
  as_date(safe_ymd_hms(x))
}

# Safe options
has_select_option <- function(data, base, option) {
  wide_col <- paste0(base, "_", option)
  
  if (wide_col %in% names(data)) {
    v <- clean_text(data[[wide_col]])
    
    return(case_when(
      v %in% c("1", "yes", "true", "selected", option) ~ 1,
      v %in% c("0", "no", "false", "") ~ 0,
      is.na(v) ~ 0,
      TRUE ~ 0
    ))
  }
  
  if (base %in% names(data)) {
    x <- str_squish(as.character(data[[base]]))
    x[x == ""] <- NA_character_
    
    return(case_when(
      is.na(x) ~ NA_real_,
      str_detect(
        paste0(" ", x, " "),
        fixed(paste0(" ", option, " "))
      ) ~ 1,
      TRUE ~ 0
    ))
  }
  
  rep(NA_real_, nrow(data))
}

# Safe rescale for KYC
rescale_0_5_to_0_3 <- function(x) {
  x <- to_num(x)
  case_when(
    is.na(x) ~ NA_real_,
    x < 0 | x > 5 ~ NA_real_,
    TRUE ~ (x / 5) * 3
  )
}

# ------------------------------------------------------------------------------
# Ensuring all required fields exist -------------------------------------------
# ------------------------------------------------------------------------------
expected_cols <- c(
  # Metadata
  "submission_date", "starttime", "endtime", "instance_id", "formdef_version",
  "key", "enumerator", "enumerator_name", "enumerator_id", "consent",
  
  # Header
  "transaction_id", "transaction_date", "transaction_time",
  "institution_type", "institution_name", "transaction_country",
  "beneficiary_country", "transaction_amount", "transaction_method",
  "transaction_outcome",
  
  # Confederate ID
  "confederate_id",
  
  # SurveyCTO calculated fields
  "kyc_score_0_3",
  "kyc_doc_govid", "kyc_doc_address", "kyc_doc_additional_id",
  "kyc_doc_biometrics", "kyc_doc_taxid", "kyc_doc_sourcefunds",
  "kyc_asked_sourcefunds",
  "tx_initiated_ddt", "j2_received_datetime", "tx_settlement_hours",
  "duration_minutes", "j2_confirmed_received",
  "doc_cost_incurred", "doc_cost_amount",
  
  # KYC visible fields
  "documents_required", "documents_required_other",
  "stringency_documents_request",
  "identity_document_comments",
  "b1_id_exam_score", "b1_id_checked", "b2_id_copy",
  "b3_address_check_score", "b4_database_check",
  "c1_personal_info", "c2_filled_form", "c2a_form_thoroughness_score",
  "c3_overall_info_collection_score",
  "d1_asked_purpose", "d1b_purpose_thoroughness",
  "d2_asked_source", "d2b_source_thoroughness",
  "d3_beneficiary_questions", "d4_documented_purpose_source",
  "e1_risk_assess", "e2_routine_suspicious_score",
  "e3_consult_supervisor",
  "f1_recording", "f1a_record_thoroughness",
  "f2_confirmation", "f3_posted_notices",
  "f4_compliance_culture_score",
  "g1_biometrics", "g3_tech",
  "h1_account_required", "h2_verification_steps",
  "h3_compliance_info", "h4_security_score",
  "i1_time_cat", "i1_exact_minutes",
  "i2_staff_professionalism", "i3_staff_training",
  "i4_overall_compliance", "i4_rationale",
  
  # Outcome details
  "j1a_reject_reasons", "j1b_incomplete_reasons",
  "j_comments",
  
  # Field notes
  "k1_field_notes", "k2_red_flags", "k3_strengths",
  "k4_questions_uncertainties", "k5_overall_impressions",
  
  # Completion information
  "scorecard_completed_date", "scorecard_completed_time",
  "time_elapsed_since_txn", "reviewed_by_team",
  "followup_needed", "followup_describe",
  
  # Cost form
  "complete_cost_form",
  "cost_fee_type", "cost_fixed_fee_amount", "cost_percentage_fee",
  "cost_total_fee_amount", "cost_fee_disclosed",
  "beneficiary_currency", "exchange_rate_disclosed",
  "amount_beneficiary_receives", "official_exchange_rate",
  "effective_exchange_rate",
  "cost_additional_fees", "cost_additional_total",
  "travel_time_minutes", "waiting_time_minutes", "service_time_minutes",
  "transport_mode", "transport_cost",
  "data_airtime_cost", "other_out_of_pocket_cost",
  "cost_disclosure", "cost_clarity", "cost_fairness",
  "g1_service_tiers", "g2_researched_other",
  "g2_cost_compare", "g2_lowest_cost"
)

raw_data <- raw_data |>
  add_missing_cols(expected_cols)

# ------------------------------------------------------------------------------
# Preprocessing select_multiple indicators -------------------------------------
# ------------------------------------------------------------------------------
raw_data <- raw_data |>
  mutate(
    doc_req_govid_sel       = has_select_option(raw_data, "documents_required", "0"),
    doc_req_address_sel     = has_select_option(raw_data, "documents_required", "1"),
    doc_req_add_id_sel      = has_select_option(raw_data, "documents_required", "2"),
    doc_req_biometrics_sel  = has_select_option(raw_data, "documents_required", "3"),
    doc_req_taxid_sel       = has_select_option(raw_data, "documents_required", "4"),
    doc_req_employment_sel  = has_select_option(raw_data, "documents_required", "5"),
    doc_req_sourcefunds_sel = has_select_option(raw_data, "documents_required", "6"),
    doc_req_other_sel       = has_select_option(raw_data, "documents_required", "7"),
    doc_req_none_sel        = has_select_option(raw_data, "documents_required", "8"),
    
    cost_additional_none_sel =
      has_select_option(raw_data, "cost_additional_fees", "none")
  )

# ------------------------------------------------------------------------------
# Country-level lookup for PPP time-cost calculation ---------------------------
# ------------------------------------------------------------------------------
min_wage_lookup <- tribble(
  ~country_clean, ~hourly_earnings_ppp, ~wage_year, ~wage_source_note,
  "argentina",   10.03,    2025,     "ILOSTAT average hourly earnings of employees in PPP$",
  "brazil",       8.12,    2025,     "ILOSTAT average hourly earnings of employees in PPP$",
  "chile",       12.71,    2024,     "ILOSTAT average hourly earnings of employees in PPP$",
  "colombia",     6.01,    2025,     "ILOSTAT average hourly earnings of employees in PPP$",
  "costa_rica",  NA_real_, NA_real_, "TBD: add ILOSTAT PPP hourly earnings",
  "ecuador",      7.71,    2025,     "ILOSTAT average hourly earnings of employees in PPP$",
  "el_salvador",  5.58,     2024,    "ILOSTAT average hourly earnings of employees in PPP$",
  "guatemala",   NA_real_, NA_real_, "TBD: add ILOSTAT PPP hourly earnings",
  "jamaica",     NA_real_, NA_real_, "TBD: add ILOSTAT PPP hourly earnings",
  "mexico",       5.29,    2025,     "ILOSTAT average hourly earnings of employees in PPP$",
  "nicaragua",    2.55,    2012,     "ILOSTAT average hourly earnings of employees in PPP$",
  "panama",      10.33,    2024,     "ILOSTAT average hourly earnings of employees in PPP$",
  "peru",         6.13,    2025,     "ILOSTAT average hourly earnings of employees in PPP$"
)

# ------------------------------------------------------------------------------
# Main cleaning pipeline -------------------------------------------------------
# ------------------------------------------------------------------------------
df_clean <- raw_data |>
  mutate(
    # --------------------------------------------------------------------------
    # Consent and metadata
    # --------------------------------------------------------------------------
    consent_num = to_yesno(consent),
    
    submission_datetime = safe_ymd_hms(submission_date),
    start_datetime      = safe_ymd_hms(starttime),
    end_datetime        = safe_ymd_hms(endtime),
    
    transaction_date_clean = safe_date(transaction_date),
    transaction_time_clean = as.character(transaction_time),
    
    transaction_start_datetime = safe_ymd_hms(
      str_c(as.character(transaction_date_clean), transaction_time_clean, sep = " ")
    ),
    
    scorecard_completed_date_clean = safe_date(scorecard_completed_date),
    scorecard_completed_time_clean = as.character(scorecard_completed_time),
    
    scorecard_completed_datetime = safe_ymd_hms(
      str_c(
        as.character(scorecard_completed_date_clean),
        scorecard_completed_time_clean,
        sep = " "
      )
    ),
    
    scorecard_delay_minutes = case_when(
      !is.na(transaction_start_datetime) & !is.na(scorecard_completed_datetime) ~
        as.numeric(difftime(
          scorecard_completed_datetime,
          transaction_start_datetime,
          units = "mins"
        )),
      TRUE ~ NA_real_
    ),
    
    # --------------------------------------------------------------------------
    # Country harmonization
    # --------------------------------------------------------------------------
    country_raw = clean_text(transaction_country),
    
    country_clean = case_when(
      country_raw %in% c("brazil", "brasil") ~ "brazil",
      country_raw == "argentina" ~ "argentina",
      country_raw == "peru" ~ "peru",
      country_raw == "mexico" ~ "mexico",
      country_raw == "colombia" ~ "colombia",
      country_raw == "guatemala" ~ "guatemala",
      country_raw %in% c("elsalvador", "el salvador", "el_salvador") ~ "el_salvador",
      country_raw == "jamaica" ~ "jamaica",
      country_raw == "chile" ~ "chile",
      country_raw == "ecuador" ~ "ecuador",
      country_raw == "nicaragua" ~ "nicaragua",
      country_raw %in% c("costarica", "costa rica", "costa_rica") ~ "costa_rica",
      country_raw %in% c("panama", "panamá") ~ "panama",
      TRUE ~ country_raw
    ),
    
    beneficiary_country_raw = clean_text(beneficiary_country),
    
    # --------------------------------------------------------------------------
    # Confederate identifier and transaction UID
    # --------------------------------------------------------------------------
    confederate_id = case_when(
      !is.na(confederate_id) & confederate_id != "" ~ as.character(confederate_id),
      !is.na(enumerator_id) & enumerator_id != "" ~ as.character(enumerator_id),
      !is.na(enumerator_name) & enumerator_name != "" ~ as.character(enumerator_name),
      TRUE ~ NA_character_
    ),
    
    transaction_uid = case_when(
      !is.na(confederate_id) & confederate_id != "" &
        !is.na(transaction_id) & transaction_id != "" ~
        paste(confederate_id, transaction_id, sep = "_"),
      TRUE ~ NA_character_
    ),
    
    # --------------------------------------------------------------------------
    # Treatment variables
    # --------------------------------------------------------------------------
    institution_type_num    = to_num(institution_type),
    transaction_method_num  = to_num(transaction_method),
    transaction_outcome_num = to_num(transaction_outcome),
    
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
    
    delivery = case_when(
      transaction_method_num == 0 ~ "In-person",
      transaction_method_num == 1 ~ "Online",
      TRUE ~ NA_character_
    ),
    
    Online = as.numeric(delivery == "Online"),
    
    amount = to_num(transaction_amount),
    Amount250 = as.numeric(amount == 250),
    
    # --------------------------------------------------------------------------
    # Transaction outcome
    # --------------------------------------------------------------------------
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
    
    rejected   = as.numeric(transaction_outcome_num == 1),
    incomplete = as.numeric(transaction_outcome_num == 2),
    abandoned  = as.numeric(transaction_outcome_num == 3),
    
    # --------------------------------------------------------------------------
    # KYC score
    # --------------------------------------------------------------------------
    # KYC 0-3 hierarchy:
    # 0 = no KYC/documentation observed
    # 1 = basic government ID requested
    # 2 = enhanced identity/address documentation requested
    # 3 = high-stringency verification: biometrics, source-of-funds documentation,
    #     or source-of-funds questioning.
    #
    # This mirrors the deployed SurveyCTO hidden kyc_score_0_3 calculation.
    # kyc_score_reported is primary; kyc_score_constructed is fallback only.
    kyc_score_reported_raw = to_num(kyc_score_0_3),
    
    kyc_score_reported = case_when(
      is.na(kyc_score_reported_raw) ~ NA_real_,
      kyc_score_reported_raw %in% c(0, 1, 2, 3) ~ kyc_score_reported_raw,
      TRUE ~ NA_real_
    ),
    
    kyc_doc_govid_num = coalesce(
      to_yesno(kyc_doc_govid),
      doc_req_govid_sel
    ),
    
    kyc_doc_address_num = coalesce(
      to_yesno(kyc_doc_address),
      doc_req_address_sel
    ),
    
    kyc_doc_add_id_num = coalesce(
      to_yesno(kyc_doc_additional_id),
      doc_req_add_id_sel
    ),
    
    kyc_doc_biometrics_num = coalesce(
      to_yesno(kyc_doc_biometrics),
      doc_req_biometrics_sel
    ),
    
    kyc_doc_taxid_num = coalesce(
      to_yesno(kyc_doc_taxid),
      doc_req_taxid_sel
    ),
    
    kyc_doc_employment_num = doc_req_employment_sel,
    
    kyc_doc_sourcefunds_num = coalesce(
      to_yesno(kyc_doc_sourcefunds),
      doc_req_sourcefunds_sel
    ),
    
    kyc_doc_other_num = doc_req_other_sel,
    kyc_doc_none_num  = doc_req_none_sel,
    
    kyc_source_asked_num = coalesce(
      to_yesno(kyc_asked_sourcefunds),
      to_yesno(d2_asked_source)
    ),
    
    kyc_purpose_asked_num = to_yesno(d1_asked_purpose),
    
    kyc_score_constructed = case_when(
      kyc_doc_none_num == 1 ~ 0,
      
      kyc_doc_biometrics_num == 1 |
        kyc_doc_sourcefunds_num == 1 |
        kyc_source_asked_num == 1 ~ 3,
      
      kyc_doc_address_num == 1 |
        kyc_doc_add_id_num == 1 |
        kyc_doc_taxid_num == 1 ~ 2,
      
      kyc_doc_govid_num == 1 ~ 1,
      
      is.na(kyc_doc_govid_num) &
        is.na(kyc_doc_address_num) &
        is.na(kyc_doc_add_id_num) &
        is.na(kyc_doc_biometrics_num) &
        is.na(kyc_doc_taxid_num) &
        is.na(kyc_doc_sourcefunds_num) &
        is.na(kyc_source_asked_num) ~ NA_real_,
      
      TRUE ~ 0
    ),
    
    kyc_score = coalesce(kyc_score_reported, kyc_score_constructed),
    
    # Richer 0-5 KYC/procedure measures
    kyc_document_stringency_0_5 = to_num(stringency_documents_request),
    kyc_id_exam_0_5             = to_num(b1_id_exam_score),
    kyc_address_check_0_5       = to_num(b3_address_check_score),
    kyc_info_collection_0_5     = to_num(c3_overall_info_collection_score),
    kyc_form_thoroughness_0_5   = to_num(c2a_form_thoroughness_score),
    kyc_purpose_probe_0_5       = to_num(d1b_purpose_thoroughness),
    kyc_source_probe_0_5        = to_num(d2b_source_thoroughness),
    kyc_documentation_0_5       = to_num(d4_documented_purpose_source),
    kyc_risk_suspicion_0_5      = to_num(e2_routine_suspicious_score),
    kyc_recordkeeping_0_5       = to_num(f1a_record_thoroughness),
    kyc_culture_0_5             = to_num(f4_compliance_culture_score),
    kyc_online_security_0_5     = to_num(h4_security_score),
    kyc_staff_professionalism_0_5 = to_num(i2_staff_professionalism),
    kyc_staff_training_0_5        = to_num(i3_staff_training),
    kyc_overall_0_5               = to_num(i4_overall_compliance),
    
    kyc_score_overall_rescaled_0_3 =
      rescale_0_5_to_0_3(i4_overall_compliance),
    
    # Composite KYC score:
    # computed only if at least 5 component scores are non-missing.
    kyc_component_n = rowSums(
      across(c(
        kyc_document_stringency_0_5,
        kyc_id_exam_0_5,
        kyc_address_check_0_5,
        kyc_info_collection_0_5,
        kyc_form_thoroughness_0_5,
        kyc_purpose_probe_0_5,
        kyc_source_probe_0_5,
        kyc_documentation_0_5,
        kyc_risk_suspicion_0_5,
        kyc_recordkeeping_0_5,
        kyc_culture_0_5,
        kyc_online_security_0_5,
        kyc_staff_professionalism_0_5,
        kyc_staff_training_0_5,
        kyc_overall_0_5
      ), ~ !is.na(.x)),
      na.rm = TRUE
    ),
    
    kyc_score_composite_0_5_raw = rowMeans(
      across(c(
        kyc_document_stringency_0_5,
        kyc_id_exam_0_5,
        kyc_address_check_0_5,
        kyc_info_collection_0_5,
        kyc_form_thoroughness_0_5,
        kyc_purpose_probe_0_5,
        kyc_source_probe_0_5,
        kyc_documentation_0_5,
        kyc_risk_suspicion_0_5,
        kyc_recordkeeping_0_5,
        kyc_culture_0_5,
        kyc_online_security_0_5,
        kyc_staff_professionalism_0_5,
        kyc_staff_training_0_5,
        kyc_overall_0_5
      )),
      na.rm = TRUE
    ),
    
    kyc_score_composite_0_5_raw = ifelse(
      is.nan(kyc_score_composite_0_5_raw),
      NA_real_,
      kyc_score_composite_0_5_raw
    ),
    
    kyc_score_composite_0_5 = case_when(
      kyc_component_n >= 5 ~ kyc_score_composite_0_5_raw,
      TRUE ~ NA_real_
    ),
    
    kyc_score_composite_0_3 = case_when(
      is.na(kyc_score_composite_0_5) ~ NA_real_,
      TRUE ~ (kyc_score_composite_0_5 / 5) * 3
    ),
    
    # --------------------------------------------------------------------------
    # Time variables
    # --------------------------------------------------------------------------
    tx_initiated_datetime = safe_ymd_hms(tx_initiated_ddt),
    tx_received_datetime  = safe_ymd_hms(j2_received_datetime),
    
    tx_settlement_hours_num = to_num(tx_settlement_hours),
    exact_minutes_num       = to_num(i1_exact_minutes),
    duration_minutes_num    = to_num(duration_minutes),
    
    travel_time_minutes_num  = to_num(travel_time_minutes),
    waiting_time_minutes_num = to_num(waiting_time_minutes),
    service_time_minutes_num = to_num(service_time_minutes),
    
    settlement_time_hours_raw = case_when(
      !is.na(tx_settlement_hours_num) ~ tx_settlement_hours_num,
      !is.na(tx_initiated_datetime) & !is.na(tx_received_datetime) ~
        as.numeric(difftime(
          tx_received_datetime,
          tx_initiated_datetime,
          units = "hours"
        )),
      TRUE ~ NA_real_
    ),
    
    transaction_duration_hours = case_when(
      !is.na(exact_minutes_num) ~ exact_minutes_num / 60,
      TRUE ~ NA_real_
    ),
    
    survey_duration_hours = case_when(
      !is.na(duration_minutes_num) ~ duration_minutes_num / 60,
      TRUE ~ NA_real_
    ),
    
    interaction_time_hours = case_when(
      !is.na(travel_time_minutes_num) |
        !is.na(waiting_time_minutes_num) |
        !is.na(service_time_minutes_num) ~
        rowSums(
          across(
            c(
              travel_time_minutes_num,
              waiting_time_minutes_num,
              service_time_minutes_num
            ),
            ~ replace_na(.x, 0)
          )
        ) / 60,
      TRUE ~ NA_real_
    ),
    
    time_hours = case_when(
      success == 1 ~ settlement_time_hours_raw,
      TRUE ~ NA_real_
    ),
    
    time_hours_source = case_when(
      success == 1 & !is.na(tx_settlement_hours_num) ~
        "tx_settlement_hours",
      
      success == 1 &
        is.na(tx_settlement_hours_num) &
        !is.na(tx_initiated_datetime) &
        !is.na(tx_received_datetime) ~
        "initiated_to_received_datetime",
      
      TRUE ~ NA_character_
    ),
    
    # --------------------------------------------------------------------------
    # Costs: local currency only
    # --------------------------------------------------------------------------
    cost_fee_type_clean = clean_text(cost_fee_type),
    
    direct_fees_local_raw        = to_num(cost_total_fee_amount),
    fixed_fee_local_raw          = to_num(cost_fixed_fee_amount),
    percentage_fee_raw           = to_num(cost_percentage_fee),
    additional_fees_local_raw    = to_num(cost_additional_total),
    doc_cost_local_raw           = to_num(doc_cost_amount),
    travel_cost_local_raw        = to_num(transport_cost),
    data_airtime_cost_local_raw  = to_num(data_airtime_cost),
    other_oop_cost_local_raw     = to_num(other_out_of_pocket_cost),
    
    amount_beneficiary_receives_num = to_num(amount_beneficiary_receives),
    official_exchange_rate_num      = to_num(official_exchange_rate),
    effective_exchange_rate_num     = to_num(effective_exchange_rate),
    
    cost_additional_none = cost_additional_none_sel,
    
    direct_fees_local = case_when(
      cost_fee_type_clean == "no_fee" ~ 0,
      !is.na(direct_fees_local_raw) ~ direct_fees_local_raw,
      TRUE ~ NA_real_
    ),
    
    additional_fees_local = case_when(
      cost_additional_none == 1 ~ 0,
      !is.na(additional_fees_local_raw) ~ additional_fees_local_raw,
      TRUE ~ NA_real_
    ),
    
    doc_cost_incurred_num = to_yesno(doc_cost_incurred),
    
    doc_cost_local = case_when(
      doc_cost_incurred_num == 0 ~ 0,
      doc_cost_incurred_num == 1 & !is.na(doc_cost_local_raw) ~ doc_cost_local_raw,
      is.na(doc_cost_incurred_num) & !is.na(doc_cost_local_raw) ~ doc_cost_local_raw,
      TRUE ~ NA_real_
    ),
    
    travel_cost_local = case_when(
      delivery == "Online" ~ 0,
      delivery == "In-person" & !is.na(travel_cost_local_raw) ~ travel_cost_local_raw,
      TRUE ~ NA_real_
    ),
    
    data_airtime_cost_local = case_when(
      !is.na(data_airtime_cost_local_raw) ~ data_airtime_cost_local_raw,
      TRUE ~ NA_real_
    ),
    
    other_out_of_pocket_cost_local = case_when(
      !is.na(other_oop_cost_local_raw) ~ other_oop_cost_local_raw,
      TRUE ~ NA_real_
    ),
    
    explicit_fees_local_all_attempts = case_when(
      !is.na(direct_fees_local) | !is.na(additional_fees_local) ~
        replace_na(direct_fees_local, 0) +
        replace_na(additional_fees_local, 0),
      TRUE ~ NA_real_
    ),
    
    explicit_fees_complete = case_when(
      !is.na(direct_fees_local) & !is.na(additional_fees_local) ~ 1,
      TRUE ~ 0
    ),
    
    cost_local = case_when(
      success == 1 ~ explicit_fees_local_all_attempts,
      TRUE ~ NA_real_
    ),
    
    total_cost_without_time_local_all_attempts = case_when(
      !is.na(explicit_fees_local_all_attempts) |
        !is.na(doc_cost_local) |
        !is.na(travel_cost_local) |
        !is.na(data_airtime_cost_local) |
        !is.na(other_out_of_pocket_cost_local) ~
        replace_na(explicit_fees_local_all_attempts, 0) +
        replace_na(doc_cost_local, 0) +
        replace_na(travel_cost_local, 0) +
        replace_na(data_airtime_cost_local, 0) +
        replace_na(other_out_of_pocket_cost_local, 0),
      TRUE ~ NA_real_
    ),
    
    total_cost_without_time_local = case_when(
      success == 1 ~ total_cost_without_time_local_all_attempts,
      TRUE ~ NA_real_
    )
  ) |>
  left_join(min_wage_lookup, by = "country_clean") |>
  mutate(
    # --------------------------------------------------------------------------
    # PPP time costs, kept separate from local-currency fees
    # --------------------------------------------------------------------------
    time_cost_ppp = case_when(
      !is.na(time_hours) & !is.na(hourly_earnings_ppp) ~
        time_hours * hourly_earnings_ppp,
      TRUE ~ NA_real_
    ),
    
    interaction_time_cost_ppp = case_when(
      !is.na(interaction_time_hours) & !is.na(hourly_earnings_ppp) ~
        interaction_time_hours * hourly_earnings_ppp,
      TRUE ~ NA_real_
    ),
    
    # --------------------------------------------------------------------------
    # Quality flags
    # --------------------------------------------------------------------------
    flag_no_consent = consent_num == 0,
    
    flag_missing_transaction_id = is.na(transaction_id) | transaction_id == "",
    flag_missing_confederate_id = is.na(confederate_id) | confederate_id == "",
    flag_missing_channel = is.na(channel),
    flag_missing_success = is.na(success),
    flag_missing_kyc = is.na(kyc_score),
    
    flag_completed_missing_cost_local =
      success == 1 & is.na(cost_local),
    
    flag_completed_missing_time =
      success == 1 & is.na(time_hours),
    
    flag_invalid_amount =
      !is.na(amount) & !(amount %in% c(100, 250)),
    
    flag_invalid_channel_method =
      !is.na(channel) &
      !is.na(delivery) &
      channel %in% c("Fintech", "Crypto") &
      delivery == "In-person",
    
    flag_negative_settlement_time =
      !is.na(settlement_time_hours_raw) & settlement_time_hours_raw < 0,
    
    flag_extreme_settlement_time =
      !is.na(settlement_time_hours_raw) & settlement_time_hours_raw > 168,
    
    flag_negative_transaction_duration =
      !is.na(transaction_duration_hours) & transaction_duration_hours < 0,
    
    flag_extreme_transaction_duration =
      !is.na(transaction_duration_hours) & transaction_duration_hours > 8,
    
    flag_negative_cost_local =
      !is.na(cost_local) & cost_local < 0,
    
    flag_failed_but_has_settlement_time =
      success == 0 & !is.na(settlement_time_hours_raw),
    
    j2_confirmed_received_num = to_yesno(j2_confirmed_received),
    
    flag_completed_but_no_received_confirmation =
      success == 1 &
      !is.na(j2_confirmed_received_num) &
      j2_confirmed_received_num == 0,
    
    flag_scorecard_after_30min =
      !is.na(scorecard_delay_minutes) & scorecard_delay_minutes > 30,
    
    flag_scorecard_not_same_day =
      !is.na(transaction_date_clean) &
      !is.na(scorecard_completed_date_clean) &
      transaction_date_clean != scorecard_completed_date_clean,
    
    flag_missing_wage_lookup =
      !is.na(country_clean) & is.na(hourly_earnings_ppp),
    
    flag_explicit_fees_partial =
      success == 1 &
      explicit_fees_complete == 0 &
      !is.na(explicit_fees_local_all_attempts),
    
    flag_duplicate_transaction_uid =
      !is.na(transaction_uid) &
      transaction_uid != "" &
      (
        duplicated(transaction_uid) |
          duplicated(transaction_uid, fromLast = TRUE)
      ),
    
    flag_duplicate_instance_id =
      !is.na(instance_id) &
      instance_id != "" &
      (
        duplicated(instance_id) |
          duplicated(instance_id, fromLast = TRUE)
      ),
    
    data_quality_flag = case_when(
      flag_no_consent ~ "No consent",
      
      flag_missing_transaction_id | flag_missing_confederate_id ~
        "Missing critical ID",
      
      flag_missing_channel | flag_missing_success ~
        "Missing treatment or success outcome",
      
      flag_duplicate_transaction_uid | flag_duplicate_instance_id ~
        "Duplicate ID",
      
      flag_invalid_amount | flag_invalid_channel_method ~
        "Protocol/coding inconsistency",
      
      flag_negative_settlement_time |
        flag_negative_transaction_duration |
        flag_negative_cost_local ~
        "Invalid negative value",
      
      flag_completed_but_no_received_confirmation ~
        "Completed but not confirmed received",
      
      flag_completed_missing_cost_local |
        flag_completed_missing_time |
        flag_missing_kyc ~
        "Outcome missingness",
      
      flag_scorecard_not_same_day ~
        "Late scorecard",
      
      flag_missing_wage_lookup ~
        "Missing wage lookup",
      
      TRUE ~ "OK"
    )
  ) |>
  mutate(
    flag_any_issue = if_any(starts_with("flag_"), ~ replace_na(.x, FALSE))
  )

# ------------------------------------------------------------------------------
# Final SurveyCTO-cleaned dataframe --------------------------------------------
# ------------------------------------------------------------------------------
analysis_df <- df_clean |>
  select(
    any_of(c(
      # IDs and metadata
      "submission_datetime",
      "start_datetime",
      "end_datetime",
      "transaction_id",
      "transaction_uid",
      "transaction_date_clean",
      "transaction_time_clean",
      "transaction_start_datetime",
      "scorecard_completed_date_clean",
      "scorecard_completed_time_clean",
      "scorecard_completed_datetime",
      "scorecard_delay_minutes",
      "confederate_id",
      "enumerator",
      "enumerator_name",
      "enumerator_id",
      "consent",
      "consent_num",
      "transaction_country",
      "country_raw",
      "country_clean",
      "beneficiary_country",
      "beneficiary_country_raw",
      
      # Treatment variables
      "institution_type",
      "institution_type_num",
      "institution_name",
      "channel",
      "MTO",
      "Fintech",
      "Crypto",
      "transaction_method",
      "transaction_method_num",
      "delivery",
      "Online",
      "transaction_amount",
      "amount",
      "Amount250",
      
      # Outcome variables
      "transaction_outcome",
      "transaction_outcome_num",
      "transaction_outcome_label",
      "success",
      "rejected",
      "incomplete",
      "abandoned",
      
      # KYC primary and alternatives
      "kyc_score",
      "kyc_score_reported",
      "kyc_score_constructed",
      "kyc_score_overall_rescaled_0_3",
      "kyc_score_composite_0_3",
      "kyc_score_composite_0_5",
      "kyc_score_composite_0_5_raw",
      "kyc_component_n",
      "documents_required",
      "documents_required_other",
      "identity_document_comments",
      "i4_rationale",
      
      # KYC components
      "kyc_doc_govid_num",
      "kyc_doc_address_num",
      "kyc_doc_add_id_num",
      "kyc_doc_biometrics_num",
      "kyc_doc_taxid_num",
      "kyc_doc_employment_num",
      "kyc_doc_sourcefunds_num",
      "kyc_doc_other_num",
      "kyc_doc_none_num",
      "kyc_source_asked_num",
      "kyc_purpose_asked_num",
      "kyc_document_stringency_0_5",
      "kyc_id_exam_0_5",
      "kyc_address_check_0_5",
      "kyc_info_collection_0_5",
      "kyc_form_thoroughness_0_5",
      "kyc_purpose_probe_0_5",
      "kyc_source_probe_0_5",
      "kyc_documentation_0_5",
      "kyc_risk_suspicion_0_5",
      "kyc_recordkeeping_0_5",
      "kyc_culture_0_5",
      "kyc_online_security_0_5",
      "kyc_staff_professionalism_0_5",
      "kyc_staff_training_0_5",
      "kyc_overall_0_5",
      
      # Time variables
      "tx_initiated_datetime",
      "tx_received_datetime",
      "tx_settlement_hours",
      "tx_settlement_hours_num",
      "settlement_time_hours_raw",
      "exact_minutes_num",
      "transaction_duration_hours",
      "duration_minutes_num",
      "survey_duration_hours",
      "travel_time_minutes_num",
      "waiting_time_minutes_num",
      "service_time_minutes_num",
      "interaction_time_hours",
      "time_hours",
      "time_hours_source",
      
      # Cost variables: local currency
      "cost_fee_type",
      "cost_fee_type_clean",
      "direct_fees_local",
      "additional_fees_local",
      "fixed_fee_local_raw",
      "percentage_fee_raw",
      "doc_cost_local",
      "travel_cost_local",
      "data_airtime_cost_local",
      "other_out_of_pocket_cost_local",
      "explicit_fees_local_all_attempts",
      "explicit_fees_complete",
      "cost_local",
      "total_cost_without_time_local_all_attempts",
      "total_cost_without_time_local",
      
      # FX raw fields, not yet converted
      "beneficiary_currency",
      "exchange_rate_disclosed",
      "amount_beneficiary_receives_num",
      "official_exchange_rate_num",
      "effective_exchange_rate_num",
      
      # Cost transparency and comparison
      "cost_fee_disclosed",
      "cost_disclosure",
      "cost_clarity",
      "cost_fairness",
      "g1_service_tiers",
      "g2_researched_other",
      "g2_cost_compare",
      "g2_lowest_cost",
      
      # PPP time cost, kept separate
      "hourly_earnings_ppp",
      "wage_year",
      "wage_source_note",
      "time_cost_ppp",
      "interaction_time_cost_ppp",
      
      # Outcome details and notes
      "j1a_reject_reasons",
      "j1b_incomplete_reasons",
      "j2_confirmed_received_num",
      "j_comments",
      "k1_field_notes",
      "k2_red_flags",
      "k3_strengths",
      "k4_questions_uncertainties",
      "k5_overall_impressions",
      "reviewed_by_team",
      "followup_needed",
      "followup_describe",
      
      # Quality and metadata
      "data_quality_flag",
      "instance_id",
      "formdef_version",
      "key"
    )),
    starts_with("documents_required_"),
    starts_with("cost_additional_fees_"),
    starts_with("flag_")
  ) |>
  rename(
    transaction_date = transaction_date_clean,
    transaction_time = transaction_time_clean,
    country = transaction_country
  )

# ------------------------------------------------------------------------------
# Basic assertions -------------------------------------------------------------
# ------------------------------------------------------------------------------
stopifnot(all(analysis_df$amount[!is.na(analysis_df$amount)] %in% c(100, 250)))
stopifnot(all(analysis_df$institution_type_num[!is.na(analysis_df$institution_type_num)] %in% 0:3))
stopifnot(all(analysis_df$transaction_method_num[!is.na(analysis_df$transaction_method_num)] %in% 0:1))
stopifnot(all(analysis_df$transaction_outcome_num[!is.na(analysis_df$transaction_outcome_num)] %in% 0:3))
stopifnot(all(analysis_df$kyc_score[!is.na(analysis_df$kyc_score)] %in% 0:3))

if (any(analysis_df$flag_duplicate_transaction_uid, na.rm = TRUE)) {
  warning("Duplicate transaction_uid detected. Review diagnostics$duplicate_transaction_uids.")
}

# ------------------------------------------------------------------------------
# Strict duplicate-submission audit --------------------------------------------
# ------------------------------------------------------------------------------
# Important:
#   flag_duplicate_transaction_uid is a diagnostic flag only.
#   It should not be used as an automatic exclusion rule because transaction_id is
#   manually entered and can be reused by mistake.
#
# Exclusion rule:
#   Drop only redundant rows in near-certain duplicate-submission clusters:
#   same transaction_uid, confederate, country, channel, delivery, amount,
#   outcome, normalized provider, transaction date, and transaction times within
#   one minute. Within each cluster, retain the most complete/latest row.
#
# Code revision added on June 1st by Cedric 
# ------------------------------------------------------------------------------
normalize_provider <- function(x) {
  x <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    str_detect(x, "coinbase") ~ "coinbase",
    str_detect(x, "binance") ~ "binance",
    str_detect(x, "paypal") ~ "paypal",
    str_detect(x, "wise") ~ "wise",
    str_detect(x, "western|acciones") ~ "western_union_acciones_valores",
    str_detect(x, "davivienda") ~ "davivienda",
    str_detect(x, "santander") ~ "santander",
    str_detect(x, "airpak|air pak") ~ "airpak",
    TRUE ~ str_replace_all(x, "[^a-z0-9]+", "_")
  )
}

analysis_df <- analysis_df |>
  mutate(
    audit_row_id = row_number(),
    provider_norm = normalize_provider(institution_name),
    tx_date_audit = as.Date(transaction_start_datetime),
    tx_datetime_audit = transaction_start_datetime
  )

core_duplicate_audit <- analysis_df |>
  filter(flag_duplicate_transaction_uid) |>
  group_by(
    transaction_uid,
    confederate_id,
    country_clean,
    channel,
    delivery,
    amount,
    transaction_outcome_label,
    provider_norm,
    tx_date_audit
  ) |>
  mutate(
    n_core_group = n(),
    time_span_minutes = as.numeric(
      difftime(
        max(tx_datetime_audit, na.rm = TRUE),
        min(tx_datetime_audit, na.rm = TRUE),
        units = "mins"
      )
    ),
    n_instances = n_distinct(instance_id),
    n_transaction_times = n_distinct(transaction_start_datetime),
    n_cost_values = n_distinct(total_cost_without_time_local, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(n_core_group > 1)

probable_duplicate_groups <- core_duplicate_audit |>
  filter(time_span_minutes <= 1) |>
  group_by(
    transaction_uid,
    confederate_id,
    country_clean,
    channel,
    delivery,
    amount,
    transaction_outcome_label,
    provider_norm,
    tx_date_audit
  ) |>
  mutate(
    probable_duplicate_group = cur_group_id()
  ) |>
  ungroup() |>
  select(
    audit_row_id,
    probable_duplicate_group
  )

completeness_vars <- c(
  "success",
  "kyc_score",
  "cost_local",
  "total_cost_without_time_local",
  "time_hours",
  "amount_beneficiary_receives_num",
  "j_comments",
  "k1_field_notes"
)

analysis_df <- analysis_df |>
  left_join(probable_duplicate_groups, by = "audit_row_id") |>
  mutate(
    flag_probable_duplicate_submission =
      !is.na(probable_duplicate_group),
    
    row_completeness = rowSums(
      across(
        any_of(completeness_vars),
        ~ !is.na(.x) & as.character(.x) != ""
      )
    )
  ) |>
  group_by(probable_duplicate_group) |>
  arrange(
    desc(row_completeness),
    desc(submission_datetime),
    .by_group = TRUE
  ) |>
  mutate(
    drop_probable_duplicate =
      !is.na(probable_duplicate_group) &
      row_number() > 1
  ) |>
  ungroup() |>
  arrange(audit_row_id)

duplicate_group_check <- analysis_df |>
  filter(flag_probable_duplicate_submission) |>
  group_by(probable_duplicate_group, transaction_uid) |>
  summarise(
    n_rows_in_group = n(),
    n_kept = sum(!drop_probable_duplicate),
    n_dropped = sum(drop_probable_duplicate),
    kept_audit_row_id = audit_row_id[!drop_probable_duplicate][1],
    dropped_audit_row_ids = paste(audit_row_id[drop_probable_duplicate], collapse = ", "),
    .groups = "drop"
  )

duplicate_decision_summary <- analysis_df |>
  summarise(
    n_before_duplicate_drop = n(),
    n_flag_duplicate_transaction_uid =
      sum(flag_duplicate_transaction_uid, na.rm = TRUE),
    n_probable_duplicate_rows =
      sum(flag_probable_duplicate_submission, na.rm = TRUE),
    n_rows_dropped_as_probable_duplicates =
      sum(drop_probable_duplicate, na.rm = TRUE),
    n_after_duplicate_drop =
      n() - sum(drop_probable_duplicate, na.rm = TRUE)
  )

probable_duplicates_dropped <- analysis_df |>
  filter(drop_probable_duplicate)

duplicate_transaction_uid_audit_all_flagged <- analysis_df |>
  filter(flag_duplicate_transaction_uid) |>
  arrange(
    transaction_uid,
    transaction_start_datetime,
    submission_datetime
  )

# Apply the conservative exclusion.
analysis_df <- analysis_df |>
  filter(!drop_probable_duplicate) |>
  arrange(audit_row_id)

analysis_df <- analysis_df |>
  mutate(
    data_quality_flag_final = case_when(
      flag_no_consent ~ "No consent",
      
      flag_missing_transaction_id | flag_missing_confederate_id ~
        "Missing critical ID",
      
      flag_missing_channel | flag_missing_success ~
        "Missing treatment or success outcome",
      
      flag_duplicate_instance_id ~
        "Duplicate instance ID",
      
      flag_probable_duplicate_submission ~
        "Retained representative of probable duplicate cluster",
      
      flag_duplicate_transaction_uid ~
        "Repeated transaction UID retained after audit",
      
      flag_invalid_amount | flag_invalid_channel_method ~
        "Protocol/coding inconsistency",
      
      flag_negative_settlement_time |
        flag_negative_transaction_duration |
        flag_negative_cost_local ~
        "Invalid negative value",
      
      flag_completed_but_no_received_confirmation ~
        "Completed but not confirmed received",
      
      flag_completed_missing_cost_local |
        flag_completed_missing_time |
        flag_missing_kyc ~
        "Outcome missingness",
      
      flag_scorecard_not_same_day ~
        "Late scorecard",
      
      flag_missing_wage_lookup ~
        "Missing wage lookup",
      
      TRUE ~ "OK"
    )
  )

# ------------------------------------------------------------------------------
# Diagnostics/sanity checks ----------------------------------------------------
# ------------------------------------------------------------------------------
diagnostics <- list(
  n_rows = tibble(n_rows = nrow(analysis_df)),
  
  n_confederates = tibble(
    n_confederates = n_distinct(analysis_df$confederate_id, na.rm = TRUE)
  ),
  
  channel_counts = analysis_df |>
    count(channel, sort = TRUE),
  
  country_counts = analysis_df |>
    count(country_clean, sort = TRUE),
  
  method_counts = analysis_df |>
    count(delivery, sort = TRUE),
  
  amount_counts = analysis_df |>
    count(amount, sort = TRUE),
  
  outcome_counts = analysis_df |>
    count(transaction_outcome_num, transaction_outcome_label, success, sort = TRUE),
  
  missing_summary = analysis_df |>
    summarise(
      n = n(),
      missing_transaction_id = sum(is.na(transaction_id) | transaction_id == ""),
      missing_transaction_uid = sum(is.na(transaction_uid) | transaction_uid == ""),
      missing_confederate_id = sum(is.na(confederate_id) | confederate_id == ""),
      missing_channel = sum(is.na(channel)),
      missing_success = sum(is.na(success)),
      missing_kyc = sum(is.na(kyc_score)),
      completed_missing_cost_local = sum(success == 1 & is.na(cost_local), na.rm = TRUE),
      completed_missing_time = sum(success == 1 & is.na(time_hours), na.rm = TRUE),
      missing_wage_lookup = sum(flag_missing_wage_lookup, na.rm = TRUE)
    ),
  
  quality_flags = analysis_df |>
    count(data_quality_flag_final, sort = TRUE),
  
  duplicate_transaction_uids = analysis_df |>
    filter(flag_duplicate_transaction_uid) |>
    count(transaction_uid, sort = TRUE),
  
  duplicate_decision_summary = duplicate_decision_summary,
  
  duplicate_group_check = duplicate_group_check,
  
  probable_duplicates_dropped = probable_duplicates_dropped,
  
  duplicate_transaction_uid_audit_all_flagged =
    duplicate_transaction_uid_audit_all_flagged,
  
  duplicate_instance_ids = analysis_df |>
    filter(flag_duplicate_instance_id) |>
    count(instance_id, sort = TRUE),
  
  kyc_validation = analysis_df |>
    summarise(
      n = n(),
      missing_reported = sum(is.na(kyc_score_reported)),
      missing_constructed = sum(is.na(kyc_score_constructed)),
      mismatches_reported_vs_constructed = sum(
        !is.na(kyc_score_reported) &
          !is.na(kyc_score_constructed) &
          kyc_score_reported != kyc_score_constructed
      ),
      mean_kyc_primary = mean(kyc_score, na.rm = TRUE),
      mean_kyc_reported = mean(kyc_score_reported, na.rm = TRUE),
      mean_kyc_constructed = mean(kyc_score_constructed, na.rm = TRUE),
      mean_kyc_composite_0_3 = mean(kyc_score_composite_0_3, na.rm = TRUE)
    ),
  
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
      mean_cost_local_successful = mean(cost_local, na.rm = TRUE),
      mean_total_cost_without_time_local_successful =
        mean(total_cost_without_time_local, na.rm = TRUE),
      mean_time_hours_successful = mean(time_hours, na.rm = TRUE),
      mean_transaction_duration_hours =
        mean(transaction_duration_hours, na.rm = TRUE),
      mean_interaction_time_hours =
        mean(interaction_time_hours, na.rm = TRUE),
      .groups = "drop"
    ),
  
  missing_by_channel = analysis_df |>
    group_by(channel) |>
    summarise(
      n = n(),
      missing_success = sum(is.na(success)),
      missing_kyc = sum(is.na(kyc_score)),
      completed = sum(success == 1, na.rm = TRUE),
      completed_missing_cost_local =
        sum(success == 1 & is.na(cost_local), na.rm = TRUE),
      completed_missing_time =
        sum(success == 1 & is.na(time_hours), na.rm = TRUE),
      pct_completed_missing_cost_local =
        100 * completed_missing_cost_local / pmax(completed, 1),
      pct_completed_missing_time =
        100 * completed_missing_time / pmax(completed, 1),
      .groups = "drop"
    ),
  
  time_summary = analysis_df |>
    summarise(
      n_success = sum(success == 1, na.rm = TRUE),
      mean_settlement_time_hours = mean(time_hours, na.rm = TRUE),
      median_settlement_time_hours = median(time_hours, na.rm = TRUE),
      max_settlement_time_hours = max(time_hours, na.rm = TRUE),
      mean_transaction_duration_hours =
        mean(transaction_duration_hours, na.rm = TRUE),
      mean_interaction_time_hours =
        mean(interaction_time_hours, na.rm = TRUE)
    ),
  
  local_cost_summary = analysis_df |>
    summarise(
      n_success = sum(success == 1, na.rm = TRUE),
      mean_cost_local = mean(cost_local, na.rm = TRUE),
      median_cost_local = median(cost_local, na.rm = TRUE),
      max_cost_local = max(cost_local, na.rm = TRUE),
      mean_total_cost_without_time_local =
        mean(total_cost_without_time_local, na.rm = TRUE),
      pct_explicit_fee_partial =
        100 * mean(flag_explicit_fees_partial, na.rm = TRUE)
    ),
  
  unmatched_countries_for_wage = analysis_df |>
    filter(flag_missing_wage_lookup) |>
    distinct(country_clean) |>
    arrange(country_clean)
)

# ------------------------------------------------------------------------------
# 12. Print diagnostics ---------------------------------------------------------
# ------------------------------------------------------------------------------

cat("\n=== IADB SurveyCTO cleaning complete ===\n")
cat("Rows:", nrow(analysis_df), "\n")
cat("Confederates:", n_distinct(analysis_df$confederate_id, na.rm = TRUE), "\n\n")

cat("=== Channel counts ===\n")
print(diagnostics$channel_counts)

cat("\n=== Outcome counts ===\n")
print(diagnostics$outcome_counts)

cat("\n=== Missing summary ===\n")
print(diagnostics$missing_summary)

cat("\n=== Quality flags ===\n")
print(diagnostics$quality_flags)

cat("\n=== KYC validation ===\n")
print(diagnostics$kyc_validation)

cat("\n=== Balance by channel ===\n")
print(diagnostics$balance_by_channel)

# ------------------------------------------------------------------------------
# Saving outputs ---------------------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(
  analysis_df,
  file.path(output_dir, "IADB_surveycto_clean_june1.csv")
)

saveRDS(
  analysis_df,
  file.path(output_dir, "IADB_surveycto_clean_june1.rds")
)

saveRDS(
  diagnostics,
  file.path(output_dir, "IADB_surveycto_clean_diagnostics_june1.rds")
)

write_csv(
  diagnostics$balance_by_channel,
  file.path(output_dir, "IADB_surveycto_balance_by_channel_june1.csv")
)

#write_csv(
#  diagnostics$missing_by_channel,
#  file.path(output_dir, "IADB_surveycto_missing_by_channel_june1.csv")
#)

write_csv(
  diagnostics$quality_flags,
  file.path(output_dir, "IADB_surveycto_quality_flags_june1.csv")
)

write_csv(
  diagnostics$kyc_validation,
  file.path(output_dir, "IADB_surveycto_kyc_validation_june1.csv")
)
