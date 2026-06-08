# ==============================================================================
# IADB - 07 Cost, FX, and Time Outcomes ----------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: May 2026 ---------------------------------------------------------------
# Revisions: June, 2026
# Purpose:
#   1. Convert local-currency cost outcomes to USD using daily FX rates;
#   2. Preserve reported time, transaction duration, and interaction-time outcomes;
#   3. Construct optional time-cost measures using hourly earnings/wages;
#   4. Build cost/time samples parallel to Script 05 samples;
#   5. Run first-pass cost/time SAP models and sensitivity checks.
#
# Inputs:
#   data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto.rds
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
#   data/manual/IADB_fx_rates_daily.csv
#
# Optional manual input:
#   data/manual/IADB_hourly_wage_lookup.csv
#
# Expected FX columns:
#   currency
#   fx_date
#   fx_rate_local_per_usd
#   fx_source
#
# Interpretation:
#   fx_rate_local_per_usd = units of local currency per 1 USD.
#   Example: BRL/USD = 5.72 means 1 USD = 5.72 BRL.
#
# Main monetary outcomes:
#   total_cost_without_time_usd, all attempted transactions with observed cost
#   total_cost_without_time_usd, successful transactions only
#
# Main time outcomes:
#   reported_time_hours           = cleaned time_hours from SurveyCTO
#   transaction_duration_hours    = transaction execution duration, if available
#   interaction_time_hours        = travel + waiting + service time, if available
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
  library(fixest)
})

# ------------------------------------------------------------------------------
# Replication notes ------------------------------------------------------------
# ------------------------------------------------------------------------------
# Required inputs:
#   These files must already exist from earlier scripts:
#   - IADB_sap_observed_first_pass.rds
#   - IADB_sap_per_protocol.rds
#   - IADB_sap_reviewed_submissions.rds
#   - IADB_fx_rates_daily.csv
#
#   Optional input:
#   - IADB_hourly_wage_lookup.csv
#     If this file is not available, the script uses the built-in wage lookup.
#
# What to change before running:
#   - Update `output_dir` so it points to the local folder where the SAP
#     dataset-builder outputs from Scripts 03 to 05 are stored.
#   - Update `manual_dir` so it points to the local folder where
#     `IADB_fx_rates_daily.csv` is stored.
#   - `results_dir` is created automatically inside `output_dir` and stores the
#     cost/time model outputs from this script.
#
# Example:
#   output_root <- "C:/Users/YourName/Drive/IADB_outputs"
#
#   output_dir <- file.path(output_root, "data", "clean", "sap_dataset_builder")
#   manual_dir <- file.path(output_root, "data", "manual")
#   results_dir <- file.path(output_dir, "sap_results_cost_fx_time")

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
output_dir <- here(
  "data",
  "clean",
  "sap_dataset_builder"
)

results_dir <- file.path(
  output_dir,
  "sap_results_cost_fx_time"
)

manual_dir <- here("data", 
                   "manual")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(manual_dir, showWarnings = FALSE, recursive = TRUE)

sap_main_path <- file.path(
  output_dir,
  "IADB_sap_observed_first_pass.rds"
)

sap_pp_path <- file.path(
  output_dir,
  "IADB_sap_per_protocol.rds"
)

sap_reviewed_path <- file.path(
  output_dir,
  "IADB_sap_reviewed_submissions.rds"
)

fx_rates_path <- file.path(
  manual_dir,
  "IADB_fx_rates_daily.csv"
)

wage_lookup_path <- file.path(
  manual_dir,
  "IADB_hourly_wage_lookup.csv"
)

# ------------------------------------------------------------------------------
# Input checks -----------------------------------------------------------------
# ------------------------------------------------------------------------------
required_input_files <- c(
  sap_main_path,
  sap_pp_path,
  sap_reviewed_path,
  fx_rates_path
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste(missing_input_files, collapse = "\n")
  )
}

# ------------------------------------------------------------------------------
# Parameters -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# 06 now creates a complete FX table, so missing FX should stop the script!
ALLOW_MISSING_FX <- FALSE

# Extreme-value flags only. These do not automatically exclude observations.
EXTREME_TOTAL_COST_USD <- 250
EXTREME_REPORTED_TIME_HOURS <- 168
EXTREME_INTERACTION_TIME_HOURS <- 12
EXTREME_TRANSACTION_DURATION_HOURS <- 8

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Safe missing columns 
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
  suppressWarnings(readr::parse_number(as.character(x)))
}

# Safe date
safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") | inherits(x, "POSIXt")) return(as.Date(x))
  
  suppressWarnings(
    as.Date(
      parse_date_time(
        as.character(x),
        orders = c(
          "ymd HMS", "ymd HM", "ymd",
          "mdy HMS", "mdy HM", "mdy",
          "dmy HMS", "dmy HM", "dmy"
        ),
        quiet = TRUE
      )
    )
  )
}

safe_datetime <- function(x) {
  if (inherits(x, "POSIXct") | inherits(x, "POSIXt")) return(x)
  
  suppressWarnings(
    parse_date_time(
      as.character(x),
      orders = c(
        "ymd HMS", "ymd HM", "ymd",
        "mdy HMS", "mdy HM", "mdy",
        "dmy HMS", "dmy HM", "dmy"
      ),
      quiet = TRUE
    )
  )
}

# Safe logical
as_logical_safe <- function(x) {
  if (is.logical(x)) {
    return(replace_na(x, FALSE))
  }
  
  x_clean <- str_to_lower(str_squish(as.character(x)))
  
  case_when(
    x_clean %in% c("true", "t", "1", "yes", "sim", "si") ~ TRUE,
    x_clean %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ FALSE
  )
}

# Harmonizing
standardize_country <- function(x) {
  x_clean <- as.character(x) |>
    str_to_lower() |>
    str_squish() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_+|_+$", "")
  
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

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[[1]]
}

# USD rates
extract_fx_dates <- function(df, sample_source_name) {
  df |>
    clean_names() |>
    add_missing_cols(c(
      "transaction_date",
      "submission_datetime",
      "assigned_date"
    )) |>
    transmute(
      sample_source = sample_source_name,
      transaction_date = safe_date(transaction_date),
      submission_date = safe_date(submission_datetime),
      assigned_date = safe_date(assigned_date),
      fx_date_candidate = coalesce(
        transaction_date,
        submission_date,
        assigned_date
      )
    )
}

# Checking key predictors for specifications
prep_model_data <- function(df, outcome) {
  df |>
    filter(
      !is.na(.data[[outcome]]),
      !is.na(confederate_match_key),
      !is.na(MTO),
      !is.na(Fintech),
      !is.na(Crypto),
      !is.na(Amount250),
      !is.na(Online)
    )
}

# Safe fixed effects specification
safe_feols <- function(formula, data, vcov = ~ confederate_match_key) {
  outcome_name <- all.vars(formula)[1]
  y <- data[[outcome_name]]
  
  if (nrow(data) == 0) {
    message("Skipping model because data has zero rows: ", outcome_name)
    return(NULL)
  }
  
  if (length(unique(na.omit(y))) <= 1) {
    message("Skipping model because outcome is constant or all missing: ", outcome_name)
    return(NULL)
  }
  
  tryCatch(
    feols(
      formula,
      data = data,
      vcov = vcov
    ),
    error = function(e) {
      message("Skipping model due to estimation error: ", outcome_name)
      message(e$message)
      NULL
    }
  )
}

estimate_model_set <- function(outcome, samples_named, prefix) {
  fml <- as.formula(
    paste0(
      outcome,
      " ~ MTO + Fintech + Crypto + Amount250 + Online | confederate_match_key"
    )
  )
  
  out <- purrr::imap(
    samples_named,
    function(df, sample_label) {
      df_model <- prep_model_data(df, outcome)
      safe_feols(fml, data = df_model)
    }
  )
  
  names(out) <- paste0("m_", prefix, "_", names(samples_named))
  
  out
}

write_etable_group <- function(models, group_name, results_dir) {
  models_nonnull <- models[
    !vapply(models, is.null, logical(1))
  ]
  
  if (length(models_nonnull) == 0) {
    message("No non-null models for group: ", group_name)
    return(invisible(NULL))
  }
  
  do.call(
    etable,
    c(
      models_nonnull,
      list(
        tex = FALSE,
        file = file.path(
          results_dir,
          paste0("IADB_06_", group_name, "_models.txt")
        )
      )
    )
  )
  
  do.call(
    etable,
    c(
      models_nonnull,
      list(
        tex = TRUE,
        file = file.path(
          results_dir,
          paste0("IADB_06_", group_name, "_models.tex")
        )
      )
    )
  )
  
  cat("\n=== ", group_name, " models ===\n", sep = "")
  print(
    do.call(
      etable,
      c(
        models_nonnull,
        list(tex = FALSE)
      )
    )
  )
  
  invisible(models_nonnull)
}

# ------------------------------------------------------------------------------
# Country-currency lookup ------------------------------------------------------
# ------------------------------------------------------------------------------
country_currency_lookup <- tribble(
  ~country_clean,  ~local_currency,
  "argentina",     "ARS",
  "brazil",        "BRL",
  "chile",         "CLP",
  "colombia",      "COP",
  "costa_rica",    "CRC",
  "ecuador",       "USD",
  "el_salvador",   "USD",
  "guatemala",     "GTQ",
  "jamaica",       "JMD",
  "mexico",        "MXN",
  "nicaragua",     "NIO",
  "panama",        "USD",
  "peru",          "PEN"
)

write_csv(
  country_currency_lookup,
  file.path(results_dir, "IADB_06_country_currency_lookup.csv")
)

# ------------------------------------------------------------------------------
# Wage / hourly earnings lookup ------------------------------------------------
# ------------------------------------------------------------------------------
# ILOSTAT data
default_wage_lookup <- tribble(
  ~country_clean,  ~hourly_earnings_ppp, ~hourly_wage_usd, ~wage_year, ~wage_source_note,
  "argentina",     10.03,                NA_real_,         2015,       "ILOSTAT average hourly earnings of employees in PPP$",
  "brazil",         8.12,                NA_real_,         2025,       "ILOSTAT average hourly earnings of employees in PPP$",
  "chile",         12.71,                NA_real_,         2024,       "ILOSTAT average hourly earnings of employees in PPP$",
  "colombia",       6.01,                NA_real_,         2025,       "ILOSTAT average hourly earnings of employees in PPP$",
  "costa_rica",    NA_real_,             NA_real_,         NA_real_,   "TBD: add ILOSTAT PPP hourly earnings or manual wage",
  "ecuador",        7.71,                NA_real_,         2025,       "ILOSTAT average hourly earnings of employees in PPP$",
  "el_salvador",   5.58,                 NA_real_,         NA_real_,   "ILOSTAT average hourly earnings of employees in PPP$",
  "guatemala",     NA_real_,             NA_real_,         NA_real_,   "TBD: add ILOSTAT PPP hourly earnings or manual wage",
  "jamaica",       NA_real_,             NA_real_,         NA_real_,   "TBD: add ILOSTAT PPP hourly earnings or manual wage",
  "mexico",         5.29,                NA_real_,         2025,       "ILOSTAT average hourly earnings of employees in PPP$",
  "nicaragua",      2.55,                NA_real_,         2012,       "ILOSTAT average hourly earnings of employees in PPP$",
  "panama",        10.33,                NA_real_,         2024,       "ILOSTAT average hourly earnings of employees in PPP$",
  "peru",           6.13,                NA_real_,         2025,       "ILOSTAT average hourly earnings of employees in PPP$"
)

if (file.exists(wage_lookup_path)) {
  wage_manual <- read_csv(
    wage_lookup_path,
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) |>
    clean_names() |>
    add_missing_cols(c(
      "country_clean",
      "hourly_earnings_ppp",
      "hourly_wage_usd",
      "wage_year",
      "wage_source_note"
    )) |>
    transmute(
      country_clean = standardize_country(country_clean),
      hourly_earnings_ppp = to_num(hourly_earnings_ppp),
      hourly_wage_usd = to_num(hourly_wage_usd),
      wage_year = to_num(wage_year),
      wage_source_note = as.character(wage_source_note)
    )
  
  wage_lookup <- bind_rows(
    wage_manual |> mutate(source_priority = 1),
    default_wage_lookup |> mutate(source_priority = 2)
  ) |>
    arrange(country_clean, source_priority) |>
    group_by(country_clean) |>
    summarise(
      hourly_earnings_ppp = first_nonmissing(hourly_earnings_ppp),
      hourly_wage_usd = first_nonmissing(hourly_wage_usd),
      wage_year = first_nonmissing(wage_year),
      wage_source_note = first_nonmissing(wage_source_note),
      .groups = "drop"
    )
} else {
  wage_lookup <- default_wage_lookup
}

write_csv(
  wage_lookup,
  file.path(results_dir, "IADB_06_wage_lookup_used.csv")
)

# ------------------------------------------------------------------------------
# Loading SAP samples ----------------------------------------------------------
# ------------------------------------------------------------------------------
sap_main <- readRDS(sap_main_path) |>
  clean_names()

sap_pp <- readRDS(sap_pp_path) |>
  clean_names()

sap_reviewed <- readRDS(sap_reviewed_path) |>
  clean_names()

# ------------------------------------------------------------------------------
# 9. Determine date range needed for FX -----------------------------------------
# ------------------------------------------------------------------------------
all_sample_dates <- bind_rows(
  extract_fx_dates(sap_main, "main_strict_slot_level"),
  extract_fx_dates(sap_pp, "per_protocol_strict_slot"),
  extract_fx_dates(sap_reviewed, "reviewed_submissions")
) |>
  filter(!is.na(fx_date_candidate)) |>
  pull(fx_date_candidate)

date_min <- min(all_sample_dates, na.rm = TRUE)
date_max <- max(all_sample_dates, na.rm = TRUE)

if (is.infinite(as.numeric(date_min)) | is.infinite(as.numeric(date_max))) {
  stop("Could not determine valid FX date range from SAP samples.")
}

cat("\nFX date range needed:\n")
cat(as.character(date_min), "to", as.character(date_max), "\n")

# ------------------------------------------------------------------------------
# Loading FX table -------------------------------------------------------------
# ------------------------------------------------------------------------------
fx_input <- read_csv(
  fx_rates_path,
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) |>
  clean_names() |>
  add_missing_cols(c(
    "currency",
    "fx_date",
    "fx_rate_local_per_usd",
    "fx_source"
  )) |>
  transmute(
    currency = str_to_upper(str_squish(as.character(currency))),
    fx_date = safe_date(fx_date),
    fx_rate_local_per_usd = to_num(fx_rate_local_per_usd),
    fx_source = as.character(fx_source)
  ) |>
  filter(
    !is.na(currency),
    !is.na(fx_date),
    !is.na(fx_rate_local_per_usd),
    fx_rate_local_per_usd > 0
  )

usd_fx <- tibble(
  currency = "USD",
  fx_date = seq.Date(date_min, date_max, by = "day"),
  fx_rate_local_per_usd = 1,
  fx_source = "USD_identity"
)

fx_raw <- bind_rows(
  fx_input,
  usd_fx
) |>
  arrange(currency, fx_date) |>
  group_by(currency, fx_date) |>
  slice(1) |>
  ungroup()

fx_skeleton <- tidyr::expand_grid(
  currency = sort(unique(c(fx_raw$currency, "USD"))),
  fx_date = seq.Date(date_min, date_max, by = "day")
)

fx_daily <- fx_skeleton |>
  left_join(
    fx_raw,
    by = c("currency", "fx_date")
  ) |>
  arrange(currency, fx_date) |>
  group_by(currency) |>
  tidyr::fill(
    fx_rate_local_per_usd,
    fx_source,
    .direction = "down"
  ) |>
  ungroup()

write_csv(
  fx_daily,
  file.path(results_dir, "IADB_06_fx_daily_completed.csv")
)

fx_coverage_summary <- fx_daily |>
  group_by(currency) |>
  summarise(
    first_fx_date = if (all(is.na(fx_rate_local_per_usd))) {
      as.Date(NA)
    } else {
      min(fx_date[!is.na(fx_rate_local_per_usd)])
    },
    last_fx_date = if (all(is.na(fx_rate_local_per_usd))) {
      as.Date(NA)
    } else {
      max(fx_date[!is.na(fx_rate_local_per_usd)])
    },
    n_daily_rows = n(),
    n_missing_fx_rate = sum(is.na(fx_rate_local_per_usd)),
    sources = paste(sort(unique(na.omit(fx_source))), collapse = "; "),
    .groups = "drop"
  )

write_csv(
  fx_coverage_summary,
  file.path(results_dir, "IADB_06_fx_coverage_summary.csv")
)

# ------------------------------------------------------------------------------
# Prepare cost/time sample function --------------------------------------------
# ------------------------------------------------------------------------------
prepare_cost_time_sample <- function(df, sample_name) {
  needed_cols <- c(
    "unique_transaction_id",
    "confederate_match_key",
    "country",
    "country_clean",
    "country_schedule_clean",
    "assigned_channel",
    "assigned_amount",
    "assigned_delivery",
    "channel_std",
    "amount",
    "delivery_std",
    "transaction_date",
    "submission_datetime",
    "assigned_date",
    "success",
    "cost_local",
    "total_cost_without_time_local",
    "time_hours",
    "transaction_duration_hours",
    "interaction_time_hours",
    "needs_manual_review_for_final",
    "protocol_deviation"
  )
  
  df |>
    add_missing_cols(needed_cols) |>
    select(
      -any_of(c(
        "local_currency",
        "fx_rate_local_per_usd",
        "fx_source",
        "hourly_earnings_ppp",
        "hourly_wage_usd",
        "wage_year",
        "wage_source_note"
      ))
    ) |>
    mutate(
      sample_name = sample_name,
      
      country_clean_final = standardize_country(
        coalesce(
          as.character(country),
          as.character(country_clean),
          as.character(country_schedule_clean)
        )
      ),
      
      transaction_date = safe_date(transaction_date),
      submission_datetime = safe_datetime(submission_datetime),
      submission_date = safe_date(submission_datetime),
      assigned_date = safe_date(assigned_date),
      
      fx_date = coalesce(
        transaction_date,
        submission_date,
        assigned_date
      ),
      
      success = to_num(success),
      assigned_amount = to_num(assigned_amount),
      amount = to_num(amount),
      cost_local = to_num(cost_local),
      total_cost_without_time_local = to_num(total_cost_without_time_local),
      reported_time_hours = to_num(time_hours),
      transaction_duration_hours = to_num(transaction_duration_hours),
      interaction_time_hours = to_num(interaction_time_hours),
      
      needs_manual_review_for_final =
        as_logical_safe(needs_manual_review_for_final),
      
      protocol_deviation =
        as_logical_safe(protocol_deviation)
    ) |>
    left_join(
      country_currency_lookup,
      by = c("country_clean_final" = "country_clean")
    ) |>
    left_join(
      fx_daily,
      by = c(
        "local_currency" = "currency",
        "fx_date" = "fx_date"
      )
    ) |>
    left_join(
      wage_lookup,
      by = c("country_clean_final" = "country_clean")
    ) |>
    mutate(
      cost_usd = case_when(
        !is.na(cost_local) & !is.na(fx_rate_local_per_usd) ~
          cost_local / fx_rate_local_per_usd,
        TRUE ~ NA_real_
      ),
      
      total_cost_without_time_usd = case_when(
        !is.na(total_cost_without_time_local) &
          !is.na(fx_rate_local_per_usd) ~
          total_cost_without_time_local / fx_rate_local_per_usd,
        TRUE ~ NA_real_
      ),
      
      log1p_cost_usd = log1p(cost_usd),
      
      log1p_total_cost_without_time_usd =
        log1p(total_cost_without_time_usd),
      
      time_cost_ppp = case_when(
        !is.na(reported_time_hours) & !is.na(hourly_earnings_ppp) ~
          reported_time_hours * hourly_earnings_ppp,
        TRUE ~ NA_real_
      ),
      
      interaction_time_cost_ppp = case_when(
        !is.na(interaction_time_hours) & !is.na(hourly_earnings_ppp) ~
          interaction_time_hours * hourly_earnings_ppp,
        TRUE ~ NA_real_
      ),
      
      time_cost_usd = case_when(
        !is.na(reported_time_hours) & !is.na(hourly_wage_usd) ~
          reported_time_hours * hourly_wage_usd,
        TRUE ~ NA_real_
      ),
      
      interaction_time_cost_usd = case_when(
        !is.na(interaction_time_hours) & !is.na(hourly_wage_usd) ~
          interaction_time_hours * hourly_wage_usd,
        TRUE ~ NA_real_
      ),
      
      total_cost_with_reported_time_usd = case_when(
        !is.na(total_cost_without_time_usd) &
          !is.na(time_cost_usd) ~
          total_cost_without_time_usd + time_cost_usd,
        TRUE ~ NA_real_
      ),
      
      total_cost_with_interaction_time_usd = case_when(
        !is.na(total_cost_without_time_usd) &
          !is.na(interaction_time_cost_usd) ~
          total_cost_without_time_usd + interaction_time_cost_usd,
        TRUE ~ NA_real_
      ),
      
      assigned_channel = factor(
        assigned_channel,
        levels = c("Banks", "MTOs", "Fintech", "Crypto")
      ),
      
      assigned_delivery = factor(
        assigned_delivery,
        levels = c("In-person", "Online")
      ),
      
      MTO = as.numeric(assigned_channel == "MTOs"),
      Fintech = as.numeric(assigned_channel == "Fintech"),
      Crypto = as.numeric(assigned_channel == "Crypto"),
      Amount250 = as.numeric(assigned_amount == 250),
      Online = as.numeric(assigned_delivery == "Online"),
      
      sample_cost_usd_any_attempt =
        !is.na(total_cost_without_time_usd),
      
      sample_cost_usd_success_only =
        success == 1 & !is.na(total_cost_without_time_usd),
      
      sample_fee_cost_usd_any_attempt =
        !is.na(cost_usd),
      
      sample_fee_cost_usd_success_only =
        success == 1 & !is.na(cost_usd),
      
      sample_reported_time =
        !is.na(reported_time_hours),
      
      sample_reported_time_success_only =
        success == 1 & !is.na(reported_time_hours),
      
      sample_transaction_duration =
        !is.na(transaction_duration_hours),
      
      sample_interaction_time =
        !is.na(interaction_time_hours),
      
      sample_time_cost_ppp =
        !is.na(time_cost_ppp),
      
      sample_time_cost_usd =
        !is.na(time_cost_usd),
      
      sample_total_cost_with_time_usd =
        !is.na(total_cost_with_reported_time_usd),
      
      flag_missing_currency =
        is.na(local_currency),
      
      flag_missing_fx =
        !is.na(total_cost_without_time_local) &
        is.na(fx_rate_local_per_usd),
      
      flag_negative_cost_usd =
        !is.na(total_cost_without_time_usd) &
        total_cost_without_time_usd < 0,
      
      flag_extreme_total_cost_usd =
        !is.na(total_cost_without_time_usd) &
        total_cost_without_time_usd > EXTREME_TOTAL_COST_USD,
      
      flag_negative_reported_time =
        !is.na(reported_time_hours) &
        reported_time_hours < 0,
      
      flag_extreme_reported_time =
        !is.na(reported_time_hours) &
        reported_time_hours > EXTREME_REPORTED_TIME_HOURS,
      
      flag_negative_interaction_time =
        !is.na(interaction_time_hours) &
        interaction_time_hours < 0,
      
      flag_extreme_interaction_time =
        !is.na(interaction_time_hours) &
        interaction_time_hours > EXTREME_INTERACTION_TIME_HOURS,
      
      flag_negative_transaction_duration =
        !is.na(transaction_duration_hours) &
        transaction_duration_hours < 0,
      
      flag_extreme_transaction_duration =
        !is.na(transaction_duration_hours) &
        transaction_duration_hours > EXTREME_TRANSACTION_DURATION_HOURS
    )
}

# ------------------------------------------------------------------------------
# Building cost/time datasets --------------------------------------------------
# ------------------------------------------------------------------------------
sap_main_ct <- prepare_cost_time_sample(
  sap_main,
  "main_strict_slot_level"
)

sap_pp_ct <- prepare_cost_time_sample(
  sap_pp,
  "per_protocol_strict_slot"
)

sap_reviewed_ct <- prepare_cost_time_sample(
  sap_reviewed,
  "reviewed_submissions"
)

# ------------------------------------------------------------------------------
# Sanity/Safety checks ---------------------------------------------------------
# ------------------------------------------------------------------------------
# Strict slot-level samples must have one row per transaction slot.
stopifnot(nrow(sap_main_ct) == n_distinct(sap_main_ct$unique_transaction_id))
stopifnot(nrow(sap_pp_ct) == n_distinct(sap_pp_ct$unique_transaction_id))

# Reviewed-submissions intentionally preserves duplicate-slot extras.
stopifnot(nrow(sap_reviewed_ct) >= n_distinct(sap_reviewed_ct$unique_transaction_id))

stopifnot(sum(is.na(sap_main_ct$assigned_channel)) == 0)
stopifnot(sum(is.na(sap_main_ct$assigned_amount)) == 0)
stopifnot(sum(is.na(sap_main_ct$assigned_delivery)) == 0)

# ------------------------------------------------------------------------------
# Diagnostics ------------------------------------------------------------------
# ------------------------------------------------------------------------------
diagnostic_cols <- c(
  "sample_name",
  "unique_transaction_id",
  "confederate_match_key",
  "country_clean_final",
  "success",
  
  "sample_cost_usd_any_attempt",
  "sample_cost_usd_success_only",
  "sample_fee_cost_usd_any_attempt",
  "sample_fee_cost_usd_success_only",
  "sample_reported_time",
  "sample_reported_time_success_only",
  "sample_transaction_duration",
  "sample_interaction_time",
  "sample_time_cost_ppp",
  "sample_time_cost_usd",
  "sample_total_cost_with_time_usd",
  
  "flag_missing_currency",
  "flag_missing_fx",
  "flag_negative_cost_usd",
  "flag_extreme_total_cost_usd",
  "flag_negative_reported_time",
  "flag_extreme_reported_time",
  "flag_negative_interaction_time",
  "flag_extreme_interaction_time",
  "flag_negative_transaction_duration",
  "flag_extreme_transaction_duration",
  
  "total_cost_without_time_usd",
  "reported_time_hours",
  "interaction_time_hours"
)

cost_time_all_samples <- bind_rows(
  sap_main_ct |> select(any_of(diagnostic_cols)),
  sap_pp_ct |> select(any_of(diagnostic_cols)),
  sap_reviewed_ct |> select(any_of(diagnostic_cols))
)

cost_time_sample_summary <- cost_time_all_samples |>
  group_by(sample_name) |>
  summarise(
    n = n(),
    n_unique_transactions = n_distinct(unique_transaction_id),
    n_confederates = n_distinct(confederate_match_key),
    n_countries = n_distinct(country_clean_final),
    
    n_success = sum(success == 1, na.rm = TRUE),
    
    n_cost_usd_any_attempt =
      sum(sample_cost_usd_any_attempt, na.rm = TRUE),
    
    n_cost_usd_success_only =
      sum(sample_cost_usd_success_only, na.rm = TRUE),
    
    n_fee_cost_usd_any_attempt =
      sum(sample_fee_cost_usd_any_attempt, na.rm = TRUE),
    
    n_fee_cost_usd_success_only =
      sum(sample_fee_cost_usd_success_only, na.rm = TRUE),
    
    n_reported_time =
      sum(sample_reported_time, na.rm = TRUE),
    
    n_reported_time_success_only =
      sum(sample_reported_time_success_only, na.rm = TRUE),
    
    n_transaction_duration =
      sum(sample_transaction_duration, na.rm = TRUE),
    
    n_interaction_time =
      sum(sample_interaction_time, na.rm = TRUE),
    
    n_time_cost_ppp =
      sum(sample_time_cost_ppp, na.rm = TRUE),
    
    n_time_cost_usd =
      sum(sample_time_cost_usd, na.rm = TRUE),
    
    n_total_cost_with_time_usd =
      sum(sample_total_cost_with_time_usd, na.rm = TRUE),
    
    missing_currency = sum(flag_missing_currency, na.rm = TRUE),
    missing_fx = sum(flag_missing_fx, na.rm = TRUE),
    negative_cost_usd = sum(flag_negative_cost_usd, na.rm = TRUE),
    extreme_total_cost_usd = sum(flag_extreme_total_cost_usd, na.rm = TRUE),
    negative_reported_time = sum(flag_negative_reported_time, na.rm = TRUE),
    extreme_reported_time = sum(flag_extreme_reported_time, na.rm = TRUE),
    extreme_interaction_time = sum(flag_extreme_interaction_time, na.rm = TRUE),
    negative_transaction_duration =
      sum(flag_negative_transaction_duration, na.rm = TRUE),
    extreme_transaction_duration =
      sum(flag_extreme_transaction_duration, na.rm = TRUE),
    
    mean_total_cost_without_time_usd =
      mean(total_cost_without_time_usd, na.rm = TRUE),
    
    median_total_cost_without_time_usd =
      median(total_cost_without_time_usd, na.rm = TRUE),
    
    mean_reported_time_hours =
      mean(reported_time_hours, na.rm = TRUE),
    
    median_reported_time_hours =
      median(reported_time_hours, na.rm = TRUE),
    
    mean_interaction_time_hours =
      mean(interaction_time_hours, na.rm = TRUE),
    
    median_interaction_time_hours =
      median(interaction_time_hours, na.rm = TRUE),
    
    .groups = "drop"
  )

fx_missing_review <- sap_main_ct |>
  filter(flag_missing_currency | flag_missing_fx) |>
  select(
    unique_transaction_id,
    confederate_match_key,
    country_clean_final,
    local_currency,
    fx_date,
    success,
    cost_local,
    total_cost_without_time_local,
    fx_rate_local_per_usd,
    fx_source,
    flag_missing_currency,
    flag_missing_fx
  )

if (nrow(fx_missing_review) > 0 && !ALLOW_MISSING_FX) {
  write_csv(
    fx_missing_review,
    file.path(results_dir, "IADB_06_fx_missing_review.csv")
  )
  
  stop(
    "FX is missing for some observations and ALLOW_MISSING_FX = FALSE. ",
    "Review IADB_06_fx_missing_review.csv."
  )
}

cost_time_outlier_review <- sap_main_ct |>
  filter(
    flag_negative_cost_usd |
      flag_extreme_total_cost_usd |
      flag_negative_reported_time |
      flag_extreme_reported_time |
      flag_negative_interaction_time |
      flag_extreme_interaction_time |
      flag_negative_transaction_duration |
      flag_extreme_transaction_duration
  ) |>
  select(
    unique_transaction_id,
    confederate_match_key,
    country_clean_final,
    assigned_channel,
    assigned_amount,
    assigned_delivery,
    success,
    total_cost_without_time_usd,
    cost_usd,
    reported_time_hours,
    transaction_duration_hours,
    interaction_time_hours,
    starts_with("flag_")
  )

cost_time_by_channel <- sap_main_ct |>
  group_by(assigned_channel) |>
  summarise(
    n = n(),
    n_cost_usd_any_attempt =
      sum(sample_cost_usd_any_attempt, na.rm = TRUE),
    n_cost_usd_success_only =
      sum(sample_cost_usd_success_only, na.rm = TRUE),
    n_reported_time =
      sum(sample_reported_time, na.rm = TRUE),
    n_interaction_time =
      sum(sample_interaction_time, na.rm = TRUE),
    mean_total_cost_without_time_usd =
      mean(total_cost_without_time_usd, na.rm = TRUE),
    median_total_cost_without_time_usd =
      median(total_cost_without_time_usd, na.rm = TRUE),
    mean_reported_time_hours =
      mean(reported_time_hours, na.rm = TRUE),
    median_reported_time_hours =
      median(reported_time_hours, na.rm = TRUE),
    mean_interaction_time_hours =
      mean(interaction_time_hours, na.rm = TRUE),
    median_interaction_time_hours =
      median(interaction_time_hours, na.rm = TRUE),
    .groups = "drop"
  )

fx_sources_used <- sap_main_ct |>
  filter(
    !is.na(total_cost_without_time_local) |
      !is.na(cost_local)
  ) |>
  count(
    local_currency,
    fx_source,
    name = "n_rows",
    sort = TRUE
  )

cat("\n=== Cost/time sample summary ===\n")
print(cost_time_sample_summary, n = Inf)

cat("\n=== Cost/time by assigned channel: main sample ===\n")
print(cost_time_by_channel, n = Inf)

cat("\n=== FX sources used in main sample cost rows ===\n")
print(fx_sources_used, n = Inf)

write_csv(
  cost_time_sample_summary,
  file.path(results_dir, "IADB_06_cost_time_sample_summary.csv")
)

write_csv(
  fx_missing_review,
  file.path(results_dir, "IADB_06_fx_missing_review.csv")
)

write_csv(
  cost_time_outlier_review,
  file.path(results_dir, "IADB_06_cost_time_outlier_review.csv")
)

write_csv(
  cost_time_by_channel,
  file.path(results_dir, "IADB_06_cost_time_by_channel_main.csv")
)

write_csv(
  fx_sources_used,
  file.path(results_dir, "IADB_06_fx_sources_used_main_cost_rows.csv")
)

# ------------------------------------------------------------------------------
# Saving cost/time datasets ----------------------------------------------------
# ------------------------------------------------------------------------------

write_csv(
  sap_main_ct,
  file.path(output_dir, "IADB_sap_observed_first_pass_cost_time.csv")
)

saveRDS(
  sap_main_ct,
  file.path(output_dir, "IADB_sap_observed_first_pass_cost_time.rds")
)

write_csv(
  sap_pp_ct,
  file.path(output_dir, "IADB_sap_per_protocol_cost_time.csv")
)

saveRDS(
  sap_pp_ct,
  file.path(output_dir, "IADB_sap_per_protocol_cost_time.rds")
)

write_csv(
  sap_reviewed_ct,
  file.path(output_dir, "IADB_sap_reviewed_submissions_cost_time.csv")
)

saveRDS(
  sap_reviewed_ct,
  file.path(output_dir, "IADB_sap_reviewed_submissions_cost_time.rds")
)

# ------------------------------------------------------------------------------
# Model samples ----------------------------------------------------------------
# ------------------------------------------------------------------------------
sample_sets <- list(
  main_strict_slot_level = sap_main_ct,
  per_protocol_strict_slot = sap_pp_ct,
  reviewed_submissions = sap_reviewed_ct
)

cost_any_samples <- purrr::map(
  sample_sets,
  ~ .x |> filter(sample_cost_usd_any_attempt)
)

cost_success_samples <- purrr::map(
  sample_sets,
  ~ .x |> filter(sample_cost_usd_success_only)
)

reported_time_samples <- purrr::map(
  sample_sets,
  ~ .x |> filter(sample_reported_time)
)

interaction_time_samples <- purrr::map(
  sample_sets,
  ~ .x |> filter(sample_interaction_time)
)

duration_samples <- purrr::map(
  sample_sets,
  ~ .x |> filter(sample_transaction_duration)
)

model_sample_summary <- bind_rows(
  imap_dfr(
    cost_any_samples,
    ~ tibble(model_sample = paste0("cost_any_", .y), n = nrow(.x))
  ),
  imap_dfr(
    cost_success_samples,
    ~ tibble(model_sample = paste0("cost_success_", .y), n = nrow(.x))
  ),
  imap_dfr(
    reported_time_samples,
    ~ tibble(model_sample = paste0("reported_time_", .y), n = nrow(.x))
  ),
  imap_dfr(
    interaction_time_samples,
    ~ tibble(model_sample = paste0("interaction_time_", .y), n = nrow(.x))
  ),
  imap_dfr(
    duration_samples,
    ~ tibble(model_sample = paste0("duration_", .y), n = nrow(.x))
  )
)

write_csv(
  model_sample_summary,
  file.path(results_dir, "IADB_06_model_sample_summary.csv")
)

cat("\n=== Model sample summary ===\n")
print(model_sample_summary, n = Inf)

# ------------------------------------------------------------------------------
# Cost/time SAP models ---------------------------------------------------------
# ------------------------------------------------------------------------------
cost_any_models <- estimate_model_set(
  outcome = "total_cost_without_time_usd",
  samples_named = cost_any_samples,
  prefix = "cost_any"
)

cost_success_models <- estimate_model_set(
  outcome = "total_cost_without_time_usd",
  samples_named = cost_success_samples,
  prefix = "cost_success"
)

reported_time_models <- estimate_model_set(
  outcome = "reported_time_hours",
  samples_named = reported_time_samples,
  prefix = "reported_time"
)

interaction_time_models <- estimate_model_set(
  outcome = "interaction_time_hours",
  samples_named = interaction_time_samples,
  prefix = "interaction_time"
)

duration_models <- estimate_model_set(
  outcome = "transaction_duration_hours",
  samples_named = duration_samples,
  prefix = "duration"
)

model_list <- c(
  cost_any_models,
  cost_success_models,
  reported_time_models,
  interaction_time_models,
  duration_models
)

model_list_nonnull <- model_list[
  !vapply(model_list, is.null, logical(1))
]

skipped_models <- tibble(
  model = names(model_list)[vapply(model_list, is.null, logical(1))],
  reason = "Outcome is constant, all missing, zero-row sample, or model could not be estimated."
)

write_csv(
  skipped_models,
  file.path(results_dir, "IADB_06_cost_time_models_skipped.csv")
)

saveRDS(
  model_list,
  file.path(results_dir, "IADB_06_cost_time_models.rds")
)

# ------------------------------------------------------------------------------
# Exporting model tables -------------------------------------------------------
# ------------------------------------------------------------------------------
write_etable_group(
  cost_any_models,
  group_name = "cost_any_attempt",
  results_dir = results_dir
)

write_etable_group(
  cost_success_models,
  group_name = "cost_success_only",
  results_dir = results_dir
)

write_etable_group(
  reported_time_models,
  group_name = "reported_time",
  results_dir = results_dir
)

write_etable_group(
  interaction_time_models,
  group_name = "interaction_time",
  results_dir = results_dir
)

write_etable_group(
  duration_models,
  group_name = "transaction_duration",
  results_dir = results_dir
)

if (length(model_list_nonnull) > 0) {
  do.call(
    etable,
    c(
      model_list_nonnull,
      list(
        tex = FALSE,
        file = file.path(results_dir, "IADB_06_cost_time_models_all.txt")
      )
    )
  )
  
  do.call(
    etable,
    c(
      model_list_nonnull,
      list(
        tex = TRUE,
        file = file.path(results_dir, "IADB_06_cost_time_models_all.tex")
      )
    )
  )
}

cat("\n=== Skipped cost/time models ===\n")
print(skipped_models, n = Inf)
