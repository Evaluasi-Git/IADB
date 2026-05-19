# ==============================================================================
# IADB - 06a Build Daily FX Rates ----------------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: May 18, 2026 -----------------------------------------------------------
# Purpose:
#   Build data/manual/IADB_fx_rates_daily.csv for Script 06.
#
# Output:
#   data/manual/IADB_fx_rates_daily.csv
#   data/manual/IADB_fx_rates_daily_diagnostics.csv
#
# Interpretation:
#   fx_rate_local_per_usd = units of local currency per 1 USD.
#   Example: BRL = 5.20 means 1 USD = 5.20 Brazilian reais.
#
# Source hierarchy:
#   1. USD identity rate
#   2. Verified FRED daily series for BRL and MXN, if FRED key is available
#   3. Public no-key currency API fallback for remaining currencies
# ==============================================================================

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fredr)
  library(httr2)
  library(janitor)
  library(here)
  library(zoo)
})

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
manual_dir <- here("data", "manual")

sap_output_dir <- here(
  "data",
  "clean",
  "sap_dataset_builder"
)

dir.create(
  manual_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  sap_output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

fx_output_path <- file.path(
  manual_dir,
  "IADB_fx_rates_daily.csv"
)

fx_diagnostics_output_path <- file.path(
  manual_dir,
  "IADB_fx_rates_daily_diagnostics.csv"
)

# ------------------------------------------------------------------------------
# Currencies needed ------------------------------------------------------------
# ------------------------------------------------------------------------------
needed_currencies <- c(
  "ARS",
  "BRL",
  "CLP",
  "COP",
  "CRC",
  "GTQ",
  "JMD",
  "MXN",
  "NIO",
  "PEN",
  "USD"
)

# ------------------------------------------------------------------------------
# Date range -------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Date range from SAP datasets, if they exist.
sap_candidate_paths <- c(
  file.path(sap_output_dir, "IADB_sap_observed_maximal_auto.rds"),
  file.path(sap_output_dir, "IADB_sap_per_protocol_maximal_auto.rds"),
  file.path(sap_output_dir, "IADB_sap_observed_first_pass.rds")
)

existing_sap_paths <- sap_candidate_paths[file.exists(sap_candidate_paths)]

# Safe dates
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

if (length(existing_sap_paths) > 0) {
  sap_dates <- purrr::map_dfr(
    existing_sap_paths,
    function(pp) {
      readRDS(pp) |>
        clean_names() |>
        add_missing_cols(c(
          "transaction_date",
          "submission_datetime",
          "assigned_date"
        )) |>
        transmute(
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
  ) |>
    filter(!is.na(fx_date_candidate)) |>
    pull(fx_date_candidate)
  
  if (length(sap_dates) > 0) {
    start_date <- min(sap_dates, na.rm = TRUE) - 7
    end_date   <- max(sap_dates, na.rm = TRUE) + 7
  } else {
    start_date <- as.Date("2026-03-01")
    end_date   <- Sys.Date()
  }
} else {
  start_date <- as.Date("2026-03-01")
  end_date   <- Sys.Date()
}

# Do not request beyond current date.
end_date <- min(end_date, Sys.Date(), na.rm = TRUE)

date_seq <- seq.Date(start_date, end_date, by = "day")

# ------------------------------------------------------------------------------
# USD identity -----------------------------------------------------------------
# ------------------------------------------------------------------------------
# For dollarized economies
usd_fx <- tibble(
  currency = "USD",
  fx_date = date_seq,
  fx_rate_local_per_usd = 1,
  fx_source = "USD_identity"
)

# ------------------------------------------------------------------------------
# Verified FRED daily series ---------------------------------------------------
# ------------------------------------------------------------------------------
# Cedric's personal FRED API key
FRED_API_KEY <- "f854d51a952bb7314ca18c6af7dcbbc9"
# Important:
#   Sys.getenv() needs the NAME of the environment variable, not the key itself.
#   Your .Renviron should contain: FRED_API_KEY=...
fred_api_key <- Sys.getenv("FRED_API_KEY")

if (fred_api_key != "") {
  fredr_set_key(fred_api_key)
} else {
  warning(
    "FRED_API_KEY is not set. BRL and MXN will use the public fallback API."
  )
}

fred_series <- tribble(
  ~currency, ~fred_series_id,
  "BRL",     "DEXBZUS",
  "MXN",     "DEXMXUS"
)

fred_safe <- purrr::safely(fredr)

get_fred_fx <- function(currency, fred_series_id) {
  if (fred_api_key == "") {
    return(
      tibble(
        currency = currency,
        fx_date = date_seq,
        fx_rate_local_per_usd = NA_real_,
        fx_source = paste0("missing_FRED_key_", fred_series_id)
      )
    )
  }
  
  out <- fred_safe(
    series_id = fred_series_id,
    observation_start = start_date,
    observation_end = end_date
  )
  
  if (!is.null(out$error)) {
    warning("FRED failed for ", currency, ": ", out$error$message)
    
    return(
      tibble(
        currency = currency,
        fx_date = date_seq,
        fx_rate_local_per_usd = NA_real_,
        fx_source = paste0("FRED_error_", fred_series_id)
      )
    )
  }
  
  out$result |>
    transmute(
      currency = currency,
      fx_date = as.Date(date),
      fx_rate_local_per_usd = as.numeric(value),
      fx_source = paste0("FRED_", fred_series_id)
    )
}

fred_fx_raw <- pmap_dfr(
  fred_series,
  \(currency, fred_series_id) get_fred_fx(currency, fred_series_id)
)

# ------------------------------------------------------------------------------
# Public no-key API fallback ---------------------------------------------------
# ------------------------------------------------------------------------------
# This uses the open fawazahmed0 exchange/currency API endpoints as a fallback.
# It is useful for broad currency coverage when FRED or central-bank feeds are
# unavailable. The project provides currency exchange data through jsDelivr and
# a pages.dev fallback endpoint. :contentReference[oaicite:1]{index=1}
api_currencies <- setdiff(
  needed_currencies,
  "USD"
)

get_public_fx_one_day <- function(date, currencies) {
  date_chr <- as.character(date)
  
  # Primary jsDelivr endpoint for date-specific rates.
  url_primary <- paste0(
    "https://cdn.jsdelivr.net/npm/@fawazahmed0/currency-api@",
    date_chr,
    "/v1/currencies/usd.json"
  )
  
  resp <- tryCatch(
    request(url_primary) |>
      req_timeout(seconds = 20) |>
      req_perform(),
    error = function(e) NULL
  )
  
  source_used <- "fawazahmed0_currency_api_jsdelivr_usd_base"
  
  # Fallback pages.dev endpoint.
  if (is.null(resp)) {
    url_fallback <- paste0(
      "https://",
      date_chr,
      ".currency-api.pages.dev/v1/currencies/usd.json"
    )
    
    resp <- tryCatch(
      request(url_fallback) |>
        req_timeout(seconds = 20) |>
        req_perform(),
      error = function(e) NULL
    )
    
    source_used <- "fawazahmed0_currency_api_pages_dev_usd_base"
  }
  
  if (is.null(resp)) {
    return(
      tibble(
        currency = currencies,
        fx_date = date,
        fx_rate_local_per_usd = NA_real_,
        fx_source = "public_currency_api_failed"
      )
    )
  }
  
  body <- tryCatch(
    resp_body_json(resp, simplifyVector = TRUE),
    error = function(e) NULL
  )
  
  if (is.null(body) || is.null(body$usd)) {
    return(
      tibble(
        currency = currencies,
        fx_date = date,
        fx_rate_local_per_usd = NA_real_,
        fx_source = "public_currency_api_no_usd_rates"
      )
    )
  }
  
  rates <- body$usd
  
  tibble(
    currency = currencies,
    fx_date = date,
    fx_rate_local_per_usd = map_dbl(
      str_to_lower(currencies),
      function(cc) {
        val <- rates[[cc]]
        
        if (is.null(val)) {
          NA_real_
        } else {
          as.numeric(val)
        }
      }
    ),
    fx_source = source_used
  )
}

public_fx_raw <- map_dfr(
  date_seq,
  \(dd) get_public_fx_one_day(dd, api_currencies)
)

# ------------------------------------------------------------------------------
# Combining rates with source priority -----------------------------------------
# ------------------------------------------------------------------------------
# Source priority:
#   USD identity = 1
#   FRED = 2
#   public fallback API = 3
#
# Important fix:
#   We first drop rows with missing rates, then select the first available source.
#   This guarantees that fx_source corresponds to the actual non-missing rate used.

fx_available <- bind_rows(
  usd_fx |>
    mutate(source_priority = 1),
  
  fred_fx_raw |>
    mutate(source_priority = 2),
  
  public_fx_raw |>
    mutate(source_priority = 3)
) |>
  filter(currency %in% needed_currencies) |>
  filter(!is.na(fx_date)) |>
  filter(
    !is.na(fx_rate_local_per_usd),
    fx_rate_local_per_usd > 0
  ) |>
  arrange(
    currency,
    fx_date,
    source_priority
  ) |>
  group_by(currency, fx_date) |>
  slice(1) |>
  ungroup() |>
  select(
    currency,
    fx_date,
    fx_rate_local_per_usd,
    fx_source
  )

fx_skeleton <- tidyr::expand_grid(
  currency = needed_currencies,
  fx_date = date_seq
)

fx_daily <- fx_skeleton |>
  left_join(
    fx_available,
    by = c("currency", "fx_date")
  ) |>
  arrange(currency, fx_date) |>
  group_by(currency) |>
  mutate(
    # Carry forward latest prior rate.
    fx_rate_local_per_usd =
      zoo::na.locf(fx_rate_local_per_usd, na.rm = FALSE),
    fx_source =
      zoo::na.locf(fx_source, na.rm = FALSE),
    
    # Backfill at beginning if first available rate starts after start_date.
    fx_rate_local_per_usd =
      zoo::na.locf(
        fx_rate_local_per_usd,
        fromLast = TRUE,
        na.rm = FALSE
      ),
    fx_source =
      zoo::na.locf(
        fx_source,
        fromLast = TRUE,
        na.rm = FALSE
      )
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# Diagnostics/Checks -----------------------------------------------------------
# ------------------------------------------------------------------------------
fx_diagnostics <- fx_daily |>
  group_by(currency) |>
  summarise(
    first_date = min(fx_date),
    last_date = max(fx_date),
    n_days = n(),
    first_rate = first(fx_rate_local_per_usd),
    last_rate = last(fx_rate_local_per_usd),
    min_rate = suppressWarnings(min(fx_rate_local_per_usd, na.rm = TRUE)),
    max_rate = suppressWarnings(max(fx_rate_local_per_usd, na.rm = TRUE)),
    missing_rates = sum(is.na(fx_rate_local_per_usd)),
    sources = paste(sort(unique(na.omit(fx_source))), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    min_rate = ifelse(is.infinite(min_rate), NA_real_, min_rate),
    max_rate = ifelse(is.infinite(max_rate), NA_real_, max_rate)
  )

fx_source_counts <- fx_daily |>
  count(
    currency,
    fx_source,
    name = "n_days",
    sort = TRUE
  )

cat("\n=== FX diagnostics ===\n")
print(fx_diagnostics, n = Inf)

cat("\n=== FX source counts ===\n")
print(fx_source_counts, n = Inf)

# ------------------------------------------------------------------------------
# 10. Save outputs --------------------------------------------------------------
# ------------------------------------------------------------------------------
write_csv(
  fx_daily,
  fx_output_path
)

write_csv(
  fx_diagnostics,
  fx_diagnostics_output_path
)

write_csv(
  fx_source_counts,
  file.path(manual_dir, "IADB_fx_rates_daily_source_counts.csv")
)

# ------------------------------------------------------------------------------
# Safety checks ----------------------------------------------------------------
# ------------------------------------------------------------------------------
if (any(fx_diagnostics$missing_rates > 0)) {
  warning(
    "Some currencies still have missing FX rates. ",
    "Review IADB_fx_rates_daily_diagnostics.csv before running cost models."
  )
}

if (any(is.na(fx_daily$fx_rate_local_per_usd))) {
  warning(
    "At least one FX row has missing fx_rate_local_per_usd."
  )
}

if (any(fx_daily$fx_rate_local_per_usd <= 0, na.rm = TRUE)) {
  stop(
    "At least one FX row has non-positive fx_rate_local_per_usd. ",
    "Review IADB_fx_rates_daily.csv."
  )
}

cat("\nSaved FX file to:\n")
cat(fx_output_path, "\n")

cat("\nSaved FX diagnostics to:\n")
cat(fx_diagnostics_output_path, "\n")
