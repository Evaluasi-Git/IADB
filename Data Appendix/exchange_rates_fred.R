# Title: Extracting FX rates from FRED -----------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: April, 2026 ------------------------------------------------------------

# Cleaning my environment
rm(list = ls())

# Managing memory 
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fredr)
  library(zoo)
})

# ------------------------------------------------------------------------------
# Settings ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Cedric's personal FRED key
fredr_set_key("f854d51a952bb7314ca18c6af7dcbbc9")

# Transaction dates
start_date <- as.Date("2026-03-04")
end_date   <- as.Date("2026-04-23")

# FRED daily series usually report:
# local currency units per 1 USD.
#
# Countries without a reliable daily FRED series are left as NA
# and can be filled manually or through another source later.
fx_series <- tribble(
  ~country_clean, ~country_label, ~local_currency, ~fred_series_id,
  "brazil",    "Brazil",    "BRL", "DEXBZUS",
  "chile",     "Chile",     "CLP", "DEXCHUS",
  "colombia",  "Colombia",  "COP", NA_character_,
  "mexico",    "Mexico",    "MXN", "DEXMXUS",
  "peru",      "Peru",      "PEN", NA_character_,
  "ecuador",   "Ecuador",   "USD", NA_character_,
  "panama",    "Panama",    "USD", NA_character_,
  "nicaragua", "Nicaragua", "NIO", NA_character_
)

# ------------------------------------------------------------------------------
# Safe FRED extraction function ------------------------------------------------
# ------------------------------------------------------------------------------
fred_safe <- purrr::safely(fredr)

get_fx_one_country <- function(country_clean,
                               country_label,
                               local_currency,
                               fred_series_id,
                               start_date,
                               end_date) {
  
  # Dollarized countries
  if (is.na(fred_series_id) && local_currency == "USD") {
    return(
      tibble(
        date = seq.Date(start_date, end_date, by = "day"),
        country_clean = country_clean,
        country_label = country_label,
        local_currency = local_currency,
        fred_series_id = NA_character_,
        lcu_per_usd = 1,
        source = "Dollarized country; fixed at 1"
      )
    )
  }
  # Countries needing manual/alternative source
  if (is.na(fred_series_id)) {
    return(
      tibble(
        date = seq.Date(start_date, end_date, by = "day"),
        country_clean = country_clean,
        country_label = country_label,
        local_currency = local_currency,
        fred_series_id = NA_character_,
        lcu_per_usd = NA_real_,
        source = "Manual/alternative source needed"
      )
    )
  }
  
  out <- fred_safe(
    series_id = fred_series_id,
    observation_start = start_date,
    observation_end = end_date
  )
  
  if (!is.null(out$error)) {
    return(
      tibble(
        date = seq.Date(start_date, end_date, by = "day"),
        country_clean = country_clean,
        country_label = country_label,
        local_currency = local_currency,
        fred_series_id = fred_series_id,
        lcu_per_usd = NA_real_,
        source = paste("FRED error:", out$error$message)
      )
    )
  }
  
  out$result |>
    transmute(
      date,
      country_clean = country_clean,
      country_label = country_label,
      local_currency = local_currency,
      fred_series_id = fred_series_id,
      lcu_per_usd = as.numeric(value),
      source = "FRED"
    )
}

# ------------------------------------------------------------------------------
# Pulling FX rates -------------------------------------------------------------
# ------------------------------------------------------------------------------
fx_daily_raw <- pmap_dfr(
  fx_series,
  \(country_clean, country_label, local_currency, fred_series_id) {
    get_fx_one_country(
      country_clean = country_clean,
      country_label = country_label,
      local_currency = local_currency,
      fred_series_id = fred_series_id,
      start_date = start_date,
      end_date = end_date
    )
  }
)

# ------------------------------------------------------------------------------
# Complete date grid and filling weekends/holidays -----------------------------
# ------------------------------------------------------------------------------
fx_daily <- fx_daily_raw |>
  group_by(country_clean, country_label, local_currency, fred_series_id, source) |>
  complete(date = seq.Date(start_date, end_date, by = "day")) |>
  arrange(country_clean, date) |>
  mutate(
    lcu_per_usd = zoo::na.locf(lcu_per_usd, na.rm = FALSE),
    lcu_per_usd = zoo::na.locf(lcu_per_usd, fromLast = TRUE, na.rm = FALSE),
    usd_per_lcu = 1 / lcu_per_usd
  ) |>
  ungroup()

# ------------------------------------------------------------------------------
# Weekly averages --------------------------------------------------------------
# ------------------------------------------------------------------------------
fx_weekly <- fx_daily |>
  mutate(week_start = floor_date(date, unit = "week", week_start = 1)) |>
  group_by(country_clean, country_label, local_currency, week_start) |>
  summarise(
    lcu_per_usd_weekly = mean(lcu_per_usd, na.rm = TRUE),
    usd_per_lcu_weekly = mean(usd_per_lcu, na.rm = TRUE),
    n_days_observed = sum(!is.na(lcu_per_usd)),
    source = paste(unique(source), collapse = "; "),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# Diagnostic table -------------------------------------------------------------
# ------------------------------------------------------------------------------
fx_diagnostics <- fx_daily |>
  group_by(country_label, country_clean, local_currency, source) |>
  summarise(
    start_rate = first(lcu_per_usd),
    end_rate = last(lcu_per_usd),
    pct_change = 100 * (end_rate / start_rate - 1),
    missing_rates = sum(is.na(lcu_per_usd)),
    .groups = "drop"
  ) |>
  arrange(desc(missing_rates), desc(abs(pct_change)))

print(fx_diagnostics, n = Inf)

# ------------------------------------------------------------------------------
# 7. Plot: Levels --------------------------------------------------------------
# ------------------------------------------------------------------------------
fx_plot_data <- fx_daily |>
  filter(!is.na(lcu_per_usd))

p_fx_levels <- ggplot(fx_plot_data, aes(x = date, y = lcu_per_usd)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ country_label, scales = "free_y") +
  labs(
    title = "Daily Exchange Rates During IADB Transactions",
    subtitle = "Local currency units per 1 USD",
    x = NULL,
    y = "Local currency per USD",
    caption = "Source: FRED where available. Ecuador and Panama fixed at 1. Countries without FRED daily series require manual/alternative source."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_fx_levels)

# ------------------------------------------------------------------------------
# Plot: Indexed Variation ------------------------------------------------------
# ------------------------------------------------------------------------------
fx_index_data <- fx_plot_data |>
  group_by(country_clean) |>
  mutate(
    lcu_per_usd_index = 100 * lcu_per_usd / first(lcu_per_usd)
  ) |>
  ungroup()

p_fx_index <- ggplot(fx_index_data, aes(x = date, y = lcu_per_usd_index)) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ country_label) +
  labs(
    title = "Exchange-Rate Variation Indexed to First Transaction Date",
    subtitle = "March 4, 2026 = 100. Values above 100 mean local currency depreciated against USD.",
    x = NULL,
    y = "Index: local currency per USD",
    caption = "Source: FRED where available."
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_fx_index)

# ------------------------------------------------------------------------------
# Saving outputs
# ------------------------------------------------------------------------------
write_csv(fx_daily, "fx_daily_iadb_2026_03_04_to_2026_04_23.csv")
write_csv(fx_weekly, "fx_weekly_iadb_2026_03_04_to_2026_04_23.csv")
write_csv(fx_diagnostics, "fx_diagnostics_iadb_2026_03_04_to_2026_04_23.csv")

ggsave(
  filename = "fx_daily_levels_iadb_2026_03_04_to_2026_04_23.png",
  plot = p_fx_levels,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "fx_daily_index_iadb_2026_03_04_to_2026_04_23.png",
  plot = p_fx_index,
  width = 11,
  height = 7,
  dpi = 300
)
