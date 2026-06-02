# ==============================================================================
# IADB - 08c Descriptives and Outcome Distributions -----------------------------
# Author: Cedric Antunes (Evaluasi)
# Date: June 2026
#
# Purpose:
#   Produce PAP/SAP-aligned descriptive statistics and outcome-distribution
#   figures for the IADB KYC/AML audit study.
#
# What this script does:
#   1. Loads final SAP analysis datasets already created upstream.
#   2. Standardizes assigned treatment variables and primary outcomes.
#   3. Produces sample-composition tables for the main, per-protocol, and
#      reviewed-submissions samples.
#   4. Produces unadjusted descriptive outcome summaries overall and by assigned
#      channel, amount, delivery mode, and country.
#   5. Produces client-facing descriptive figures for outcome distributions.
#
# What this script does NOT do:
#   - It does not estimate causal treatment effects.
#   - It does not replace Script 08 primary CR2/Romano-Wolf/Holm inference.
#   - It does not clean SurveyCTO data or alter analysis datasets.
#   - It does not use raw descriptives for confirmatory claims.
#
# Interpretation:
#   All tables and figures in this script are descriptive. They show observed
#   distributions and unadjusted outcome levels. Confirmatory causal claims should
#   use Script 08 model outputs and multiplicity-adjusted inference.
#
# Inputs:
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol.rds
#   data/clean/sap_dataset_builder/IADB_sap_reviewed_submissions.rds
#   data/clean/sap_dataset_builder/IADB_sap_observed_first_pass_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_per_protocol_cost_time.rds
#   data/clean/sap_dataset_builder/IADB_sap_reviewed_submissions_cost_time.rds
#
# Outputs:
#   data/clean/sap_dataset_builder/descriptives/
#   data/clean/sap_dataset_builder/final_figures/descriptives/
# ===============================================================================

# ------------------------------------------------------------------------------
# 0. Setup ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(readr)
  library(here)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1. Paths ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_dir <- here("data", "clean", "sap_dataset_builder")
desc_dir <- file.path(sap_dir, "descriptives")
figure_dir <- file.path(sap_dir, "final_figures", "descriptives")

# Optional: keep a copy in final_etables for easier report compilation.
etable_dir <- file.path(sap_dir, "final_etables")

dir.create(desc_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(etable_dir, showWarnings = FALSE, recursive = TRUE)

sap_main_path <- file.path(sap_dir, "IADB_sap_observed_first_pass.rds")
sap_pp_path <- file.path(sap_dir, "IADB_sap_per_protocol.rds")
sap_reviewed_path <- file.path(sap_dir, "IADB_sap_reviewed_submissions.rds")

ct_main_path <- file.path(sap_dir, "IADB_sap_observed_first_pass_cost_time.rds")
ct_pp_path <- file.path(sap_dir, "IADB_sap_per_protocol_cost_time.rds")
ct_reviewed_path <- file.path(sap_dir, "IADB_sap_reviewed_submissions_cost_time.rds")

required_files <- c(
  sap_main_path,
  sap_pp_path,
  sap_reviewed_path,
  ct_main_path,
  ct_pp_path,
  ct_reviewed_path
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste(missing_files, collapse = "\n")
  )
}

# ------------------------------------------------------------------------------
# 2. Visual settings -----------------------------------------------------------
# ------------------------------------------------------------------------------
COLOR_BAR <- "#dfe2d2"
COLOR_BAR_DARK <- "#6cbf84"
COLOR_CI <- "#323339"
COLOR_TEXT <- "#323339"
COLOR_LINE <- "#323339"
COLOR_ALT <- "#f26968"

BASE_SIZE <- 12

primary_outcomes <- c(
  "success",
  "kyc_0_3",
  "cost_success",
  "time_duration"
)

outcome_labels <- c(
  success = "Transaction success\n(%)",
  kyc_0_3 = "KYC score\n(0-3 points)",
  cost_success = "Transaction cost\n(USD)",
  time_duration = "Transaction duration\n(hours)",
  cost_any_sensitivity = "Any-attempt cost\n(USD)",
  reported_time_sensitivity = "Reported time\n(hours)",
  interaction_time_sensitivity = "Interaction time\n(hours)",
  kyc_0_5_sensitivity = "KYC composite\n(0-5 points)"
)

channel_levels <- c("Banks", "MTOs", "Fintech", "Crypto")
amount_levels <- c("100", "250")
delivery_levels <- c("In-person", "Online")

channel_labels <- c(
  Banks = "Banks",
  MTOs = "MTOs",
  Fintech = "Fintech",
  Crypto = "Crypto"
)

# ------------------------------------------------------------------------------
# 3. Helpers -------------------------------------------------------------------
# ------------------------------------------------------------------------------
to_num <- function(x) {
  suppressWarnings(readr::parse_number(as.character(x)))
}

as_logical_safe <- function(x) {
  if (is.logical(x)) return(replace_na(x, FALSE))
  x_clean <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  case_when(
    x_clean %in% c("true", "t", "1", "yes", "sim", "si") ~ TRUE,
    x_clean %in% c("false", "f", "0", "no", "na", "", "missing") ~ FALSE,
    TRUE ~ FALSE
  )
}

add_missing_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) df[[cc]] <- NA
  }
  df
}

filter_required_flag <- function(df, flag_name) {
  if (!flag_name %in% names(df)) {
    stop("Required sample flag is missing: ", flag_name)
  }
  df |> filter(as_logical_safe(.data[[flag_name]]))
}

standardize_model_vars <- function(df) {
  df |>
    clean_names() |>
    add_missing_cols(c(
      "success",
      "kyc_score",
      "kyc_score_composite_0_5",
      "total_cost_without_time_usd",
      "reported_time_hours",
      "interaction_time_hours",
      "transaction_duration_hours",
      "assigned_channel",
      "assigned_amount",
      "assigned_delivery",
      "confederate_match_key",
      "country",
      "needs_manual_review_for_final",
      "sample_cost_usd_any_attempt",
      "sample_cost_usd_success_only",
      "sample_reported_time",
      "sample_interaction_time",
      "sample_transaction_duration"
    )) |>
    mutate(
      success = to_num(success),
      kyc_score_0_3 = to_num(kyc_score),
      kyc_score_0_5 = to_num(kyc_score_composite_0_5),
      total_cost_without_time_usd = to_num(total_cost_without_time_usd),
      reported_time_hours = to_num(reported_time_hours),
      interaction_time_hours = to_num(interaction_time_hours),
      transaction_duration_hours = to_num(transaction_duration_hours),
      assigned_amount = to_num(assigned_amount),
      assigned_amount_label = factor(
        as.character(assigned_amount),
        levels = amount_levels,
        labels = c("USD 100", "USD 250")
      ),
      assigned_channel = factor(
        assigned_channel,
        levels = channel_levels
      ),
      assigned_delivery = factor(
        assigned_delivery,
        levels = delivery_levels
      ),
      country = as.factor(country),
      confederate_match_key = as.factor(confederate_match_key),
      needs_manual_review_for_final = as_logical_safe(needs_manual_review_for_final),
      sample_cost_usd_any_attempt = as_logical_safe(sample_cost_usd_any_attempt),
      sample_cost_usd_success_only = as_logical_safe(sample_cost_usd_success_only),
      sample_reported_time = as_logical_safe(sample_reported_time),
      sample_interaction_time = as_logical_safe(sample_interaction_time),
      sample_transaction_duration = as_logical_safe(sample_transaction_duration)
    )
}

mean_ci <- function(x, conf_level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) {
    return(tibble(
      n = 0L,
      mean = NA_real_,
      sd = NA_real_,
      se = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      median = NA_real_,
      p25 = NA_real_,
      p75 = NA_real_,
      min = NA_real_,
      max = NA_real_
    ))
  }
  m <- mean(x)
  s <- stats::sd(x)
  se <- s / sqrt(n)
  crit <- if (n > 1) stats::qt((1 + conf_level) / 2, df = n - 1) else NA_real_
  tibble(
    n = n,
    mean = m,
    sd = s,
    se = se,
    conf.low = if_else(n > 1, m - crit * se, NA_real_),
    conf.high = if_else(n > 1, m + crit * se, NA_real_),
    median = stats::median(x),
    p25 = as.numeric(stats::quantile(x, 0.25, names = FALSE, type = 7)),
    p75 = as.numeric(stats::quantile(x, 0.75, names = FALSE, type = 7)),
    min = min(x),
    max = max(x)
  )
}

summarize_outcome <- function(df, outcome_var, outcome_label, group_vars = NULL) {
  if (is.null(group_vars) || length(group_vars) == 0) {
    out <- mean_ci(df[[outcome_var]]) |>
      mutate(outcome_label = outcome_label, .before = 1)
  } else {
    out <- df |>
      group_by(across(all_of(group_vars))) |>
      group_modify(~ mean_ci(.x[[outcome_var]])) |>
      ungroup() |>
      mutate(outcome_label = outcome_label, .before = 1)
  }
  out
}

save_csv_both <- function(df, filename) {
  readr::write_csv(df, file.path(desc_dir, filename))
  readr::write_csv(df, file.path(etable_dir, filename))
  invisible(df)
}

save_plot <- function(plot, output_stem, width = 11, height = 6.8) {
  ggsave(
    filename = file.path(figure_dir, paste0(output_stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = 320
  )
  ggsave(
    filename = file.path(figure_dir, paste0(output_stem, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    device = cairo_pdf
  )
  invisible(plot)
}

base_theme <- function() {
  theme_minimal(base_size = BASE_SIZE) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", color = COLOR_TEXT, size = BASE_SIZE + 2),
      plot.subtitle = element_text(color = COLOR_TEXT, size = BASE_SIZE - 1),
      plot.caption = element_text(color = COLOR_TEXT, size = BASE_SIZE - 3, hjust = 0),
      axis.text.x = element_text(color = COLOR_TEXT, size = BASE_SIZE - 2),
      axis.text.y = element_text(color = COLOR_TEXT, size = BASE_SIZE - 2),
      axis.title.y = element_text(color = COLOR_TEXT, size = BASE_SIZE - 1),
      strip.text = element_text(face = "bold", color = COLOR_TEXT, size = BASE_SIZE - 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# ------------------------------------------------------------------------------
# 4. Load SAP datasets ---------------------------------------------------------
# ------------------------------------------------------------------------------
sap_main <- readRDS(sap_main_path) |> standardize_model_vars()
sap_pp <- readRDS(sap_pp_path) |> standardize_model_vars()
sap_reviewed <- readRDS(sap_reviewed_path) |> standardize_model_vars()

ct_main <- readRDS(ct_main_path) |> standardize_model_vars()
ct_pp <- readRDS(ct_pp_path) |> standardize_model_vars()
ct_reviewed <- readRDS(ct_reviewed_path) |> standardize_model_vars()

sap_samples <- list(
  main_strict_slot_level = sap_main,
  per_protocol_strict_slot = sap_pp,
  reviewed_submissions = sap_reviewed
)

ct_samples <- list(
  main_strict_slot_level = ct_main,
  per_protocol_strict_slot = ct_pp,
  reviewed_submissions = ct_reviewed
)

cost_any_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_cost_usd_any_attempt"))
cost_success_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_cost_usd_success_only"))
reported_time_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_reported_time"))
interaction_time_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_interaction_time"))
duration_samples <- purrr::map(ct_samples, ~ filter_required_flag(.x, "sample_transaction_duration"))

# Main descriptive sample map --------------------------------------------------
primary_dataset_map <- list(
  success = list(
    outcome_var = "success",
    outcome_label = "success",
    data = sap_samples$main_strict_slot_level,
    display_label = "Transaction success"
  ),
  kyc_0_3 = list(
    outcome_var = "kyc_score_0_3",
    outcome_label = "kyc_0_3",
    data = sap_samples$main_strict_slot_level,
    display_label = "KYC score (0-3)"
  ),
  cost_success = list(
    outcome_var = "total_cost_without_time_usd",
    outcome_label = "cost_success",
    data = cost_success_samples$main_strict_slot_level,
    display_label = "Transaction cost (successful transactions)"
  ),
  time_duration = list(
    outcome_var = "transaction_duration_hours",
    outcome_label = "time_duration",
    data = duration_samples$main_strict_slot_level,
    display_label = "Transaction duration"
  )
)

# ------------------------------------------------------------------------------
# 5. Sample-composition diagnostics -------------------------------------------
# ------------------------------------------------------------------------------
sample_overview <- bind_rows(
  imap_dfr(sap_samples, ~ tibble(
    dataset_family = "success_kyc",
    sample_label = .y,
    n_rows = nrow(.x),
    n_confederates = n_distinct(.x$confederate_match_key),
    n_countries = n_distinct(.x$country),
    n_success_nonmissing = sum(!is.na(.x$success)),
    n_kyc_0_3_nonmissing = sum(!is.na(.x$kyc_score_0_3)),
    n_kyc_0_5_nonmissing = sum(!is.na(.x$kyc_score_0_5))
  )),
  imap_dfr(cost_success_samples, ~ tibble(
    dataset_family = "cost_success",
    sample_label = .y,
    n_rows = nrow(.x),
    n_confederates = n_distinct(.x$confederate_match_key),
    n_countries = n_distinct(.x$country),
    n_outcome_nonmissing = sum(!is.na(.x$total_cost_without_time_usd))
  )),
  imap_dfr(duration_samples, ~ tibble(
    dataset_family = "time_duration",
    sample_label = .y,
    n_rows = nrow(.x),
    n_confederates = n_distinct(.x$confederate_match_key),
    n_countries = n_distinct(.x$country),
    n_outcome_nonmissing = sum(!is.na(.x$transaction_duration_hours))
  )),
  imap_dfr(reported_time_samples, ~ tibble(
    dataset_family = "reported_time_sensitivity",
    sample_label = .y,
    n_rows = nrow(.x),
    n_confederates = n_distinct(.x$confederate_match_key),
    n_countries = n_distinct(.x$country),
    n_outcome_nonmissing = sum(!is.na(.x$reported_time_hours))
  )),
  imap_dfr(interaction_time_samples, ~ tibble(
    dataset_family = "interaction_time_sensitivity",
    sample_label = .y,
    n_rows = nrow(.x),
    n_confederates = n_distinct(.x$confederate_match_key),
    n_countries = n_distinct(.x$country),
    n_outcome_nonmissing = sum(!is.na(.x$interaction_time_hours))
  ))
)

save_csv_both(sample_overview, "IADB_08c_sample_overview.csv")

composition_by_channel <- imap_dfr(sap_samples, ~ .x |>
  count(sample_label = .y, assigned_channel, name = "n") |>
  group_by(sample_label) |>
  mutate(share = n / sum(n)) |>
  ungroup()
)
save_csv_both(composition_by_channel, "IADB_08c_sample_composition_by_channel.csv")

composition_by_amount <- imap_dfr(sap_samples, ~ .x |>
  count(sample_label = .y, assigned_amount_label, name = "n") |>
  group_by(sample_label) |>
  mutate(share = n / sum(n)) |>
  ungroup()
)
save_csv_both(composition_by_amount, "IADB_08c_sample_composition_by_amount.csv")

composition_by_delivery <- imap_dfr(sap_samples, ~ .x |>
  count(sample_label = .y, assigned_delivery, name = "n") |>
  group_by(sample_label) |>
  mutate(share = n / sum(n)) |>
  ungroup()
)
save_csv_both(composition_by_delivery, "IADB_08c_sample_composition_by_delivery.csv")

composition_channel_amount_delivery <- sap_samples$main_strict_slot_level |>
  count(assigned_channel, assigned_amount_label, assigned_delivery, name = "n") |>
  arrange(assigned_channel, assigned_amount_label, assigned_delivery)
save_csv_both(composition_channel_amount_delivery, "IADB_08c_main_sample_channel_amount_delivery_counts.csv")

composition_by_country <- sap_samples$main_strict_slot_level |>
  count(country, assigned_channel, name = "n") |>
  group_by(country) |>
  mutate(share_within_country = n / sum(n)) |>
  ungroup() |>
  arrange(country, assigned_channel)
save_csv_both(composition_by_country, "IADB_08c_main_sample_channel_by_country_counts.csv")

# ------------------------------------------------------------------------------
# 6. Outcome summaries ---------------------------------------------------------
# ------------------------------------------------------------------------------
outcome_summary_overall <- imap_dfr(primary_dataset_map, function(spec, nm) {
  summarize_outcome(
    spec$data,
    outcome_var = spec$outcome_var,
    outcome_label = spec$outcome_label
  )
}) |>
  mutate(
    outcome_display = recode(outcome_label, !!!outcome_labels, .default = outcome_label),
    mean_display = if_else(outcome_label == "success", mean * 100, mean),
    conf.low_display = if_else(outcome_label == "success", conf.low * 100, conf.low),
    conf.high_display = if_else(outcome_label == "success", conf.high * 100, conf.high)
  )
save_csv_both(outcome_summary_overall, "IADB_08c_outcome_summary_overall.csv")

outcome_summary_by_channel <- imap_dfr(primary_dataset_map, function(spec, nm) {
  summarize_outcome(
    spec$data,
    outcome_var = spec$outcome_var,
    outcome_label = spec$outcome_label,
    group_vars = "assigned_channel"
  )
}) |>
  mutate(
    outcome_display = recode(outcome_label, !!!outcome_labels, .default = outcome_label),
    mean_display = if_else(outcome_label == "success", mean * 100, mean),
    conf.low_display = if_else(outcome_label == "success", conf.low * 100, conf.low),
    conf.high_display = if_else(outcome_label == "success", conf.high * 100, conf.high)
  )
save_csv_both(outcome_summary_by_channel, "IADB_08c_outcome_summary_by_channel.csv")

outcome_summary_by_amount <- imap_dfr(primary_dataset_map, function(spec, nm) {
  summarize_outcome(
    spec$data,
    outcome_var = spec$outcome_var,
    outcome_label = spec$outcome_label,
    group_vars = "assigned_amount_label"
  )
}) |>
  mutate(
    outcome_display = recode(outcome_label, !!!outcome_labels, .default = outcome_label),
    mean_display = if_else(outcome_label == "success", mean * 100, mean),
    conf.low_display = if_else(outcome_label == "success", conf.low * 100, conf.low),
    conf.high_display = if_else(outcome_label == "success", conf.high * 100, conf.high)
  )
save_csv_both(outcome_summary_by_amount, "IADB_08c_outcome_summary_by_amount.csv")

outcome_summary_by_delivery <- imap_dfr(primary_dataset_map, function(spec, nm) {
  summarize_outcome(
    spec$data,
    outcome_var = spec$outcome_var,
    outcome_label = spec$outcome_label,
    group_vars = "assigned_delivery"
  )
}) |>
  mutate(
    outcome_display = recode(outcome_label, !!!outcome_labels, .default = outcome_label),
    mean_display = if_else(outcome_label == "success", mean * 100, mean),
    conf.low_display = if_else(outcome_label == "success", conf.low * 100, conf.low),
    conf.high_display = if_else(outcome_label == "success", conf.high * 100, conf.high)
  )
save_csv_both(outcome_summary_by_delivery, "IADB_08c_outcome_summary_by_delivery.csv")

outcome_summary_by_country <- imap_dfr(primary_dataset_map, function(spec, nm) {
  summarize_outcome(
    spec$data,
    outcome_var = spec$outcome_var,
    outcome_label = spec$outcome_label,
    group_vars = "country"
  )
}) |>
  mutate(
    outcome_display = recode(outcome_label, !!!outcome_labels, .default = outcome_label),
    mean_display = if_else(outcome_label == "success", mean * 100, mean),
    conf.low_display = if_else(outcome_label == "success", conf.low * 100, conf.low),
    conf.high_display = if_else(outcome_label == "success", conf.high * 100, conf.high)
  )
save_csv_both(outcome_summary_by_country, "IADB_08c_outcome_summary_by_country.csv")

# Sensitivity outcome summaries ------------------------------------------------
sensitivity_dataset_map <- list(
  kyc_0_5_sensitivity = list(
    outcome_var = "kyc_score_0_5",
    outcome_label = "kyc_0_5_sensitivity",
    data = sap_samples$main_strict_slot_level
  ),
  cost_any_sensitivity = list(
    outcome_var = "total_cost_without_time_usd",
    outcome_label = "cost_any_sensitivity",
    data = cost_any_samples$main_strict_slot_level
  ),
  reported_time_sensitivity = list(
    outcome_var = "reported_time_hours",
    outcome_label = "reported_time_sensitivity",
    data = reported_time_samples$main_strict_slot_level
  ),
  interaction_time_sensitivity = list(
    outcome_var = "interaction_time_hours",
    outcome_label = "interaction_time_sensitivity",
    data = interaction_time_samples$main_strict_slot_level
  )
)

sensitivity_summary_by_channel <- imap_dfr(sensitivity_dataset_map, function(spec, nm) {
  summarize_outcome(
    spec$data,
    outcome_var = spec$outcome_var,
    outcome_label = spec$outcome_label,
    group_vars = "assigned_channel"
  )
}) |>
  mutate(outcome_display = recode(outcome_label, !!!outcome_labels, .default = outcome_label))
save_csv_both(sensitivity_summary_by_channel, "IADB_08c_sensitivity_outcome_summary_by_channel.csv")

# KYC discrete distribution ----------------------------------------------------
kyc_distribution_by_channel <- sap_samples$main_strict_slot_level |>
  filter(!is.na(kyc_score_0_3)) |>
  mutate(kyc_score_0_3_label = factor(kyc_score_0_3, levels = 0:3)) |>
  count(assigned_channel, kyc_score_0_3_label, name = "n") |>
  group_by(assigned_channel) |>
  mutate(share = n / sum(n)) |>
  ungroup()
save_csv_both(kyc_distribution_by_channel, "IADB_08c_kyc_distribution_by_channel.csv")

success_distribution_by_channel <- sap_samples$main_strict_slot_level |>
  filter(!is.na(success)) |>
  mutate(success_label = factor(success, levels = c(0, 1), labels = c("Not successful", "Successful"))) |>
  count(assigned_channel, success_label, name = "n") |>
  group_by(assigned_channel) |>
  mutate(share = n / sum(n)) |>
  ungroup()
save_csv_both(success_distribution_by_channel, "IADB_08c_success_distribution_by_channel.csv")

# ------------------------------------------------------------------------------
# 7. Missingness / availability summaries -------------------------------------
# ------------------------------------------------------------------------------
outcome_availability <- sap_samples$main_strict_slot_level |>
  summarise(
    n_rows = n(),
    success_nonmissing = sum(!is.na(success)),
    success_missing = sum(is.na(success)),
    kyc_0_3_nonmissing = sum(!is.na(kyc_score_0_3)),
    kyc_0_3_missing = sum(is.na(kyc_score_0_3)),
    kyc_0_5_nonmissing = sum(!is.na(kyc_score_0_5)),
    kyc_0_5_missing = sum(is.na(kyc_score_0_5))
  ) |>
  bind_cols(
    ct_samples$main_strict_slot_level |>
      summarise(
        cost_nonmissing = sum(!is.na(total_cost_without_time_usd)),
        cost_missing = sum(is.na(total_cost_without_time_usd)),
        transaction_duration_nonmissing = sum(!is.na(transaction_duration_hours)),
        transaction_duration_missing = sum(is.na(transaction_duration_hours)),
        reported_time_nonmissing = sum(!is.na(reported_time_hours)),
        reported_time_missing = sum(is.na(reported_time_hours)),
        interaction_time_nonmissing = sum(!is.na(interaction_time_hours)),
        interaction_time_missing = sum(is.na(interaction_time_hours))
      )
  )
save_csv_both(outcome_availability, "IADB_08c_outcome_availability_main_sample.csv")

outcome_availability_by_channel <- sap_samples$main_strict_slot_level |>
  group_by(assigned_channel) |>
  summarise(
    n_rows = n(),
    success_nonmissing = sum(!is.na(success)),
    success_missing = sum(is.na(success)),
    kyc_0_3_nonmissing = sum(!is.na(kyc_score_0_3)),
    kyc_0_3_missing = sum(is.na(kyc_score_0_3)),
    .groups = "drop"
  ) |>
  left_join(
    ct_samples$main_strict_slot_level |>
      group_by(assigned_channel) |>
      summarise(
        cost_nonmissing = sum(!is.na(total_cost_without_time_usd)),
        cost_missing = sum(is.na(total_cost_without_time_usd)),
        transaction_duration_nonmissing = sum(!is.na(transaction_duration_hours)),
        transaction_duration_missing = sum(is.na(transaction_duration_hours)),
        .groups = "drop"
      ),
    by = "assigned_channel"
  )
save_csv_both(outcome_availability_by_channel, "IADB_08c_outcome_availability_by_channel.csv")

# ------------------------------------------------------------------------------
# 8. Figures: sample composition ----------------------------------------------
# ------------------------------------------------------------------------------
p_comp_channel <- composition_by_channel |>
  filter(sample_label == "main_strict_slot_level") |>
  mutate(assigned_channel = factor(assigned_channel, levels = channel_levels)) |>
  ggplot(aes(x = assigned_channel, y = n)) +
  geom_col(fill = COLOR_BAR, width = 0.68) +
  geom_text(aes(label = n), vjust = -0.35, color = COLOR_TEXT, size = 3.6) +
  labs(
    title = "Main analysis sample by assigned channel",
    subtitle = "Counts are based on the main strict slot-level success/KYC sample.",
    x = NULL,
    y = "Transactions",
    caption = "Descriptive count only; this figure does not estimate treatment effects."
  ) +
  base_theme() +
  theme(legend.position = "none")
save_plot(p_comp_channel, "IADB_08c_sample_composition_by_channel", width = 8, height = 5.6)

p_comp_amount_delivery <- bind_rows(
  composition_by_amount |>
    filter(sample_label == "main_strict_slot_level") |>
    transmute(panel = "Assigned amount", category = as.character(assigned_amount_label), n, share),
  composition_by_delivery |>
    filter(sample_label == "main_strict_slot_level") |>
    transmute(panel = "Assigned delivery", category = as.character(assigned_delivery), n, share)
) |>
  mutate(category = factor(category, levels = c("USD 100", "USD 250", "In-person", "Online"))) |>
  ggplot(aes(x = category, y = n)) +
  geom_col(fill = COLOR_BAR, width = 0.68) +
  geom_text(aes(label = n), vjust = -0.35, color = COLOR_TEXT, size = 3.6) +
  facet_wrap(~ panel, scales = "free_x", nrow = 1) +
  labs(
    title = "Main analysis sample by amount and delivery mode",
    subtitle = "Counts are based on the main strict slot-level success/KYC sample.",
    x = NULL,
    y = "Transactions",
    caption = "Descriptive count only; this figure does not estimate treatment effects."
  ) +
  base_theme() +
  theme(legend.position = "none")
save_plot(p_comp_amount_delivery, "IADB_08c_sample_composition_by_amount_delivery", width = 9.5, height = 5.6)

# ------------------------------------------------------------------------------
# 9. Figures: primary outcome means by channel ---------------------------------
# ------------------------------------------------------------------------------
channel_plot_df <- outcome_summary_by_channel |>
  filter(outcome_label %in% primary_outcomes) |>
  mutate(
    outcome_label_clean = factor(
      recode(outcome_label, !!!outcome_labels, .default = outcome_label),
      levels = unname(outcome_labels[primary_outcomes])
    ),
    assigned_channel = factor(assigned_channel, levels = channel_levels),
    mean_plot = mean_display,
    conf.low_plot = conf.low_display,
    conf.high_plot = conf.high_display
  )

p_outcomes_by_channel <- ggplot(
  channel_plot_df,
  aes(x = assigned_channel, y = mean_plot)
) +
  geom_col(fill = COLOR_BAR, width = 0.68) +
  geom_errorbar(
    aes(ymin = conf.low_plot, ymax = conf.high_plot),
    width = 0.18,
    linewidth = 0.75,
    color = COLOR_CI
  ) +
  facet_wrap(~ outcome_label_clean, scales = "free_y", nrow = 1) +
  labs(
    title = "Primary outcome levels by assigned channel",
    subtitle = "Unadjusted descriptive means with transaction-level 95% CIs.",
    x = NULL,
    y = "Observed outcome level",
    caption = paste(
      "Success is shown as a percentage; other outcomes are in natural units.",
      "Descriptive figure only; confirmatory inference comes from Script 08 adjusted models."
    )
  ) +
  base_theme() +
  theme(legend.position = "none")
save_plot(p_outcomes_by_channel, "IADB_08c_primary_outcome_levels_by_channel", width = 11.5, height = 6.8)

# ------------------------------------------------------------------------------
# 10. Figures: amount and delivery descriptives --------------------------------
# ------------------------------------------------------------------------------
amount_plot_df <- outcome_summary_by_amount |>
  filter(outcome_label %in% primary_outcomes) |>
  mutate(
    outcome_label_clean = factor(
      recode(outcome_label, !!!outcome_labels, .default = outcome_label),
      levels = unname(outcome_labels[primary_outcomes])
    ),
    mean_plot = mean_display,
    conf.low_plot = conf.low_display,
    conf.high_plot = conf.high_display
  )

p_outcomes_by_amount <- ggplot(
  amount_plot_df,
  aes(x = assigned_amount_label, y = mean_plot)
) +
  geom_col(fill = COLOR_BAR, width = 0.68) +
  geom_errorbar(
    aes(ymin = conf.low_plot, ymax = conf.high_plot),
    width = 0.18,
    linewidth = 0.75,
    color = COLOR_CI
  ) +
  facet_wrap(~ outcome_label_clean, scales = "free_y", nrow = 1) +
  labs(
    title = "Primary outcome levels by assigned amount",
    subtitle = "Unadjusted descriptive means with transaction-level 95% CIs.",
    x = NULL,
    y = "Observed outcome level",
    caption = "Descriptive figure only; confirmatory inference comes from Script 08 adjusted models."
  ) +
  base_theme() +
  theme(legend.position = "none")
save_plot(p_outcomes_by_amount, "IADB_08c_primary_outcome_levels_by_amount", width = 10, height = 6.8)

delivery_plot_df <- outcome_summary_by_delivery |>
  filter(outcome_label %in% primary_outcomes) |>
  mutate(
    outcome_label_clean = factor(
      recode(outcome_label, !!!outcome_labels, .default = outcome_label),
      levels = unname(outcome_labels[primary_outcomes])
    ),
    mean_plot = mean_display,
    conf.low_plot = conf.low_display,
    conf.high_plot = conf.high_display
  )

p_outcomes_by_delivery <- ggplot(
  delivery_plot_df,
  aes(x = assigned_delivery, y = mean_plot)
) +
  geom_col(fill = COLOR_BAR, width = 0.68) +
  geom_errorbar(
    aes(ymin = conf.low_plot, ymax = conf.high_plot),
    width = 0.18,
    linewidth = 0.75,
    color = COLOR_CI
  ) +
  facet_wrap(~ outcome_label_clean, scales = "free_y", nrow = 1) +
  labs(
    title = "Primary outcome levels by assigned delivery mode",
    subtitle = "Unadjusted descriptive means with transaction-level 95% CIs.",
    x = NULL,
    y = "Observed outcome level",
    caption = "Descriptive figure only; confirmatory inference comes from Script 08 adjusted models."
  ) +
  base_theme() +
  theme(legend.position = "none")
save_plot(p_outcomes_by_delivery, "IADB_08c_primary_outcome_levels_by_delivery", width = 10, height = 6.8)

# ------------------------------------------------------------------------------
# 11. Figures: discrete success/KYC distributions ------------------------------
# ------------------------------------------------------------------------------
p_success_dist <- success_distribution_by_channel |>
  mutate(assigned_channel = factor(assigned_channel, levels = channel_levels)) |>
  ggplot(aes(x = assigned_channel, y = share, fill = success_label)) +
  geom_col(width = 0.72, color = "white") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Not successful" = COLOR_ALT, "Successful" = COLOR_BAR_DARK)) +
  labs(
    title = "Transaction success distribution by assigned channel",
    subtitle = "Shares are unadjusted and descriptive.",
    x = NULL,
    y = "Share of transactions",
    caption = "Descriptive figure only; confirmatory inference comes from Script 08 adjusted models."
  ) +
  base_theme()
save_plot(p_success_dist, "IADB_08c_success_distribution_by_channel", width = 8.5, height = 5.8)

p_kyc_dist <- kyc_distribution_by_channel |>
  mutate(assigned_channel = factor(assigned_channel, levels = channel_levels)) |>
  ggplot(aes(x = assigned_channel, y = share, fill = kyc_score_0_3_label)) +
  geom_col(width = 0.72, color = "white") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_brewer(palette = "Greens", direction = 1, na.value = COLOR_BAR) +
  labs(
    title = "KYC score distribution by assigned channel",
    subtitle = "KYC score ranges from 0 to 3; shares are unadjusted and descriptive.",
    x = NULL,
    y = "Share of transactions",
    fill = "KYC score",
    caption = "Descriptive figure only; confirmatory inference comes from Script 08 adjusted models."
  ) +
  base_theme()
save_plot(p_kyc_dist, "IADB_08c_kyc_distribution_by_channel", width = 8.5, height = 5.8)

# ------------------------------------------------------------------------------
# 12. Figures: cost and duration distributions ---------------------------------
# ------------------------------------------------------------------------------
plot_box_outcome <- function(df, outcome_var, output_stem, title, subtitle, y_label) {
  plot_df <- df |>
    filter(!is.na(.data[[outcome_var]]), !is.na(assigned_channel)) |>
    mutate(assigned_channel = factor(assigned_channel, levels = channel_levels))

  p <- ggplot(plot_df, aes(x = assigned_channel, y = .data[[outcome_var]])) +
    geom_boxplot(
      fill = COLOR_BAR,
      color = COLOR_CI,
      outlier.alpha = 0.35,
      width = 0.62
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = y_label,
      caption = "Boxplots are descriptive and unadjusted. Confirmatory inference comes from Script 08 adjusted models."
    ) +
    base_theme() +
    theme(legend.position = "none")

  save_plot(p, output_stem, width = 8.5, height = 5.8)
}

plot_box_outcome(
  df = cost_success_samples$main_strict_slot_level,
  outcome_var = "total_cost_without_time_usd",
  output_stem = "IADB_08c_cost_distribution_by_channel_success_only",
  title = "Transaction cost distribution by assigned channel",
  subtitle = "Successful transactions only; cost excludes time valuation.",
  y_label = "Transaction cost (USD)"
)

plot_box_outcome(
  df = duration_samples$main_strict_slot_level,
  outcome_var = "transaction_duration_hours",
  output_stem = "IADB_08c_duration_distribution_by_channel",
  title = "Transaction duration distribution by assigned channel",
  subtitle = "Successful transactions with observed transaction duration.",
  y_label = "Transaction duration (hours)"
)

# ------------------------------------------------------------------------------
# 13. Optional country descriptive plot ----------------------------------------
# ------------------------------------------------------------------------------
# Country-level success rates are descriptive and can be based on small country
# cells. For the binary success outcome, use Wilson binomial 95% CIs rather than
# normal/t-based CIs so intervals are bounded between 0 and 100%.

pretty_country <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "_", " ")
  x <- stringr::str_to_title(x)
  dplyr::recode(
    x,
    "Argentina" = "Argentina",
    "Brazil" = "Brazil",
    "Chile" = "Chile",
    "Colombia" = "Colombia",
    "Ecuador" = "Ecuador",
    "El Salvador" = "El Salvador",
    "Mexico" = "Mexico",
    "Nicaragua" = "Nicaragua",
    "Panama" = "Panama",
    "Peru" = "Peru",
    .default = x
  )
}

country_success_df <- sap_samples$main_strict_slot_level |>
  filter(!is.na(success), !is.na(country)) |>
  group_by(country) |>
  summarise(
    n = n(),
    successes = sum(success == 1, na.rm = TRUE),
    success_rate = successes / n,
    .groups = "drop"
  ) |>
  mutate(
    # Wilson binomial interval for proportions.
    z = stats::qnorm(0.975),
    denom = 1 + (z^2 / n),
    center = (success_rate + (z^2 / (2 * n))) / denom,
    half_width = (
      z * sqrt((success_rate * (1 - success_rate) / n) + (z^2 / (4 * n^2)))
    ) / denom,
    conf.low_plot = pmax(0, 100 * (center - half_width)),
    conf.high_plot = pmin(100, 100 * (center + half_width)),
    mean_plot = 100 * success_rate,
    country_label = pretty_country(country),
    country_label_n = paste0(country_label, "\n(n=", n, ")"),
    country_label_n = forcats::fct_reorder(country_label_n, mean_plot, .desc = FALSE)
  )

p_country_success <- ggplot(country_success_df, aes(y = country_label_n, x = mean_plot)) +
  geom_col(fill = COLOR_BAR, width = 0.68) +
  geom_errorbarh(
    aes(xmin = conf.low_plot, xmax = conf.high_plot),
    height = 0.18,
    linewidth = 0.75,
    color = COLOR_CI
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Transaction success by country",
    subtitle = "Unadjusted descriptive success rates with Wilson 95% CIs; labels show country sample size.",
    x = "Success rate",
    y = NULL,
    caption = "Country differences are descriptive and not powered as confirmatory country-level effects."
  ) +
  base_theme() +
  theme(legend.position = "none")
save_plot(p_country_success, "IADB_08c_success_by_country_descriptive", width = 8.5, height = 6.2)

save_csv_both(
  country_success_df |>
    select(country, country_label, n, successes, success_rate, mean_plot, conf.low_plot, conf.high_plot),
  "IADB_08c_success_by_country_wilson_ci.csv"
)

# ------------------------------------------------------------------------------
# 14. README / appendix notes --------------------------------------------------
# ------------------------------------------------------------------------------
appendix_notes <- c(
  "# IADB 08c Descriptive Analysis Notes",
  "",
  "This script produces descriptive statistics and outcome-distribution figures for the IADB KYC/AML audit study.",
  "",
  "## Interpretation",
  "",
  "All outputs are descriptive. They show observed distributions, sample composition, and unadjusted outcome levels. They should not be interpreted as causal effects or as substitutes for the PAP/SAP confirmatory models in Script 08.",
  "",
  "## Main files",
  "",
  "- `IADB_08c_sample_overview.csv`: analysis-sample sizes by dataset family.",
  "- `IADB_08c_sample_composition_by_channel.csv`: assigned-channel composition by sample.",
  "- `IADB_08c_outcome_summary_overall.csv`: unadjusted primary outcome means overall.",
  "- `IADB_08c_outcome_summary_by_channel.csv`: unadjusted primary outcome means by assigned channel.",
  "- `IADB_08c_outcome_summary_by_amount.csv`: unadjusted primary outcome means by assigned amount.",
  "- `IADB_08c_outcome_summary_by_delivery.csv`: unadjusted primary outcome means by assigned delivery mode.",
  "- `IADB_08c_kyc_distribution_by_channel.csv`: discrete 0-3 KYC score distribution by channel.",
  "- `IADB_08c_success_distribution_by_channel.csv`: success/failure distribution by channel.",
  "",
  "## Main figures",
  "",
  "- `IADB_08c_primary_outcome_levels_by_channel.png`: descriptive primary outcomes by channel.",
  "- `IADB_08c_primary_outcome_levels_by_amount.png`: descriptive primary outcomes by amount.",
  "- `IADB_08c_primary_outcome_levels_by_delivery.png`: descriptive primary outcomes by delivery mode.",
  "- `IADB_08c_success_distribution_by_channel.png`: success distribution by channel.",
  "- `IADB_08c_kyc_distribution_by_channel.png`: KYC score distribution by channel.",
  "- `IADB_08c_cost_distribution_by_channel_success_only.png`: cost boxplot by channel.",
  "- `IADB_08c_duration_distribution_by_channel.png`: duration boxplot by channel.",
  "",
  "## Relationship to Script 08",
  "",
  "Script 08 remains the source of confirmatory estimates, CR2 confidence intervals, Romano-Wolf adjusted p-values for channel comparisons, and Holm-adjusted p-values for amount/delivery comparisons."
)

writeLines(appendix_notes, file.path(desc_dir, "IADB_08c_descriptive_analysis_notes.md"))

# ------------------------------------------------------------------------------
# 15. Console summary ----------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n=== IADB 08c DESCRIPTIVES COMPLETE ===\n")
cat("Tables saved to:\n")
cat("  ", desc_dir, "\n", sep = "")
cat("Figures saved to:\n")
cat("  ", figure_dir, "\n", sep = "")
cat("\nImportant interpretation note:\n")
cat("  These outputs are descriptive only. Use Script 08 for confirmatory inference.\n")
