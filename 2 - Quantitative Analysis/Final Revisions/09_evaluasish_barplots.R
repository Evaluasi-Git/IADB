# ==============================================================================
# IADB - 09 Plot Script for Boss-Revision Results ------------------------------
# Author: Cedric Antunes / Evaluasi --------------------------------------------
# Date: June 2026 --------------------------------------------------------------
#
# Purpose:
#   Convert Script 08 boss-revision outputs into Evaluasi-style figures for the
#   revised IADB report.
#
# Main changes relative to the original plot script:
#   1. Uses the boss-revision Script 08 outputs in final_etables_boss_revision/.
#   2. Uses any-KYC (kyc_any) as the primary KYC outcome.
#   3. Colors channel bars only if Romano-Wolf FWER p < .05 AND 95% CI excludes 0.
#   4. Colors amount/delivery bars only if Westfall-Young FWER p < .05 AND 95% CI excludes 0.
#   5. Keeps diagnostic/sensitivity figures separate and clearly labeled.
#   6. Adds descriptive, collinearity, Lee-bound, coverage/adherence, temporal,
#      MDE, and natural-scale sensitivity figures.
#
# Required inputs from Script 08 boss revision:
#   data/clean/sap_dataset_builder/final_etables_boss_revision/
#     IADB_08R_core_results_with_mht.csv
#     IADB_08R_core_results_cr2.csv
#     IADB_08R_all_results_cr2.csv
#     IADB_08R_family_B_amount_delivery_RW_crosscheck.csv
#     IADB_08R_collinearity_diagnostics.csv
#     IADB_08R_lee_bounds_cost_duration.csv
#     IADB_08R_coverage_adherence_summary.csv
#     IADB_08R_channel_realization_diagnostics.csv
#     IADB_08R_temporal_permutation.csv
#     IADB_08R_realized_mde_table.csv
#     IADB_08R_functional_form_sensitivity.csv
#
# Optional inputs for descriptive Figures 1 and 2:
#   data/clean/sap_dataset_builder/
#     IADB_sap_observed_first_pass.rds
#     IADB_sap_observed_first_pass_cost_time.rds
#
# Outputs:
#   data/clean/sap_dataset_builder/final_figures_boss_revision/
#     Main-report figures:
#       IADB_09_fig01_descriptive_primary_outcomes_by_channel.{png,pdf}
#       IADB_09_fig02_kyc_score_distribution_by_channel.{png,pdf}
#       IADB_09_fig03_primary_channel_comparisons_RW.{png,pdf}
#       IADB_09_fig04_transaction_characteristics_WY.{png,pdf}
#       IADB_09_fig05_kyc_clean_collinearity_diagnostics.{png,pdf}
#     Appendix figures:
#       IADB_09_app_figA1_channel_effects_by_sample.{png,pdf}
#       IADB_09_app_figA2_channel_effects_by_model.{png,pdf}
#       IADB_09_app_figA3_lee_bounds_cost_duration.{png,pdf}
#       IADB_09_app_figA4_coverage_adherence_realization.{png,pdf}
#       IADB_09_app_figA5_channel_realization_diagnostic.{png,pdf}
#       IADB_09_app_figA6_temporal_order_diagnostics.{png,pdf}
#       IADB_09_app_figA7_realized_mde.{png,pdf}
#       IADB_09_app_figA8_cost_duration_natural_scale_sensitivity.{png,pdf}
#       IADB_09_app_figA9_familyB_WY_RW_crosscheck.{png,pdf}
# ==============================================================================

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(here)
  library(scales)
  library(janitor)
})

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_dir <- here("data", "clean", "sap_dataset_builder")
etable_dir <- file.path(sap_dir, "final_etables_boss_revision")
figure_dir <- file.path(sap_dir, "final_figures_boss_revision")
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

# Helper: first look in final_etables_boss_revision; then allow a local fallback.
# The fallback is useful if you copy the CSVs into the working directory for testing.
find_input <- function(filename, required = TRUE) {
  candidates <- c(
    file.path(etable_dir, filename),
    here(filename),
    file.path(getwd(), filename)
  )
  found <- candidates[file.exists(candidates)][1]
  if (length(found) == 0 || is.na(found)) {
    if (isTRUE(required)) {
      stop("Missing required input file: ", filename,
           "\nLooked in:\n  - ", paste(candidates, collapse = "\n  - "))
    } else {
      return(NA_character_)
    }
  }
  found
}

# Script 08R output files -------------------------------------------------------
core_mht_path      <- find_input("IADB_08R_core_results_with_mht.csv")
core_cr2_path      <- find_input("IADB_08R_core_results_cr2.csv")
all_cr2_path       <- find_input("IADB_08R_all_results_cr2.csv")
familyB_rw_path    <- find_input("IADB_08R_family_B_amount_delivery_RW_crosscheck.csv", required = FALSE)
collinearity_path  <- find_input("IADB_08R_collinearity_diagnostics.csv", required = FALSE)
lee_bounds_path    <- find_input("IADB_08R_lee_bounds_cost_duration.csv", required = FALSE)
coverage_path      <- find_input("IADB_08R_coverage_adherence_summary.csv", required = FALSE)
channel_real_path  <- find_input("IADB_08R_channel_realization_diagnostics.csv", required = FALSE)
temporal_path      <- find_input("IADB_08R_temporal_permutation.csv", required = FALSE)
mde_path           <- find_input("IADB_08R_realized_mde_table.csv", required = FALSE)
functional_path    <- find_input("IADB_08R_functional_form_sensitivity.csv", required = FALSE)

# Optional RDS files for descriptive Figures 1 and 2 ---------------------------
sap_main_path <- file.path(sap_dir, "IADB_sap_observed_first_pass.rds")
ct_main_path  <- file.path(sap_dir, "IADB_sap_observed_first_pass_cost_time.rds")

# ------------------------------------------------------------------------------
# Visual settings --------------------------------------------------------------
# ------------------------------------------------------------------------------
COLOR_NULL <- "#dfe2d2"
COLOR_POSITIVE_SIG <- "#6cbf84"
COLOR_NEGATIVE_SIG <- "#f26968"
COLOR_CI <- "#323339"
COLOR_ZERO <- "#323339"
COLOR_LIGHT_GREEN <- "#e5f5e0"
COLOR_MID_GREEN <- "#a1d99b"
COLOR_DARK_GREEN <- "#006437"
COLOR_TEAL <- "#00BFC4"

BASE_SIZE <- 12

primary_outcomes <- c("success", "kyc_any", "cost_success", "time_duration")

outcome_labels <- c(
  success = "Transaction success\n(p.p.)",
  kyc_any = "Any observed KYC\n(p.p.)",
  cost_success = "Transaction cost\n(USD)",
  time_duration = "Transaction duration\n(hours)",
  kyc_0_3_robustness = "KYC score\n(0-3 points)",
  kyc_enhanced = "Enhanced KYC\n(p.p.)"
)

term_labels <- c(
  MTO = "MTOs\nvs Banks",
  Fintech = "Fintech\nvs Banks",
  Crypto = "Crypto\nvs Banks",
  Amount250 = "USD 250\nvs USD 100",
  Online = "Online\nvs in-person"
)

sample_labels <- c(
  first_pass_observed_primary = "First-pass\nprimary",
  per_protocol_robustness = "Per-protocol\nrobustness",
  reviewed_submissions_robustness = "Reviewed\nsubmissions"
)

model_labels <- c(
  M1_channel_only = "Model 1\nChannel only",
  M2_adjusted_confederate_fe_PRIMARY = "Model 2\nPrimary FE",
  M3_country_fe_exploratory = "Model 3\nCountry FE"
)

channel_levels <- c("Banks", "MTOs", "Fintech", "Crypto")
channel_labels <- c(
  Banks = "Banks",
  MTOs = "MTOs",
  Fintech = "Fintech",
  Crypto = "Crypto"
)

# ------------------------------------------------------------------------------
# Generic helpers --------------------------------------------------------------
# ------------------------------------------------------------------------------
check_required_cols <- function(df, cols, object_name) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(object_name, " is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
}

to_num <- function(x) suppressWarnings(as.numeric(x))

as_logical_safe <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(x == 1)
  x_chr <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  x_chr %in% c("true", "t", "1", "yes", "y")
}

add_missing_cols <- function(df, cols) {
  for (cc in cols) {
    if (!cc %in% names(df)) df[[cc]] <- NA
  }
  df
}

standardize_model_vars <- function(df) {
  df <- df |>
    janitor::clean_names() |>
    add_missing_cols(c(
      "success", "kyc_score", "kyc_score_composite_0_5",
      "total_cost_without_time_usd", "transaction_duration_hours",
      "assigned_channel", "assigned_amount", "assigned_delivery",
      "confederate_match_key", "country"
    ))

  df |>
    mutate(
      success = to_num(success),
      kyc_score_0_3 = to_num(kyc_score),
      kyc_any = case_when(
        is.na(kyc_score_0_3) ~ NA_real_,
        kyc_score_0_3 >= 1 ~ 1,
        kyc_score_0_3 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      assigned_channel = stringr::str_squish(as.character(assigned_channel)),
      assigned_channel = case_when(
        assigned_channel %in% c("Bank", "Banks", "Traditional banks", "Traditional Banks") ~ "Banks",
        assigned_channel %in% c("MTO", "MTOs", "Money transfer", "Money Transfer Operator", "Money Transfer Operators") ~ "MTOs",
        assigned_channel %in% c("Fintech", "FinTech") ~ "Fintech",
        assigned_channel %in% c("Crypto", "Cryptocurrency", "Cryptocurrency exchanges") ~ "Crypto",
        TRUE ~ assigned_channel
      ),
      assigned_channel = factor(assigned_channel, levels = channel_levels),
      assigned_delivery = stringr::str_squish(as.character(assigned_delivery)),
      assigned_delivery = case_when(
        stringr::str_to_lower(assigned_delivery) %in% c("in-person", "in person", "in_person", "person") ~ "In-person",
        stringr::str_to_lower(assigned_delivery) %in% c("online", "digital", "app") ~ "Online",
        TRUE ~ assigned_delivery
      ),
      assigned_amount = to_num(assigned_amount),
      total_cost_without_time_usd = to_num(total_cost_without_time_usd),
      transaction_duration_hours = to_num(transaction_duration_hours)
    )
}

scale_for_plot <- function(df) {
  df |>
    mutate(
      outcome_label_key = as.character(outcome_label),
      term_key = as.character(term),
      plot_scale = if_else(outcome_label_key %in% c("success", "kyc_any", "kyc_enhanced"), 100, 1),
      estimate_plot = estimate * plot_scale,
      conf.low_plot = conf.low * plot_scale,
      conf.high_plot = conf.high * plot_scale,
      outcome_label_clean = recode(outcome_label_key, !!!outcome_labels, .default = outcome_label_key),
      term_clean = recode(term_key, !!!term_labels, .default = term_key),
      outcome_label_clean = factor(outcome_label_clean, levels = unname(outcome_labels[primary_outcomes])),
      term_clean = factor(term_clean, levels = unname(term_labels[c("MTO", "Fintech", "Crypto", "Amount250", "Online")]))
    )
}

ci_excludes_zero_vec <- function(low, high) {
  !is.na(low) & !is.na(high) & ((low > 0 & high > 0) | (low < 0 & high < 0))
}

classify_bar_status <- function(df, p_col, alpha = 0.05, require_ci_excludes_zero = TRUE) {
  if (!p_col %in% names(df)) stop("Requested p-value column not found: ", p_col)

  out <- df |>
    mutate(
      p_for_coloring = .data[[p_col]],
      ci_excludes_zero = ci_excludes_zero_vec(conf.low, conf.high),
      significant_for_coloring = !is.na(p_for_coloring) & p_for_coloring < alpha
    )

  if (isTRUE(require_ci_excludes_zero)) {
    out <- out |> mutate(significant_for_coloring = significant_for_coloring & ci_excludes_zero)
  }

  out |>
    mutate(
      bar_status = case_when(
        significant_for_coloring & estimate > 0 ~ "significant_positive",
        significant_for_coloring & estimate < 0 ~ "significant_negative",
        TRUE ~ "null_or_not_significant"
      ),
      bar_status = factor(
        bar_status,
        levels = c("null_or_not_significant", "significant_positive", "significant_negative")
      )
    )
}

save_plot_both <- function(plot, output_stem, width, height, dpi = 320) {
  png_path <- file.path(figure_dir, paste0(output_stem, ".png"))
  pdf_path <- file.path(figure_dir, paste0(output_stem, ".pdf"))

  ggsave(filename = png_path, plot = plot, width = width, height = height, dpi = dpi)

  # Try cairo PDF for better embedding; fall back to default pdf if unavailable.
  tryCatch(
    ggsave(filename = pdf_path, plot = plot, width = width, height = height, device = grDevices::cairo_pdf),
    error = function(e) ggsave(filename = pdf_path, plot = plot, width = width, height = height, device = "pdf")
  )

  invisible(list(png = png_path, pdf = pdf_path))
}

base_theme <- function() {
  theme_minimal(base_size = BASE_SIZE) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE + 2),
      plot.subtitle = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
      plot.caption = element_text(color = COLOR_CI, size = BASE_SIZE - 3, hjust = 0),
      axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 2),
      axis.text.y = element_text(color = COLOR_CI, size = BASE_SIZE - 2),
      axis.title.y = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
      strip.text = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE - 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

make_fwer_barplot <- function(df, title, subtitle, caption, output_stem, width = 11, height = 6.8) {
  plot_df <- df |>
    scale_for_plot() |>
    filter(!is.na(estimate_plot), !is.na(conf.low_plot), !is.na(conf.high_plot))

  p <- ggplot(plot_df, aes(x = term_clean, y = estimate_plot, fill = bar_status)) +
    geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
    geom_col(width = 0.68, color = NA) +
    geom_errorbar(aes(ymin = conf.low_plot, ymax = conf.high_plot), width = 0.18, linewidth = 0.75, color = COLOR_CI) +
    facet_wrap(~ outcome_label_clean, scales = "free_y", nrow = 1) +
    scale_fill_manual(
      values = c(
        null_or_not_significant = COLOR_NULL,
        significant_positive = COLOR_POSITIVE_SIG,
        significant_negative = COLOR_NEGATIVE_SIG
      ),
      drop = FALSE
    ) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Estimated difference from reference category", caption = caption) +
    base_theme()

  save_plot_both(p, output_stem, width = width, height = height)
  invisible(p)
}

# ------------------------------------------------------------------------------
# Read Script 08R outputs ------------------------------------------------------
# ------------------------------------------------------------------------------
core_mht <- read_csv(core_mht_path, show_col_types = FALSE)
core_cr2 <- read_csv(core_cr2_path, show_col_types = FALSE)
all_cr2 <- read_csv(all_cr2_path, show_col_types = FALSE)

check_required_cols(
  core_mht,
  c("outcome_label", "sample_label", "model_label", "term", "estimate",
    "std.error", "p.value", "conf.low", "conf.high", "p_romano_wolf",
    "p_westfall_young", "p_fwer_primary", "significant_fwer_primary_05"),
  "core_mht"
)

# ------------------------------------------------------------------------------
# Main Figure 1: Descriptive co-primary outcome levels by scheduled channel ----
# ------------------------------------------------------------------------------
if (file.exists(sap_main_path) && file.exists(ct_main_path)) {
  sap_main <- readRDS(sap_main_path) |> standardize_model_vars()
  ct_main <- readRDS(ct_main_path) |> standardize_model_vars()

  make_mean_ci <- function(df, y, outcome_label, multiplier = 1) {
    df |>
      filter(!is.na(assigned_channel), !is.na(.data[[y]])) |>
      group_by(assigned_channel) |>
      summarise(
        n = n(),
        mean = mean(.data[[y]], na.rm = TRUE),
        sd = sd(.data[[y]], na.rm = TRUE),
        se = sd / sqrt(n),
        tcrit = qt(0.975, df = pmax(n - 1, 1)),
        conf.low = mean - tcrit * se,
        conf.high = mean + tcrit * se,
        .groups = "drop"
      ) |>
      mutate(
        outcome_label = outcome_label,
        estimate_plot = mean * multiplier,
        conf.low_plot = conf.low * multiplier,
        conf.high_plot = conf.high * multiplier
      )
  }

  desc_df <- bind_rows(
    make_mean_ci(sap_main, "success", "Transaction success\n(%)", 100),
    make_mean_ci(sap_main, "kyc_any", "Any observed KYC\n(%)", 100),
    make_mean_ci(ct_main, "total_cost_without_time_usd", "Transaction cost\n(USD)", 1),
    make_mean_ci(ct_main, "transaction_duration_hours", "Transaction duration\n(hours)", 1)
  ) |>
    mutate(
      assigned_channel = factor(as.character(assigned_channel), levels = channel_levels, labels = unname(channel_labels)),
      outcome_label = factor(outcome_label, levels = c(
        "Transaction success\n(%)", "Any observed KYC\n(%)", "Transaction cost\n(USD)", "Transaction duration\n(hours)"
      ))
    )

  p_desc <- ggplot(desc_df, aes(x = assigned_channel, y = estimate_plot)) +
    geom_col(width = 0.68, fill = COLOR_NULL) +
    geom_errorbar(aes(ymin = conf.low_plot, ymax = conf.high_plot), width = 0.18, linewidth = 0.75, color = COLOR_CI) +
    facet_wrap(~ outcome_label, scales = "free_y", nrow = 1) +
    labs(
      title = "Primary outcome levels by scheduled channel",
      subtitle = "Unadjusted descriptive means with transaction-level 95% confidence intervals.",
      x = NULL,
      y = "Observed outcome level",
      caption = "Descriptive figure only. Cost and duration are conditional on successful transactions with observed values."
    ) +
    base_theme()

  save_plot_both(p_desc, "IADB_09_fig01_descriptive_primary_outcomes_by_channel", width = 11, height = 6.6)

  # ------------------------------------------------------------------------------
  # Main Figure 2: KYC 0-3 distribution by scheduled channel --------------------
  # ------------------------------------------------------------------------------
  kyc_dist <- sap_main |>
    filter(!is.na(assigned_channel), !is.na(kyc_score_0_3)) |>
    mutate(
      kyc_score_0_3 = factor(as.character(kyc_score_0_3), levels = c("0", "1", "2", "3")),
      assigned_channel = factor(as.character(assigned_channel), levels = channel_levels, labels = unname(channel_labels))
    ) |>
    count(assigned_channel, kyc_score_0_3, name = "n") |>
    group_by(assigned_channel) |>
    mutate(share = n / sum(n)) |>
    ungroup()

  p_kyc_dist <- ggplot(kyc_dist, aes(x = assigned_channel, y = share, fill = kyc_score_0_3)) +
    geom_col(width = 0.68, color = "white", linewidth = 0.25) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(
      values = c("0" = COLOR_LIGHT_GREEN, "1" = COLOR_MID_GREEN, "2" = COLOR_TEAL, "3" = COLOR_DARK_GREEN),
      name = "KYC score"
    ) +
    labs(
      title = "KYC score distribution by scheduled channel",
      subtitle = "The underlying 0-3 score is descriptive; the primary KYC model uses any observed KYC.",
      x = NULL,
      y = "Share of transactions",
      caption = "A score of 0 indicates no observed customer-facing KYC requirement; scores 1-3 are coded as any observed KYC."
    ) +
    theme_minimal(base_size = BASE_SIZE) +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE + 2),
      plot.subtitle = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
      plot.caption = element_text(color = COLOR_CI, size = BASE_SIZE - 3, hjust = 0),
      axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 2),
      axis.text.y = element_text(color = COLOR_CI, size = BASE_SIZE - 2),
      axis.title.y = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  save_plot_both(p_kyc_dist, "IADB_09_fig02_kyc_score_distribution_by_channel", width = 9.5, height = 6.2)

} else {
  warning(
    "Optional descriptive RDS files were not found. Skipping Figure 1 and Figure 2.\n",
    "Expected:\n  - ", sap_main_path, "\n  - ", ct_main_path
  )
}

# ------------------------------------------------------------------------------
# Main Figure 3: Primary channel comparisons using RW FWER ---------------------
# ------------------------------------------------------------------------------
channel_plot_df <- core_mht |>
  filter(
    sample_label == "first_pass_observed_primary",
    model_label == "M2_adjusted_confederate_fe_PRIMARY",
    outcome_label %in% primary_outcomes,
    term %in% c("MTO", "Fintech", "Crypto")
  ) |>
  mutate(p_channel_primary = coalesce(p_fwer_primary, p_romano_wolf)) |>
  classify_bar_status(p_col = "p_channel_primary", alpha = 0.05, require_ci_excludes_zero = TRUE)

p_channel <- make_fwer_barplot(
  df = channel_plot_df,
  title = "Primary channel comparisons relative to Banks",
  subtitle = "Model 2 with confederate fixed effects. Bars are colored only if Romano-Wolf FWER p < 0.05 and the 95% CI excludes zero.",
  caption = paste(
    "Channel contrasts are associational comparisons across pre-existing institution types.",
    "Reference categories: Banks, USD 100, in-person.",
    "Success and any-KYC are shown in percentage points; cost and duration are in natural units."
  ),
  output_stem = "IADB_09_fig03_primary_channel_comparisons_RW",
  width = 11,
  height = 6.8
)

# ------------------------------------------------------------------------------
# Main Figure 4: Amount/delivery comparisons using WY FWER ---------------------
# ------------------------------------------------------------------------------
transaction_plot_df <- core_mht |>
  filter(
    sample_label == "first_pass_observed_primary",
    model_label == "M2_adjusted_confederate_fe_PRIMARY",
    outcome_label %in% primary_outcomes,
    term %in% c("Amount250", "Online")
  ) |>
  mutate(p_transaction_primary = coalesce(p_fwer_primary, p_westfall_young)) |>
  classify_bar_status(p_col = "p_transaction_primary", alpha = 0.05, require_ci_excludes_zero = TRUE)

p_transaction <- make_fwer_barplot(
  df = transaction_plot_df,
  title = "Randomized transaction-characteristic comparisons",
  subtitle = "Model 2 with confederate fixed effects. Bars are colored only if Westfall-Young FWER p < 0.05 and the 95% CI excludes zero.",
  caption = paste(
    "Amount and delivery are randomized transaction-characteristic contrasts where feasible.",
    "Reference categories: USD 100 and in-person.",
    "Success and any-KYC are shown in percentage points; cost and duration are in natural units."
  ),
  output_stem = "IADB_09_fig04_transaction_characteristics_WY",
  width = 9.5,
  height = 6.8
)

# ------------------------------------------------------------------------------
# Main Figure 5: KYC clean collinearity diagnostics ----------------------------
# ------------------------------------------------------------------------------
if (!is.na(collinearity_path)) {
  col_diag <- read_csv(collinearity_path, show_col_types = FALSE)

  kyc_clean <- col_diag |>
    filter(outcome_label == "kyc_any") |>
    filter(
      (sample_label == "online_only_channel_comparison" & term %in% c("MTO", "Fintech", "Crypto")) |
        (sample_label == "banks_mto_only_delivery_comparison" & term %in% c("MTO", "Online"))
    ) |>
    mutate(
      diagnostic = case_when(
        sample_label == "online_only_channel_comparison" ~ "Online-only\nchannel comparison",
        sample_label == "banks_mto_only_delivery_comparison" ~ "Banks/MTO-only\ndelivery comparison",
        TRUE ~ sample_label
      ),
      diagnostic = factor(diagnostic, levels = c("Online-only\nchannel comparison", "Banks/MTO-only\ndelivery comparison")),
      outcome_label = "kyc_any"
    ) |>
    classify_bar_status(p_col = "p.value", alpha = 0.05, require_ci_excludes_zero = TRUE) |>
    scale_for_plot() |>
    filter(!is.na(term_clean))

  p_collinearity <- ggplot(kyc_clean, aes(x = term_clean, y = estimate_plot, fill = bar_status)) +
    geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
    geom_col(width = 0.68, color = NA) +
    geom_errorbar(aes(ymin = conf.low_plot, ymax = conf.high_plot), width = 0.18, linewidth = 0.75, color = COLOR_CI) +
    facet_wrap(~ diagnostic, scales = "free_x", nrow = 1) +
    scale_fill_manual(
      values = c(
        null_or_not_significant = COLOR_NULL,
        significant_positive = COLOR_POSITIVE_SIG,
        significant_negative = COLOR_NEGATIVE_SIG
      ),
      drop = FALSE
    ) +
    labs(
      title = "KYC diagnostics for the channel-by-delivery structure",
      subtitle = "Diagnostic estimates use CR2 CIs. Colored bars indicate raw p < 0.05 and CI excludes zero; these are not confirmatory tests.",
      x = NULL,
      y = "Estimated difference in any observed KYC (p.p.)",
      caption = paste(
        "Online-only comparison drops in-person rows and compares channels among online transactions.",
        "Banks/MTO-only comparison restricts to channels where both delivery modes are observed."
      )
    ) +
    base_theme() +
    theme(axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 3))

  save_plot_both(p_collinearity, "IADB_09_fig05_kyc_clean_collinearity_diagnostics", width = 9.8, height = 6.2)
}

# ------------------------------------------------------------------------------
# Appendix Figure A1: Channel effects across samples ---------------------------
# ------------------------------------------------------------------------------
channel_by_sample_df <- all_cr2 |>
  filter(
    skipped == FALSE,
    outcome_label %in% primary_outcomes,
    model_label == "M2_adjusted_confederate_fe_PRIMARY",
    term %in% c("MTO", "Fintech", "Crypto")
  ) |>
  mutate(
    sample_label_clean = recode(as.character(sample_label), !!!sample_labels, .default = as.character(sample_label)),
    sample_label_clean = factor(sample_label_clean, levels = unname(sample_labels))
  ) |>
  classify_bar_status(p_col = "p.value", alpha = 0.05, require_ci_excludes_zero = TRUE) |>
  scale_for_plot()

p_sample <- ggplot(channel_by_sample_df, aes(x = term_clean, y = estimate_plot, fill = bar_status)) +
  geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
  geom_col(width = 0.68, color = NA) +
  geom_errorbar(aes(ymin = conf.low_plot, ymax = conf.high_plot), width = 0.18, linewidth = 0.75, color = COLOR_CI) +
  facet_grid(outcome_label_clean ~ sample_label_clean, scales = "free_y") +
  scale_fill_manual(
    values = c(null_or_not_significant = COLOR_NULL, significant_positive = COLOR_POSITIVE_SIG, significant_negative = COLOR_NEGATIVE_SIG),
    drop = FALSE
  ) +
  labs(
    title = "Channel comparisons across analysis samples",
    subtitle = "Preferred Model 2. Sensitivity color rule: raw p < 0.05 and 95% CI excludes zero.",
    x = NULL,
    y = "Estimated difference from Banks",
    caption = "Appendix sensitivity figure. Confirmatory claims should use the Romano-Wolf-adjusted primary channel figure."
  ) +
  base_theme() +
  theme(axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 3), strip.text = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE - 2))

save_plot_both(p_sample, "IADB_09_app_figA1_channel_effects_by_sample", width = 12, height = 9)

# ------------------------------------------------------------------------------
# Appendix Figure A2: Channel effects across model specifications --------------
# ------------------------------------------------------------------------------
channel_by_model_df <- core_cr2 |>
  filter(
    skipped == FALSE,
    sample_label == "first_pass_observed_primary",
    outcome_label %in% primary_outcomes,
    term %in% c("MTO", "Fintech", "Crypto"),
    model_label %in% names(model_labels)
  ) |>
  mutate(
    model_label_clean = recode(as.character(model_label), !!!model_labels, .default = as.character(model_label)),
    model_label_clean = factor(model_label_clean, levels = unname(model_labels))
  ) |>
  classify_bar_status(p_col = "p.value", alpha = 0.05, require_ci_excludes_zero = TRUE) |>
  scale_for_plot()

p_model <- ggplot(channel_by_model_df, aes(x = term_clean, y = estimate_plot, fill = bar_status)) +
  geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
  geom_col(width = 0.68, color = NA) +
  geom_errorbar(aes(ymin = conf.low_plot, ymax = conf.high_plot), width = 0.18, linewidth = 0.75, color = COLOR_CI) +
  facet_grid(outcome_label_clean ~ model_label_clean, scales = "free_y") +
  scale_fill_manual(
    values = c(null_or_not_significant = COLOR_NULL, significant_positive = COLOR_POSITIVE_SIG, significant_negative = COLOR_NEGATIVE_SIG),
    drop = FALSE
  ) +
  labs(
    title = "Channel comparisons across model specifications",
    subtitle = "First-pass primary sample. Sensitivity color rule: raw p < 0.05 and 95% CI excludes zero.",
    x = NULL,
    y = "Estimated difference from Banks",
    caption = "Appendix specification-sensitivity figure. Confirmatory claims should use the Romano-Wolf-adjusted primary channel figure."
  ) +
  base_theme() +
  theme(axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 3), strip.text = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE - 2))

save_plot_both(p_model, "IADB_09_app_figA2_channel_effects_by_model", width = 13, height = 9)

# ------------------------------------------------------------------------------
# Appendix Figure A3: Lee-style bounds for cost and duration -------------------
# ------------------------------------------------------------------------------
if (!is.na(lee_bounds_path)) {
  lee <- read_csv(lee_bounds_path, show_col_types = FALSE) |>
    mutate(
      outcome_label = case_when(
        outcome == "total_cost_without_time_usd" ~ "Transaction cost\n(USD)",
        outcome == "transaction_duration_hours" ~ "Transaction duration\n(hours)",
        TRUE ~ outcome
      ),
      channel = factor(channel, levels = c("MTOs", "Fintech", "Crypto"))
    )

  p_lee <- ggplot(lee, aes(x = channel, y = complete_case_difference)) +
    geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
    geom_linerange(aes(ymin = lee_lower, ymax = lee_upper), linewidth = 1.15, color = COLOR_CI) +
    geom_point(size = 2.8, color = COLOR_DARK_GREEN) +
    facet_wrap(~ outcome_label, scales = "free_y", nrow = 1) +
    labs(
      title = "Lee-style bounds for conditional cost and duration",
      subtitle = "Points show complete-case differences relative to Banks; vertical ranges show Lee-style selection bounds.",
      x = NULL,
      y = "Difference from Banks",
      caption = "Appendix sensitivity figure. Bounds assess selection/missingness; they are not confirmatory channel-effect estimates."
    ) +
    base_theme()

  save_plot_both(p_lee, "IADB_09_app_figA3_lee_bounds_cost_duration", width = 9.8, height = 5.8)
}

# ------------------------------------------------------------------------------
# Appendix Figure A4: Coverage/adherence realization by sample -----------------
# ------------------------------------------------------------------------------
if (!is.na(coverage_path)) {
  coverage <- read_csv(coverage_path, show_col_types = FALSE) |>
    mutate(
      sample_clean = recode(as.character(sample), !!!sample_labels, .default = as.character(sample)),
      sample_clean = factor(sample_clean, levels = unname(sample_labels)),
      realization_pct = 100 * realization_rate_vs_canonical
    )

  p_coverage <- ggplot(coverage, aes(x = sample_clean, y = realization_pct)) +
    geom_col(width = 0.65, fill = COLOR_NULL) +
    geom_text(aes(label = paste0(round(realization_pct, 1), "%")), vjust = -0.35, size = 3.5, color = COLOR_CI) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(coverage$realization_pct, na.rm = TRUE) * 1.2)) +
    labs(
      title = "Sample realization relative to canonical schedule slots",
      subtitle = "Canonical denominator = 980 scheduled transaction slots.",
      x = NULL,
      y = "Realization rate",
      caption = "Appendix implementation figure. Coverage/adherence is treated as a selection diagnostic."
    ) +
    base_theme()

  save_plot_both(p_coverage, "IADB_09_app_figA4_coverage_adherence_realization", width = 8.5, height = 5.8)
}

# ------------------------------------------------------------------------------
# Appendix Figure A5: Channel realization diagnostic ---------------------------
# ------------------------------------------------------------------------------
if (!is.na(channel_real_path)) {
  channel_real <- read_csv(channel_real_path, show_col_types = FALSE) |>
    mutate(
      assigned_channel = factor(assigned_channel, levels = channel_levels),
      realization_pct = 100 * realization_rate
    )

  p_channel_real <- ggplot(channel_real, aes(x = assigned_channel, y = realization_pct)) +
    geom_col(width = 0.65, fill = COLOR_NULL) +
    geom_text(aes(label = paste0(round(realization_pct, 1), "%")), vjust = -0.35, size = 3.5, color = COLOR_CI) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(channel_real$realization_pct, na.rm = TRUE) * 1.2)) +
    labs(
      title = "Realized transaction slots by scheduled channel",
      subtitle = "Diagnostic realization rates by channel.",
      x = NULL,
      y = "Realization rate",
      caption = paste(unique(channel_real$diagnostic_basis), collapse = " ")
    ) +
    base_theme()

  save_plot_both(p_channel_real, "IADB_09_app_figA5_channel_realization_diagnostic", width = 8.5, height = 5.8)
}

# ------------------------------------------------------------------------------
# Appendix Figure A6: Temporal/order diagnostics -------------------------------
# ------------------------------------------------------------------------------
if (!is.na(temporal_path)) {
  temporal <- read_csv(temporal_path, show_col_types = FALSE) |>
    mutate(
      # Fix duplicated labels from an earlier flattening bug, e.g. success.success -> success.
      outcome_label_clean_key = str_replace(outcome_label, "^([^.]+)\\.\\1$", "\\1"),
      outcome_clean = recode(outcome_label_clean_key, !!!outcome_labels, .default = outcome_label_clean_key),
      term_clean = case_when(
        term == "seqpos_centered" ~ "Sequence position",
        str_detect(term, ":late$") ~ str_replace(term, ":late$", " × late"),
        TRUE ~ term
      ),
      significant = p_temporal_permutation < 0.05
    )

  p_temporal <- ggplot(temporal, aes(x = reorder(term_clean, p_temporal_permutation), y = p_temporal_permutation)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", linewidth = 0.45, color = COLOR_ZERO) +
    geom_point(size = 2.5, color = COLOR_DARK_GREEN) +
    coord_flip() +
    facet_wrap(~ outcome_clean, scales = "free_y") +
    scale_y_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
    labs(
      title = "Temporal/order permutation diagnostics",
      subtitle = "Exploratory p-values from within-confederate order permutations. Dashed line marks p = 0.05.",
      x = NULL,
      y = "Permutation p-value",
      caption = "Appendix diagnostic figure. These tests assess sequence/time contamination, not channel effects."
    ) +
    base_theme() +
    theme(legend.position = "none")

  save_plot_both(p_temporal, "IADB_09_app_figA6_temporal_order_diagnostics", width = 11, height = 8)
}

# ------------------------------------------------------------------------------
# Appendix Figure A7: Realized MDEs -------------------------------------------
# ------------------------------------------------------------------------------
if (!is.na(mde_path)) {
  mde <- read_csv(mde_path, show_col_types = FALSE) |>
    filter(outcome_label %in% primary_outcomes, term %in% c("MTO", "Fintech", "Crypto", "Amount250", "Online")) |>
    mutate(
      plot_scale = if_else(outcome_label %in% c("success", "kyc_any"), 100, 1),
      mde_plot = mde_approx * plot_scale,
      outcome_clean = recode(outcome_label, !!!outcome_labels, .default = outcome_label),
      term_clean = recode(term, !!!term_labels, .default = term),
      outcome_clean = factor(outcome_clean, levels = unname(outcome_labels[primary_outcomes])),
      term_clean = factor(term_clean, levels = unname(term_labels[c("MTO", "Fintech", "Crypto", "Amount250", "Online")]))
    )

  p_mde <- ggplot(mde, aes(x = term_clean, y = mde_plot)) +
    geom_col(width = 0.65, fill = COLOR_NULL) +
    facet_wrap(~ outcome_clean, scales = "free_y", nrow = 1) +
    labs(
      title = "Approximate realized minimum detectable effects",
      subtitle = "MDE approximation uses (1.96 + 0.84) × CR2 standard error for each primary coefficient.",
      x = NULL,
      y = "Approximate MDE",
      caption = "Appendix diagnostic figure. Use for null-result phrasing, not as an exact prospective power calculation."
    ) +
    base_theme() +
    theme(axis.text.x = element_text(angle = 45,color = COLOR_CI, size = BASE_SIZE - 3))

  save_plot_both(p_mde, "IADB_09_app_figA7_realized_mde", width = 12, height = 6.4)
}

# ------------------------------------------------------------------------------
# Appendix Figure A8: Natural-scale cost/duration sensitivity ------------------
# ------------------------------------------------------------------------------
if (!is.na(functional_path)) {
  functional <- read_csv(functional_path, show_col_types = FALSE) |>
    filter(
      model_label == "M2_adjusted_confederate_fe_PRIMARY",
      term %in% c("MTO", "Fintech", "Crypto"),
      str_detect(outcome_label, "^(cost_success|time_duration)__"),
      !str_detect(outcome_label, "y_log1p|y_asinh")
    ) |>
    mutate(
      outcome_family = case_when(
        str_detect(outcome_label, "^cost_success") ~ "Transaction cost\n(USD)",
        str_detect(outcome_label, "^time_duration") ~ "Transaction duration\n(hours)",
        TRUE ~ outcome_label
      ),
      specification = case_when(
        str_detect(outcome_label, "__y_raw$") ~ "Raw",
        str_detect(outcome_label, "__y_winsor_01_99$") ~ "Winsor 1-99",
        str_detect(outcome_label, "__y_winsor_05_95$") ~ "Winsor 5-95",
        str_detect(outcome_label, "__iqr_trim$") ~ "IQR trim",
        TRUE ~ outcome_label
      ),
      specification = factor(specification, levels = c("Raw", "Winsor 1-99", "Winsor 5-95", "IQR trim")),
      term_clean = recode(term, !!!term_labels, .default = term)
    ) |>
    classify_bar_status(p_col = "p.value", alpha = 0.05, require_ci_excludes_zero = TRUE)

  p_functional <- ggplot(functional, aes(x = specification, y = estimate, fill = bar_status)) +
    geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
    geom_col(width = 0.65, color = NA) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.16, linewidth = 0.65, color = COLOR_CI) +
    facet_grid(outcome_family ~ term_clean, scales = "free_y") +
    scale_fill_manual(
      values = c(null_or_not_significant = COLOR_NULL, significant_positive = COLOR_POSITIVE_SIG, significant_negative = COLOR_NEGATIVE_SIG),
      drop = FALSE
    ) +
    labs(
      title = "Natural-scale cost and duration sensitivity checks",
      subtitle = "Raw, winsorized, and trimmed outcomes. Log/asinh transformations are omitted because coefficients are not in the same units.",
      x = NULL,
      y = "Estimated difference from Banks",
      caption = "Appendix sensitivity figure. Color rule: raw p < 0.05 and 95% CI excludes zero; not confirmatory."
    ) +
    base_theme() +
    theme(axis.text.x = element_text(angle = 35, hjust = 1, color = COLOR_CI, size = BASE_SIZE - 3))

  save_plot_both(p_functional, "IADB_09_app_figA8_cost_duration_natural_scale_sensitivity", width = 12, height = 7.2)
}

# ------------------------------------------------------------------------------
# Appendix Figure A9: Family B WY vs RW cross-check ----------------------------
# ------------------------------------------------------------------------------
if (!is.na(familyB_rw_path)) {
  famB_rw <- read_csv(familyB_rw_path, show_col_types = FALSE)

  famB_compare <- core_mht |>
    filter(
      sample_label == "first_pass_observed_primary",
      model_label == "M2_adjusted_confederate_fe_PRIMARY",
      outcome_label %in% primary_outcomes,
      term %in% c("Amount250", "Online")
    ) |>
    select(outcome_label, term, p_westfall_young) |>
    left_join(
      famB_rw |>
        filter(outcome_label %in% primary_outcomes, term %in% c("Amount250", "Online")) |>
        select(outcome_label, term, p_romano_wolf),
      by = c("outcome_label", "term")
    ) |>
    pivot_longer(
      cols = c(p_westfall_young, p_romano_wolf),
      names_to = "method",
      values_to = "p_adjusted"
    ) |>
    mutate(
      method = recode(method, p_westfall_young = "Westfall-Young\nprimary", p_romano_wolf = "Romano-Wolf\ncross-check"),
      outcome_clean = recode(outcome_label, !!!outcome_labels, .default = outcome_label),
      term_clean = recode(term, !!!term_labels, .default = term),
      outcome_clean = factor(outcome_clean, levels = unname(outcome_labels[primary_outcomes])),
      method = factor(method, levels = c("Westfall-Young\nprimary", "Romano-Wolf\ncross-check"))
    )

  p_famb <- ggplot(famB_compare, aes(x = method, y = p_adjusted)) +
    geom_hline(yintercept = 0.05, linetype = "dashed", linewidth = 0.45, color = COLOR_ZERO) +
    geom_col(width = 0.65, fill = COLOR_NULL) +
    facet_grid(outcome_clean ~ term_clean) +
    scale_y_continuous(limits = c(0, 1), labels = number_format(accuracy = 0.01)) +
    labs(
      title = "Family B adjusted p-values: Westfall-Young and Romano-Wolf cross-check",
      subtitle = "Westfall-Young is the primary design-based procedure for amount/delivery; Romano-Wolf is an asymptotic cross-check.",
      x = NULL,
      y = "Adjusted p-value",
      caption = "Appendix diagnostic figure. Dashed line marks p = 0.05."
    ) +
    base_theme() +
    theme(axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 4))

  save_plot_both(p_famb, "IADB_09_app_figA9_familyB_WY_RW_crosscheck", width = 11, height = 9)
}

# ------------------------------------------------------------------------------
# Console summary --------------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n=== IADB SCRIPT 09 BOSS-REVISION PLOTS COMPLETE ===\n")
cat("Figures saved to:\n")
cat("  ", figure_dir, "\n", sep = "")
cat("\nMain-report figures:\n")
cat("  1. IADB_09_fig01_descriptive_primary_outcomes_by_channel\n")
cat("  2. IADB_09_fig02_kyc_score_distribution_by_channel\n")
cat("  3. IADB_09_fig03_primary_channel_comparisons_RW\n")
cat("  4. IADB_09_fig04_transaction_characteristics_WY\n")
cat("  5. IADB_09_fig05_kyc_clean_collinearity_diagnostics\n")
cat("\nAppendix figures:\n")
cat("  A1. Channel effects by sample\n")
cat("  A2. Channel effects by model\n")
cat("  A3. Lee bounds\n")
cat("  A4. Coverage/adherence realization\n")
cat("  A5. Channel realization diagnostic\n")
cat("  A6. Temporal/order diagnostics\n")
cat("  A7. Realized MDEs\n")
cat("  A8. Natural-scale cost/duration sensitivity\n")
cat("  A9. Family B WY/RW cross-check\n")
cat("\nColoring rules:\n")
cat("  Main channel figure: Romano-Wolf FWER p < .05 AND 95% CI excludes zero.\n")
cat("  Main amount/delivery figure: Westfall-Young FWER p < .05 AND 95% CI excludes zero.\n")
cat("  Diagnostics/sensitivity figures: raw p < .05 AND 95% CI excludes zero, unless otherwise noted.\n")
