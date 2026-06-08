# ==============================================================================
# IADB - 09 Plot Script 08 PAP/SAP Results -------------------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: June 2026 --------------------------------------------------------------
#
# Purpose:
#   Convert Script 08 PAP/SAP coefficient and multiplicity outputs into
#   Evaluasih vertical barplots with 95% confidence intervals.
#
# Robust reporting rule used here:
#   - Main confirmatory channel plots are colored significant only if:
#       Romano-Wolf adjusted p < 0.05 AND the plotted 95% CI excludes zero.
#   - Main confirmatory transaction-characteristic plots are colored significant only if:
#       Holm adjusted p < 0.05 AND the plotted 95% CI excludes zero.
#   - Appendix/sensitivity plots are colored only if raw p < 0.05 AND the plotted
#       95% CI excludes zero; they are explicitly sensitivity/exploratory plots.
#   - No raw p < 0.10 color rule is used in this script.
#
# Evaluasi visual rule:
#   - Null / not significant effect:        #dfe2d2
#   - Significant positive effect:          #6cbf84
#   - Significant negative effect:          #f26968
#   - 95% CI bars:                          #323339
#
# Inputs from Script 08:
#   data/clean/sap_dataset_builder/final_etables/
#     IADB_08_pap_primary_models_cr2.csv
#     IADB_08_pap_all_models_cr2.csv
#     IADB_08_multiplicity_channel_family.csv
#     IADB_08_multiplicity_transaction_family.csv
#     IADB_08_primary_outcome_definition_log.csv
#
# Outputs:
#   data/clean/sap_dataset_builder/final_figures/
#     IADB_08_plot_primary_channel_effects_pap_adjusted_robust.{png,pdf}
#     IADB_08_plot_primary_transaction_effects_pap_adjusted_robust.{png,pdf}
#     IADB_08_plot_channel_effects_by_sample_p05_sensitivity_robust.{png,pdf}
#     IADB_08_plot_channel_effects_by_model_p05_sensitivity_robust.{png,pdf}
# ===============================================================================

# Cleaning my environment
rm(list = ls())

# Managing memory
gc()

# Required packeages -----------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(here)
  library(scales)
})

# ------------------------------------------------------------------------------
# Paths ------------------------------------------------------------------------
# ------------------------------------------------------------------------------
sap_dir <- here("data", "clean", "sap_dataset_builder")
etable_dir <- file.path(sap_dir, "final_etables")
figure_dir <- file.path(sap_dir, "final_figures")

dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

primary_results_path <- file.path(etable_dir, "IADB_08_pap_primary_models_cr2.csv")
all_results_path <- file.path(etable_dir, "IADB_08_pap_all_models_cr2.csv")
channel_family_path <- file.path(etable_dir, "IADB_08_multiplicity_channel_family.csv")
transaction_family_path <- file.path(etable_dir, "IADB_08_multiplicity_transaction_family.csv")
outcome_log_path <- file.path(etable_dir, "IADB_08_primary_outcome_definition_log.csv")

required_files <- c(
  primary_results_path,
  all_results_path,
  channel_family_path,
  transaction_family_path,
  outcome_log_path
)

# ------------------------------------------------------------------------------
# Visual settings --------------------------------------------------------------
# ------------------------------------------------------------------------------
COLOR_NULL <- "#dfe2d2"
COLOR_POSITIVE_SIG <- "#6cbf84"
COLOR_NEGATIVE_SIG <- "#f26968"
COLOR_CI <- "#323339"
COLOR_ZERO <- "#323339"

BASE_SIZE <- 12

primary_outcomes <- c(
  "success",
  "kyc_0_3",
  "cost_success",
  "time_duration"
)

outcome_labels <- c(
  success = "Transaction success\n(p.p.)",
  kyc_0_3 = "KYC score\n(0-3 points)",
  cost_success = "Transaction cost\n(USD)",
  time_duration = "Transaction duration\n(hours)",
  cost_any_sensitivity = "Any-attempt cost\n(USD)",
  reported_time_sensitivity = "Reported time\n(hours)",
  interaction_time_sensitivity = "Interaction time\n(hours)",
  kyc_0_5_sensitivity = "KYC composite\n(0-5 points)"
)

term_labels <- c(
  MTO = "MTOs\nvs Banks",
  Fintech = "Fintech\nvs Banks",
  Crypto = "Crypto\nvs Banks",
  Amount250 = "USD 250\nvs USD 100",
  Online = "Online\nvs in-person"
)

model_labels <- c(
  M1_unadjusted = "Model 1\nUnadjusted",
  M2_adjusted = "Model 2\nAdjusted",
  M3_country_fe = "Model 3\nCountry FE",
  M2_confederate_fe = "Preferred\nConfed. FE"
)

sample_labels <- c(
  main_strict_slot_level = "Main strict\nslot-level",
  per_protocol_strict_slot = "Per-protocol\nstrict slot",
  reviewed_submissions = "Reviewed\nsubmissions"
)

# ------------------------------------------------------------------------------
# Reading Script 08 outputs ----------------------------------------------------
# ------------------------------------------------------------------------------
primary_results <- read_csv(primary_results_path, show_col_types = FALSE)
all_results <- read_csv(all_results_path, show_col_types = FALSE)
channel_family <- read_csv(channel_family_path, show_col_types = FALSE)
transaction_family <- read_csv(transaction_family_path, show_col_types = FALSE)
outcome_log <- read_csv(outcome_log_path, show_col_types = FALSE)

# ------------------------------------------------------------------------------
# Helpers ----------------------------------------------------------------------
# ------------------------------------------------------------------------------
check_required_cols <- function(df, cols, object_name) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      object_name, " is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
}

check_required_cols(
  primary_results,
  c("outcome_label", "sample_label", "model_label", "term", "estimate",
    "std.error", "p.value", "conf.low", "conf.high", "skipped"),
  "primary_results"
)

check_required_cols(
  channel_family,
  c("outcome_label", "term", "estimate", "p.value", "conf.low", "conf.high",
    "p_romano_wolf", "significant_romano_wolf_05"),
  "channel_family"
)

check_required_cols(
  transaction_family,
  c("outcome_label", "term", "estimate", "p.value", "conf.low", "conf.high",
    "p_holm", "significant_holm_05"),
  "transaction_family"
)

scale_for_plot <- function(df) {
  df |>
    mutate(
      outcome_label_key = as.character(outcome_label),
      term_key = as.character(term),
      plot_scale = if_else(outcome_label_key == "success", 100, 1),
      estimate_plot = estimate * plot_scale,
      conf.low_plot = conf.low * plot_scale,
      conf.high_plot = conf.high * plot_scale,
      outcome_label_clean = recode(
        outcome_label_key,
        !!!outcome_labels,
        .default = outcome_label_key
      ),
      term_clean = recode(
        term_key,
        !!!term_labels,
        .default = term_key
      ),
      outcome_label_clean = factor(
        outcome_label_clean,
        levels = unname(outcome_labels[primary_outcomes])
      ),
      term_clean = factor(
        term_clean,
        levels = unname(term_labels[c("MTO", "Fintech", "Crypto", "Amount250", "Online")])
      )
    )
}

classify_bar_status <- function(df, p_col, alpha = 0.05, require_ci_excludes_zero = TRUE) {
  if (!p_col %in% names(df)) {
    stop("Requested p-value column not found: ", p_col)
  }

  out <- df |>
    mutate(
      p_for_coloring = .data[[p_col]],
      alpha_for_coloring = alpha,
      ci_excludes_zero = !is.na(conf.low) & !is.na(conf.high) &
        ((conf.low > 0 & conf.high > 0) | (conf.low < 0 & conf.high < 0)),
      significant_for_coloring = !is.na(p_for_coloring) & p_for_coloring < alpha
    )

  # require_ci_excludes_zero is a scalar function option, not a row-level column.
  # Use ordinary if/else rather than dplyr::if_else(), which expects vectorized
  # true/false branches with sizes matching the condition.
  if (isTRUE(require_ci_excludes_zero)) {
    out <- out |>
      mutate(significant_for_coloring = significant_for_coloring & ci_excludes_zero)
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
        levels = c(
          "null_or_not_significant",
          "significant_positive",
          "significant_negative"
        )
      )
    )
}

make_barplot <- function(
    df,
    title,
    subtitle,
    caption,
    output_stem,
    width = 11,
    height = 6.8
) {
  plot_df <- df |>
    scale_for_plot() |>
    filter(!is.na(estimate_plot), !is.na(conf.low_plot), !is.na(conf.high_plot))

  p <- ggplot(plot_df, aes(x = term_clean, y = estimate_plot, fill = bar_status)) +
    geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
    geom_col(width = 0.68, color = NA) +
    geom_errorbar(
      aes(ymin = conf.low_plot, ymax = conf.high_plot),
      width = 0.18,
      linewidth = 0.75,
      color = COLOR_CI
    ) +
    facet_wrap(~ outcome_label_clean, scales = "free_y", nrow = 1) +
    scale_fill_manual(
      values = c(
        null_or_not_significant = COLOR_NULL,
        significant_positive = COLOR_POSITIVE_SIG,
        significant_negative = COLOR_NEGATIVE_SIG
      ),
      drop = FALSE
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Estimated difference from reference category",
      caption = caption
    ) +
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

  ggsave(
    filename = file.path(figure_dir, paste0(output_stem, ".png")),
    plot = p,
    width = width,
    height = height,
    dpi = 320
  )

  ggsave(
    filename = file.path(figure_dir, paste0(output_stem, ".pdf")),
    plot = p,
    width = width,
    height = height,
    device = cairo_pdf
  )

  invisible(p)
}

# ------------------------------------------------------------------------------
# Primary confirmatory plots: PAP-adjusted significance at alpha = 0.05 --------
# ------------------------------------------------------------------------------
channel_pap_plot_df <- channel_family |>
  filter(
    outcome_label %in% primary_outcomes,
    term %in% c("MTO", "Fintech", "Crypto")
  ) |>
  classify_bar_status(
    p_col = "p_romano_wolf",
    alpha = 0.05,
    require_ci_excludes_zero = TRUE
  )

p_channel_pap <- make_barplot(
  df = channel_pap_plot_df,
  title = "Primary channel effects relative to Banks",
  subtitle = "95% CIs use CR2. Bars are colored only if Romano-Wolf p < 0.05 and the 95% CI excludes zero.",
  caption = paste(
    "Reference categories: Banks, USD 100, in-person.",
    "Success effects are shown in percentage points; other outcomes are in natural units.",
    "Positive significant = green; negative significant = red; null/not significant = grey."
  ),
  output_stem = "IADB_08_plot_primary_channel_effects_pap_adjusted_robust"
)

transaction_pap_plot_df <- transaction_family |>
  filter(
    outcome_label %in% primary_outcomes,
    term %in% c("Amount250", "Online")
  ) |>
  classify_bar_status(
    p_col = "p_holm",
    alpha = 0.05,
    require_ci_excludes_zero = TRUE
  )

p_transaction_pap <- make_barplot(
  df = transaction_pap_plot_df,
  title = "Primary transaction-characteristic effects",
  subtitle = "95% CIs use CR2. Bars are colored only if Holm p < 0.05 and the 95% CI excludes zero.",
  caption = paste(
    "Reference categories: USD 100 and in-person.",
    "Success effects are shown in percentage points; other outcomes are in natural units.",
    "Positive significant = green; negative significant = red; null/not significant = grey."
  ),
  output_stem = "IADB_08_plot_primary_transaction_effects_pap_adjusted_robust",
  width = 9.5,
  height = 6.8
)

# ------------------------------------------------------------------------------
# Appendix/sensitivity plot: channel effects across samples --------------------
# ------------------------------------------------------------------------------
channel_by_sample_df <- all_results |>
  filter(
    skipped == FALSE,
    outcome_label %in% primary_outcomes,
    model_label == "M2_confederate_fe",
    term %in% c("MTO", "Fintech", "Crypto")
  ) |>
  mutate(
    sample_label_clean = recode(as.character(sample_label), !!!sample_labels, .default = as.character(sample_label)),
    sample_label_clean = factor(sample_label_clean, levels = unname(sample_labels))
  ) |>
  classify_bar_status(
    p_col = "p.value",
    alpha = 0.05,
    require_ci_excludes_zero = TRUE
  ) |>
  scale_for_plot()

p_sample <- ggplot(
  channel_by_sample_df,
  aes(x = term_clean, y = estimate_plot, fill = bar_status)
) +
  geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
  geom_col(width = 0.68, color = NA) +
  geom_errorbar(
    aes(ymin = conf.low_plot, ymax = conf.high_plot),
    width = 0.18,
    linewidth = 0.75,
    color = COLOR_CI
  ) +
  facet_grid(outcome_label_clean ~ sample_label_clean, scales = "free_y") +
  scale_fill_manual(
    values = c(
      null_or_not_significant = COLOR_NULL,
      significant_positive = COLOR_POSITIVE_SIG,
      significant_negative = COLOR_NEGATIVE_SIG
    ),
    drop = FALSE
  ) +
  labs(
    title = "Channel effects across analysis samples",
    subtitle = "Preferred confederate-FE model. Sensitivity color rule: raw p < 0.05 and 95% CI excludes zero.",
    x = NULL,
    y = "Estimated difference from Banks",
    caption = paste(
      "Reference categories: Banks, USD 100, in-person.",
      "Success effects are shown in percentage points; other outcomes are in natural units.",
      "This figure is for sensitivity checks; confirmatory claims should use the PAP-adjusted figure."
    )
  ) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE + 2),
    plot.subtitle = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
    plot.caption = element_text(color = COLOR_CI, size = BASE_SIZE - 3, hjust = 0),
    axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 3),
    axis.text.y = element_text(color = COLOR_CI, size = BASE_SIZE - 2),
    axis.title.y = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
    strip.text = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE - 2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Saving in .png for final report
ggsave(
  filename = file.path(figure_dir, "IADB_08_plot_channel_effects_by_sample_p05_sensitivity_robust.png"),
  plot = p_sample,
  width = 12,
  height = 9,
  dpi = 320
)

# Saving in .pdf too for emailing and internal circulation
ggsave(
  filename = file.path(figure_dir, "IADB_08_plot_channel_effects_by_sample_p05_sensitivity_robust.pdf"),
  plot = p_sample,
  width = 12,
  height = 9,
  device = cairo_pdf
)

# ------------------------------------------------------------------------------
# Appendix/sensitivity plot: channel effects across model specifications -------
# ------------------------------------------------------------------------------
channel_by_model_df <- primary_results |>
  filter(
    skipped == FALSE,
    sample_label == "main_strict_slot_level",
    outcome_label %in% primary_outcomes,
    term %in% c("MTO", "Fintech", "Crypto"),
    model_label %in% c(
      "M1_unadjusted",
      "M2_adjusted",
      "M3_country_fe",
      "M2_confederate_fe"
    )
  ) |>
  mutate(
    model_label_clean = recode(as.character(model_label), !!!model_labels, .default = as.character(model_label)),
    model_label_clean = factor(model_label_clean, levels = unname(model_labels))
  ) |>
  classify_bar_status(
    p_col = "p.value",
    alpha = 0.05,
    require_ci_excludes_zero = TRUE
  ) |>
  scale_for_plot()

p_model <- ggplot(
  channel_by_model_df,
  aes(x = term_clean, y = estimate_plot, fill = bar_status)
) +
  geom_hline(yintercept = 0, linewidth = 0.45, color = COLOR_ZERO) +
  geom_col(width = 0.68, color = NA) +
  geom_errorbar(
    aes(ymin = conf.low_plot, ymax = conf.high_plot),
    width = 0.18,
    linewidth = 0.75,
    color = COLOR_CI
  ) +
  facet_grid(outcome_label_clean ~ model_label_clean, scales = "free_y") +
  scale_fill_manual(
    values = c(
      null_or_not_significant = COLOR_NULL,
      significant_positive = COLOR_POSITIVE_SIG,
      significant_negative = COLOR_NEGATIVE_SIG
    ),
    drop = FALSE
  ) +
  labs(
    title = "Channel effects across PAP/SAP model specifications",
    subtitle = "Main strict slot-level sample. Sensitivity color rule: raw p < 0.05 and 95% CI excludes zero.",
    x = NULL,
    y = "Estimated difference from Banks",
    caption = paste(
      "Reference categories: Banks, USD 100, in-person.",
      "Success effects are shown in percentage points; other outcomes are in natural units.",
      "This figure is for specification sensitivity; confirmatory claims should use the PAP-adjusted figure."
    )
  ) +
  theme_minimal(base_size = BASE_SIZE) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE + 2),
    plot.subtitle = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
    plot.caption = element_text(color = COLOR_CI, size = BASE_SIZE - 3, hjust = 0),
    axis.text.x = element_text(color = COLOR_CI, size = BASE_SIZE - 3),
    axis.text.y = element_text(color = COLOR_CI, size = BASE_SIZE - 2),
    axis.title.y = element_text(color = COLOR_CI, size = BASE_SIZE - 1),
    strip.text = element_text(face = "bold", color = COLOR_CI, size = BASE_SIZE - 2),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = file.path(figure_dir, "IADB_08_plot_channel_effects_by_model_p05_sensitivity_robust.png"),
  plot = p_model,
  width = 13,
  height = 9,
  dpi = 320
)

ggsave(
  filename = file.path(figure_dir, "IADB_08_plot_channel_effects_by_model_p05_sensitivity_robust.pdf"),
  plot = p_model,
  width = 13,
  height = 9,
  device = cairo_pdf
)

# ------------------------------------------------------------------------------
# Console summary --------------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n=== IADB SCRIPT 08 CLIENT PLOTS COMPLETE: ROBUST VERSION ===\n")
cat("Figures saved to:\n")
cat("  ", figure_dir, "\n", sep = "")
cat("\nClient-facing significance rule:\n")
cat("  Channel effects: Romano-Wolf p < 0.05 AND 95% CI excludes zero.\n")
cat("  Amount/Online effects: Holm p < 0.05 AND 95% CI excludes zero.\n")
cat("  Sensitivity figures: raw p < 0.05 AND 95% CI excludes zero; not confirmatory.\n")
cat("  Raw p < 0.10 is not used for coloring in this robust client version.\n")
