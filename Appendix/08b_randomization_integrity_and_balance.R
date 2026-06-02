# ==============================================================================
# IADB - 08b Randomization Integrity and Schedule Audit ------------------------
# Author: Cedric Antunes (Evaluasi) --------------------------------------------
# Date: June 2026 --------------------------------------------------------------
#
# Purpose:
#   Document and audit the randomization schedules used in the IADB KYC/AML audit.
#
# What this script does:
#   1. Reads all available master schedule CSVs.
#   2. Classifies schedules as active/canonical vs archival/superseded.
#   3. Harmonizes variable names and adds explicit randomization-wave labels.
#   4. Checks channel, amount, delivery, block/week, and consecutive-channel balance.
#   5. Documents the supplemental Laura constraint: no Bank assignments in the
#      May 22--31 supplemental wave.
#   6. Exports schedule diagnostics that can be used in the PAP/SAP appendix.
# ===============================================================================

# ------------------------------------------------------------------------------
# 0. Setup ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(here)
  library(janitor)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# 1. Paths ---------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Place the schedule CSVs either in the project root or in this folder.
schedule_dir <- here("data", "raw", "randomization_schedules")
out_dir <- here("data", "clean", "sap_dataset_builder", "randomization_audit")

dir.create(schedule_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Helper: find file either in schedule_dir or project root.
resolve_schedule_path <- function(file_name) {
  candidate_paths <- c(
    file.path(schedule_dir, file_name),
    here(file_name)
  )

  existing <- candidate_paths[file.exists(candidate_paths)]

  if (length(existing) == 0) {
    return(NA_character_)
  }

  existing[[1]]
}

# ------------------------------------------------------------------------------
# 2. Schedule registry ----------------------------------------------------------
# ------------------------------------------------------------------------------
# Revise include_in_audit if your fieldwork records indicate that an archival file
# was actually distributed and used.
#
# Current interpretation of uploaded files:
#   - master_schedule_feb26.csv is the named V2/original schedule. The Feb13 file
#     appears identical and is kept as archival.
#   - master_schedule_apr24_may25.csv is the late-April replacement/additional
#     confederate wave.
#   - master_schedule_supplemental_may22_may31.csv is the revised supplemental
#     wave, including Laura's no-Bank constraint. The May21 supplemental file is
#     kept as superseded because it still assigns Laura to Bank.
#   - master_schedule.csv, master_schedule_mar12.csv, and
#     master_schedule_mar30_may15.csv are kept as archival/superseded unless you
#     explicitly set include_in_audit = TRUE.

schedule_registry <- tribble(
  ~file_name, ~wave_id, ~wave_label, ~schedule_status, ~include_in_audit, ~notes,
  "master_schedule.csv", "archival_initial_600", "Archival initial anonymous schedule", "archival", FALSE,
  "Early 15 x 40 schedule without named confederates. Kept for provenance only unless it was distributed.",

  "master_schedule_feb13.csv", "archival_feb13_named", "Archival named V2 schedule - Feb13 copy", "archival_duplicate", FALSE,
  "Named 17 x 40 schedule. Appears identical to master_schedule_feb26.csv; kept as archival.",

  "master_schedule_feb26.csv", "wave1_original_v2", "Wave 1: original named V2 schedule", "active_canonical", TRUE,
  "Named original/V2 schedule used as the main early fieldwork assignment schedule.",

  "master_schedule_mar12.csv", "archival_mar12_interim", "Archival March 12 interim schedule", "archival_or_interim", FALSE,
  "Interim 3-confederate schedule. Set include_in_audit = TRUE if it was distributed and used.",

  "master_schedule_mar30_may15.csv", "archival_mar30_may15_interim", "Archival March 30-May 15 schedule", "archival_or_interim", FALSE,
  "Interim 1-confederate schedule. Set include_in_audit = TRUE if it was distributed and used.",

  "master_schedule_apr24_may25.csv", "wave2_late_april", "Wave 2: late-April/replacement schedule", "active_canonical", TRUE,
  "Late-April schedule for additional/replacement confederates after attrition and implementation losses.",

  "master_schedule_supplemental_may21_may31.csv", "archival_supplemental_may21", "Archival supplemental May21-May31 schedule", "superseded", FALSE,
  "Earlier supplemental version; superseded by May22-May31 because Laura could not perform Bank transactions.",

  "master_schedule_supplemental_may22_may31.csv", "wave3_supplemental_may22", "Wave 3: supplemental May22-May31 schedule", "active_canonical", TRUE,
  "Final supplemental schedule for returning confederates; Laura's feasible assignment set excludes Bank."
) |>
  mutate(
    resolved_path = map_chr(file_name, resolve_schedule_path),
    file_found = !is.na(resolved_path)
  )

missing_canonical <- schedule_registry |>
  filter(include_in_audit, !file_found)

if (nrow(missing_canonical) > 0) {
  stop(
    "Missing canonical schedule file(s):\n",
    paste(missing_canonical$file_name, collapse = "\n"),
    "\n\nPut these files in either:\n",
    schedule_dir,
    "\nor in the project root:\n",
    here()
  )
}

write_csv(
  schedule_registry,
  file.path(out_dir, "IADB_08b_schedule_file_inventory.csv")
)

# ------------------------------------------------------------------------------
# 3. Read and harmonize schedules ----------------------------------------------
# ------------------------------------------------------------------------------
read_schedule_file <- function(file_name, wave_id, wave_label, schedule_status,
                               include_in_audit, notes, resolved_path, file_found) {
  if (!isTRUE(file_found)) {
    return(tibble())
  }

  df <- read_csv(resolved_path, show_col_types = FALSE) |>
    clean_names()

  # Ensure common columns exist across old and new schedule versions.
  needed <- c(
    "confederate_id", "confederate_code", "confederate_name", "nationality",
    "country", "transaction_order", "transaction_id", "block_10", "phase",
    "assigned_week", "approximate_date", "send_by_date", "channel",
    "amount_usd", "delivery_method", "data_collection_wave",
    "supplemental_transaction", "original_schedule_transaction",
    "replacement_for_dropout", "notes"
  )

  missing_cols <- setdiff(needed, names(df))
  if (length(missing_cols) > 0) {
    for (cc in missing_cols) df[[cc]] <- NA
  }

  df |>
    mutate(
      source_file = file_name,
      wave_id = wave_id,
      wave_label = wave_label,
      schedule_status = schedule_status,
      include_in_audit = include_in_audit,
      registry_notes = notes,
      confederate_id = as.character(confederate_id),
      confederate_code = as.character(confederate_code),
      confederate_name = as.character(confederate_name),
      country = as.character(country),
      transaction_order = suppressWarnings(as.integer(transaction_order)),
      transaction_id = as.character(transaction_id),
      block_10 = suppressWarnings(as.integer(block_10)),
      phase = suppressWarnings(as.integer(phase)),
      assigned_week = suppressWarnings(as.integer(assigned_week)),
      approximate_date = as.Date(approximate_date),
      send_by_date = as.Date(send_by_date),
      channel_raw = as.character(channel),
      channel = case_when(
        str_to_lower(channel_raw) %in% c("bank", "banks") ~ "Banks",
        str_to_lower(channel_raw) %in% c("mts", "mto", "mtos") ~ "MTOs",
        str_to_lower(channel_raw) == "fintech" ~ "Fintech",
        str_to_lower(channel_raw) == "crypto" ~ "Crypto",
        TRUE ~ channel_raw
      ),
      channel = factor(channel, levels = c("Banks", "MTOs", "Fintech", "Crypto")),
      amount_usd = suppressWarnings(as.numeric(amount_usd)),
      delivery_method = case_when(
        str_to_lower(as.character(delivery_method)) %in% c("in-person", "in person", "inperson") ~ "In-person",
        str_to_lower(as.character(delivery_method)) == "online" ~ "Online",
        TRUE ~ as.character(delivery_method)
      ),
      delivery_method = factor(delivery_method, levels = c("In-person", "Online")),
      supplemental_transaction = as.integer(replace_na(as.numeric(supplemental_transaction), 0)),
      original_schedule_transaction = as.integer(replace_na(as.numeric(original_schedule_transaction), 1)),
      replacement_for_dropout = as.integer(replace_na(as.numeric(replacement_for_dropout), 0)),
      # Stable person key for diagnostics. For early unnamed schedules, use ID + country.
      confederate_key = case_when(
        !is.na(confederate_code) & confederate_code != "NA" & confederate_code != "" ~ confederate_code,
        !is.na(confederate_name) & confederate_name != "NA" & confederate_name != "" ~ confederate_name,
        TRUE ~ paste0("id_", confederate_id, "_", country)
      ),
      confederate_wave_key = paste(wave_id, confederate_key, sep = "__"),
      laura_supplemental_no_bank_constraint =
        wave_id == "wave3_supplemental_may22" &
        str_detect(str_to_lower(confederate_key), "laura")
    ) |>
    select(
      source_file, wave_id, wave_label, schedule_status, include_in_audit,
      registry_notes,
      confederate_id, confederate_code, confederate_name, confederate_key,
      confederate_wave_key, nationality, country, transaction_order,
      transaction_id, block_10, phase, assigned_week, approximate_date,
      send_by_date, channel_raw, channel, amount_usd, delivery_method,
      data_collection_wave, supplemental_transaction,
      original_schedule_transaction, replacement_for_dropout,
      laura_supplemental_no_bank_constraint, notes
    )
}

all_schedules <- pmap_dfr(schedule_registry, read_schedule_file)
canonical_schedule <- all_schedules |>
  filter(include_in_audit)

write_csv(
  all_schedules,
  file.path(out_dir, "IADB_08b_all_schedule_versions_harmonized.csv")
)

write_csv(
  canonical_schedule,
  file.path(out_dir, "IADB_08b_master_randomization_schedule_canonical.csv")
)

# ------------------------------------------------------------------------------
# 4. File-level and wave-level diagnostics -------------------------------------
# ------------------------------------------------------------------------------
schedule_file_summary <- all_schedules |>
  group_by(source_file, wave_id, wave_label, schedule_status, include_in_audit) |>
  summarise(
    n_rows = n(),
    n_confederates = n_distinct(confederate_key),
    min_date = min(approximate_date, na.rm = TRUE),
    max_date = max(approximate_date, na.rm = TRUE),
    n_banks = sum(channel == "Banks", na.rm = TRUE),
    n_mtos = sum(channel == "MTOs", na.rm = TRUE),
    n_fintech = sum(channel == "Fintech", na.rm = TRUE),
    n_crypto = sum(channel == "Crypto", na.rm = TRUE),
    n_usd100 = sum(amount_usd == 100, na.rm = TRUE),
    n_usd250 = sum(amount_usd == 250, na.rm = TRUE),
    n_in_person = sum(delivery_method == "In-person", na.rm = TRUE),
    n_online = sum(delivery_method == "Online", na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  schedule_file_summary,
  file.path(out_dir, "IADB_08b_schedule_file_summary.csv")
)

wave_summary <- canonical_schedule |>
  group_by(wave_id, wave_label) |>
  summarise(
    n_rows = n(),
    n_confederates = n_distinct(confederate_key),
    min_date = min(approximate_date, na.rm = TRUE),
    max_date = max(approximate_date, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  wave_summary,
  file.path(out_dir, "IADB_08b_canonical_wave_summary.csv")
)

# ------------------------------------------------------------------------------
# 5. Balance diagnostics --------------------------------------------------------
# ------------------------------------------------------------------------------
channel_balance_by_wave <- canonical_schedule |>
  count(wave_id, wave_label, channel, name = "n") |>
  group_by(wave_id, wave_label) |>
  mutate(share = n / sum(n)) |>
  ungroup()

amount_balance_by_wave <- canonical_schedule |>
  count(wave_id, wave_label, amount_usd, name = "n") |>
  group_by(wave_id, wave_label) |>
  mutate(share = n / sum(n)) |>
  ungroup()

delivery_balance_by_wave <- canonical_schedule |>
  count(wave_id, wave_label, delivery_method, name = "n") |>
  group_by(wave_id, wave_label) |>
  mutate(share = n / sum(n)) |>
  ungroup()

channel_amount_balance <- canonical_schedule |>
  count(wave_id, wave_label, channel, amount_usd, name = "n") |>
  group_by(wave_id, wave_label, channel) |>
  mutate(share_within_channel = n / sum(n)) |>
  ungroup()

channel_delivery_balance <- canonical_schedule |>
  count(wave_id, wave_label, channel, delivery_method, name = "n") |>
  group_by(wave_id, wave_label, channel) |>
  mutate(share_within_channel = n / sum(n)) |>
  ungroup()

confederate_channel_balance <- canonical_schedule |>
  count(wave_id, wave_label, confederate_key, laura_supplemental_no_bank_constraint, channel, name = "n") |>
  pivot_wider(names_from = channel, values_from = n, values_fill = 0) |>
  mutate(
    total_transactions = Banks + MTOs + Fintech + Crypto,
    expected_note = case_when(
      laura_supplemental_no_bank_constraint ~ "Laura supplemental wave: Bank excluded by feasible assignment constraint.",
      total_transactions == 40 ~ "Expected balanced 10 per channel.",
      total_transactions == 20 ~ "Expected balanced 5 per channel unless constrained.",
      TRUE ~ "Check manually: non-standard transaction count."
    ),
    balanced_four_channels = Banks == MTOs & MTOs == Fintech & Fintech == Crypto,
    no_bank_constraint_ok = laura_supplemental_no_bank_constraint & Banks == 0
  )

confederate_amount_balance <- canonical_schedule |>
  count(wave_id, wave_label, confederate_key, amount_usd, name = "n") |>
  pivot_wider(names_from = amount_usd, values_from = n, values_fill = 0, names_prefix = "usd_")

write_csv(channel_balance_by_wave, file.path(out_dir, "IADB_08b_channel_balance_by_wave.csv"))
write_csv(amount_balance_by_wave, file.path(out_dir, "IADB_08b_amount_balance_by_wave.csv"))
write_csv(delivery_balance_by_wave, file.path(out_dir, "IADB_08b_delivery_balance_by_wave.csv"))
write_csv(channel_amount_balance, file.path(out_dir, "IADB_08b_amount_within_channel_by_wave.csv"))
write_csv(channel_delivery_balance, file.path(out_dir, "IADB_08b_delivery_within_channel_by_wave.csv"))
write_csv(confederate_channel_balance, file.path(out_dir, "IADB_08b_channel_balance_by_confederate.csv"))
write_csv(confederate_amount_balance, file.path(out_dir, "IADB_08b_amount_balance_by_confederate.csv"))

# ------------------------------------------------------------------------------
# 6. Consecutive-channel diagnostics -------------------------------------------
# ------------------------------------------------------------------------------
consecutive_channel_check <- canonical_schedule |>
  arrange(wave_id, confederate_key, transaction_order) |>
  group_by(wave_id, wave_label, confederate_key) |>
  mutate(
    same_as_previous = channel == lag(channel),
    run_id = cumsum(is.na(lag(channel)) | channel != lag(channel)),
    run_length = ave(as.integer(channel), run_id, FUN = length),
    violates_more_than_two_consecutive = run_length > 2
  ) |>
  ungroup()

consecutive_violations <- consecutive_channel_check |>
  filter(violates_more_than_two_consecutive) |>
  select(
    wave_id, wave_label, confederate_key, transaction_order, block_10,
    assigned_week, approximate_date, channel, run_id, run_length
  )

write_csv(
  consecutive_channel_check,
  file.path(out_dir, "IADB_08b_consecutive_channel_check_all_rows.csv")
)

write_csv(
  consecutive_violations,
  file.path(out_dir, "IADB_08b_consecutive_channel_violations.csv")
)

# ------------------------------------------------------------------------------
# 7. Block/week diagnostics -----------------------------------------------------
# ------------------------------------------------------------------------------
block_balance <- canonical_schedule |>
  count(wave_id, wave_label, confederate_key, block_10, channel, name = "n") |>
  group_by(wave_id, wave_label, confederate_key, block_10) |>
  mutate(total_block_n = sum(n)) |>
  ungroup()

week_balance <- canonical_schedule |>
  count(wave_id, wave_label, assigned_week, channel, name = "n") |>
  group_by(wave_id, wave_label, assigned_week) |>
  mutate(total_week_n = sum(n)) |>
  ungroup()

daily_load <- canonical_schedule |>
  count(wave_id, wave_label, confederate_key, approximate_date, name = "n_transactions_that_date") |>
  arrange(wave_id, confederate_key, approximate_date)

write_csv(block_balance, file.path(out_dir, "IADB_08b_block_channel_balance.csv"))
write_csv(week_balance, file.path(out_dir, "IADB_08b_week_channel_balance.csv"))
write_csv(daily_load, file.path(out_dir, "IADB_08b_daily_transaction_load_by_confederate.csv"))

# ------------------------------------------------------------------------------
# 8. Laura supplemental documentation ------------------------------------------
# ------------------------------------------------------------------------------
laura_supplemental_check <- canonical_schedule |>
  filter(laura_supplemental_no_bank_constraint) |>
  count(confederate_key, channel, amount_usd, delivery_method, name = "n") |>
  arrange(confederate_key, channel, amount_usd, delivery_method)

laura_summary <- canonical_schedule |>
  filter(str_detect(str_to_lower(confederate_key), "laura")) |>
  group_by(wave_id, wave_label, confederate_key) |>
  summarise(
    n_transactions = n(),
    n_banks = sum(channel == "Banks", na.rm = TRUE),
    n_mtos = sum(channel == "MTOs", na.rm = TRUE),
    n_fintech = sum(channel == "Fintech", na.rm = TRUE),
    n_crypto = sum(channel == "Crypto", na.rm = TRUE),
    no_bank_constraint_flagged = any(laura_supplemental_no_bank_constraint),
    .groups = "drop"
  )

write_csv(laura_supplemental_check, file.path(out_dir, "IADB_08b_laura_supplemental_assignment_detail.csv"))
write_csv(laura_summary, file.path(out_dir, "IADB_08b_laura_schedule_summary.csv"))

# ------------------------------------------------------------------------------
# 9. Documentation table --------------------------------------------------------
# ------------------------------------------------------------------------------
deviation_documentation <- tribble(
  ~issue, ~documentation, ~recommended_reporting_language,
  "Multiple randomization waves",
  "The final schedule consists of a named original/V2 wave, a late-April replacement/additional-confederate wave, and a May 22-May 31 supplemental wave.",
  "We document analyses by randomization wave and preserve assignment variables from the schedule used at the time of fieldwork.",

  "Superseded schedules",
  "Several archival or superseded schedule files exist. The audit registry marks these as archival unless include_in_audit is manually set to TRUE.",
  "The appendix distinguishes final/canonical schedules from earlier schedule versions kept for provenance.",

  "Supplemental constrained assignment for Laura",
  "The final supplemental May22-May31 schedule excludes Bank assignments for Laura because Bank transactions were infeasible for her at that point in fieldwork.",
  "Laura's supplemental transactions are treated as ex-ante constrained randomization over feasible channels, not as post-assignment Bank non-compliance.",

  "ITT-style assignment variables",
  "The primary analysis should continue to use assigned channel, amount, and delivery mode from the canonical schedule.",
  "Per-protocol or actual-channel analyses should be reported only as sensitivity/exploratory checks."
)

write_csv(deviation_documentation, file.path(out_dir, "IADB_08b_schedule_deviation_documentation.csv"))

# ------------------------------------------------------------------------------
# 10. Console summary ----------------------------------------------------------
# ------------------------------------------------------------------------------
cat("\n=== IADB RANDOMIZATION SCHEDULE AUDIT COMPLETE ===\n")
cat("Outputs saved to:\n")
cat("  ", out_dir, "\n", sep = "")
cat("\nCanonical schedule rows included in audit: ", nrow(canonical_schedule), "\n", sep = "")
cat("Canonical confederate-wave units: ", n_distinct(canonical_schedule$confederate_wave_key), "\n", sep = "")
cat("Consecutive-channel violations (>2 same channel in a row): ", nrow(consecutive_violations), "\n", sep = "")
cat("\nFiles included in canonical audit:\n")
print(schedule_registry |> filter(include_in_audit) |> select(file_name, wave_id, file_found))
cat("\nFiles marked archival/superseded:\n")
print(schedule_registry |> filter(!include_in_audit) |> select(file_name, wave_id, schedule_status, file_found))
cat("\nReview IADB_08b_schedule_file_inventory.csv before reporting to confirm which schedule versions were actually used.\n")
