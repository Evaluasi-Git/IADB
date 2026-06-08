# Quantitative Analysis

This folder contains the core R pipeline for the IADB KYC/AML audit study. The scripts clean raw SurveyCTO data, merge SurveyCTO submissions to randomized transaction schedules and payment-tracking records, construct SAP analysis samples, build FX-adjusted cost outcomes, estimate PAP/SAP-aligned models, apply multiple-testing corrections, and export final client-facing figures.

The scripts should be run sequentially. Each script contains a short replication note at the top indicating the required inputs and the local paths that need to be changed before replication.

Step-by-step for replication and input files [here]([https://www.example.com](https://docs.google.com/document/d/1FQVRKk3eEwpFQXLxNnyRj6ZZsv77kPWTAf45P81lSb8/edit?usp=sharing))

## Replication workflow

The repository contains the R scripts. Sensitive input files should be stored separately and should not be committed to GitHub if they contain confidential or identifiable information.

To replicate the analysis:

1. Download or clone this repository.
2. Download the required input files from the shared Drive folder.
3. Open each script and update the path block at the top of the script so that it points to the local location of the required inputs and the desired output folder.
4. Run the scripts in numerical order.
5. After each script, verify that the main output files were created before moving to the next script.

The replication documentation highlights the main output files in yellow. These are the most important checkpoints because they either contain the script’s primary analytical product or serve as required inputs for later scripts.

## Script sequence

Run the scripts in the order below.

```r
source("01_data_cleaning.R")
source("02_enriched_payment_schedule.R")
source("03_build_sap_dataset.R")
source("04_build_analysis_sample_and_attrition.R")
source("04b_duplicate_slot_recovery_audit.R")
source("04c_prepare_recovery_review.R")
source("05_run_sap_models.R")
source("06_exchange_rates_fred.R")
source("07_cost_time_sap_models.R")
source("08_generate_final_results.R")
source("09_evaluasish_barplots.R")
```

## Required R packages

The scripts use the following packages:

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "janitor",
  "readr",
  "stringr",
  "stringi",
  "here",
  "fixest",
  "fredr",
  "httr2",
  "zoo",
  "estimatr",
  "broom",
  "scales",
  "dqrng"
))
```

The `modelsummary` package is optional and is only used for convenience LaTeX output in Script 08:

```r
install.packages("modelsummary")
```

The FRED API key is optional but recommended for Script 06:

```r
Sys.setenv(FRED_API_KEY = "your_key_here")
```

Do not hard-code a personal FRED API key in the scripts.

## Suggested folder structure

The scripts can be replicated from any local folder as long as the paths at the top of each script are updated. A convenient structure is:

```text
IADB_replication/
├── scripts/
│   ├── 01_data_cleaning.R
│   ├── 02_enriched_payment_schedule.R
│   ├── 03_build_sap_dataset.R
│   ├── 04_build_analysis_sample_and_attrition.R
│   ├── 04b_duplicate_slot_recovery_audit.R
│   ├── 04c_prepare_recovery_review.R
│   ├── 05_run_sap_models.R
│   ├── 06_exchange_rates_fred.R
│   ├── 07_cost_time_sap_models.R
│   ├── 08_generate_final_results.R
│   └── 09_evaluasish_barplots.R
├── inputs/
│   ├── raw/
│   ├── manual/
│   └── randomization/
└── outputs/
    ├── data/
    │   ├── clean/
    │   └── manual/
    └── figures/
```

Replicators do not need to use this exact folder structure, but they must update the path block at the top of each script before running it.

# Pipeline overview

## `01_data_cleaning.R`

Cleans the raw SurveyCTO wide-format export and creates the first cleaned transaction-level dataset.

**Main input:**

```text
IADB_Survey_WIDE_june1.csv
```

**Main outputs:**

```text
IADB_surveycto_clean_june1.csv
IADB_surveycto_clean_june1.rds
IADB_surveycto_clean_diagnostics_june1.rds
IADB_surveycto_balance_by_channel_june1.csv
IADB_surveycto_quality_flags_june1.csv
IADB_surveycto_kyc_validation_june1.csv
```

**Key downstream output:**

```text
IADB_surveycto_clean_june1.csv
```

This file is used by Script 03.

## `02_enriched_payment_schedule.R`

Builds the enriched randomized/payment schedule by stacking randomized schedules and matching them to the internal payment-tracking file.

**Main inputs:**

```text
[IADB] - Internal Payment Tracking - Payment Schedule_june1.csv
master_schedule_feb13.csv
master_schedule_feb26.csv
master_schedule_mar12.csv
master_schedule_mar30_may15.csv
master_schedule_apr24_may25.csv
master_schedule_supplemental_may21_may31.csv
master_schedule_supplemental_may22_may31.csv
```

**Main outputs:**

```text
IADB_payment_schedule_enriched_with_randomization.csv
IADB_payment_schedule_enriched_with_randomization.rds
```

**Key downstream output:**

```text
IADB_payment_schedule_enriched_with_randomization.csv
```

This file is used by Script 03.

## `03_build_sap_dataset.R`

Builds the SAP-ready datasets by matching cleaned SurveyCTO submissions to the enriched randomized/payment schedule. It creates the schedule-level denominator, observed valid-attempt sample, strict assignment-match sensitivity sample, and reviewed-submissions sample.

**Main inputs:**

```text
IADB_surveycto_clean_june1.csv
IADB_payment_schedule_enriched_with_randomization.csv
IADB_manual_schedule_match_review_completed.csv
IADB_confederate_crosswalk.csv
```

The manual review file is required when `SKIP_MANUAL_REVIEW <- FALSE`. The confederate crosswalk is optional and is used when SurveyCTO confederate IDs and schedule names need harmonization.

**Main outputs:**

```text
IADB_sap_schedule_level_base.csv
IADB_sap_schedule_level_base.rds
IADB_sap_observed_first_pass.csv
IADB_sap_observed_first_pass.rds
IADB_sap_attempted_after_funding.csv
IADB_sap_attempted_after_funding.rds
IADB_sap_per_protocol.csv
IADB_sap_per_protocol.rds
IADB_sap_reviewed_submissions.csv
IADB_sap_reviewed_submissions.rds
IADB_03_surveycto_schedule_matched_full_audit.csv
IADB_03_slot_level_duplicate_resolution.csv
IADB_03_sap_merge_checks.csv
```

**Key downstream outputs:**

```text
IADB_sap_observed_first_pass.rds
IADB_sap_attempted_after_funding.rds
IADB_sap_per_protocol.rds
IADB_sap_reviewed_submissions.rds
IADB_03_surveycto_schedule_matched_full_audit.csv
IADB_03_slot_level_duplicate_resolution.csv
IADB_03_sap_merge_checks.csv
```

These files are used by Scripts 04, 04b, 04c, 05, 06, 07, and 08.

## `04_build_analysis_sample_and_attrition.R`

Builds sample-flow and attrition diagnostics from the SAP datasets created in Script 03. It also creates backward-compatible “maximal auto” aliases used by some downstream checks.

**Main inputs:**

```text
IADB_03_surveycto_schedule_matched_full_audit.csv
IADB_sap_schedule_level_base.rds
IADB_sap_observed_first_pass.rds
IADB_sap_attempted_after_funding.rds
IADB_sap_per_protocol.rds
IADB_sap_reviewed_submissions.rds
IADB_03_slot_level_duplicate_resolution.csv
```

**Main outputs:**

```text
IADB_04_sample_comparison.csv
IADB_04_sample_quality_summary.csv
IADB_04_slot_duplicate_resolution_summary.csv
IADB_sap_schedule_level_base_maximal_auto.csv
IADB_sap_schedule_level_base_maximal_auto.rds
IADB_sap_observed_maximal_auto.csv
IADB_sap_observed_maximal_auto.rds
IADB_sap_per_protocol_maximal_auto.csv
IADB_sap_per_protocol_maximal_auto.rds
```

**Key downstream outputs:**

```text
IADB_sap_observed_maximal_auto.rds
IADB_sap_per_protocol_maximal_auto.rds
```

These files are optional inputs for Script 06 when inferring the FX date range.

## `04b_duplicate_slot_recovery_audit.R`

Audits duplicate-slot resolution after Script 03. It verifies that the strict slot-level observed sample has one row per schedule slot and documents which duplicate-slot rows were excluded from the strict slot-level sample.

**Main inputs:**

```text
IADB_03_surveycto_schedule_matched_full_audit.csv
IADB_sap_observed_first_pass.rds
IADB_sap_reviewed_submissions.rds
IADB_03_slot_level_duplicate_resolution.csv
IADB_03_sap_merge_checks.csv
IADB_03_duplicate_slot_reassignment_log.csv
IADB_03_final_duplicate_schedule_slots.csv
```

The last two files are optional if produced by Script 03.

**Main outputs:**

```text
IADB_04b_duplicate_resolution_audit_summary.csv
IADB_04b_strict_slot_level_checks.csv
IADB_04b_reviewed_submission_checks.csv
IADB_04b_sample_preservation_check.csv
IADB_04b_slot_resolution_summary.csv
IADB_04b_duplicate_slot_group_summary.csv
IADB_04b_slot_level_excluded_duplicate_rows.csv
IADB_04b_slot_level_kept_duplicate_rows.csv
IADB_04b_duplicate_reassignment_summary.csv
```

**Key downstream output:**

```text
IADB_04b_duplicate_resolution_audit_summary.csv
```

This file is used by Script 04c.

## `04c_prepare_recovery_review.R`

Confirms that no separate manual recovery review is required under the revised pipeline and writes compatibility files expected by earlier versions of the workflow.

**Main inputs:**

```text
IADB_03_slot_level_duplicate_resolution.csv
IADB_sap_observed_first_pass.rds
IADB_sap_reviewed_submissions.rds
IADB_04b_duplicate_resolution_audit_summary.csv
```

**Main outputs:**

```text
IADB_04c_recovery_review_summary.csv
IADB_04c_all_strong_recovery_candidates.csv
IADB_04c_candidate_slot_pressure.csv
IADB_04c_strong_recovery_candidates_for_review.csv
IADB_04c_recovery_decisions_template.csv
```

**Key downstream output:**

```text
None
```

This script mainly provides documentation and compatibility outputs.

## `05_run_sap_models.R`

Runs first-pass SAP models for transaction success and KYC burden across the main and sensitivity samples. These models are useful for internal diagnostics, but Script 08 produces the final PAP/SAP-aligned results used for reporting.

**Main inputs:**

```text
IADB_sap_observed_first_pass.rds
IADB_sap_attempted_after_funding.rds
IADB_sap_per_protocol.rds
IADB_sap_reviewed_submissions.rds
```

**Main outputs:**

```text
sap_sample_summary.csv
sap_main_by_channel.csv
sap_models_skipped.csv
sap_success_kyc_main_and_sensitivity.txt
sap_success_kyc_main_and_sensitivity.tex
sap_success_kyc_main_and_sensitivity.rds
sap_success_models.txt
sap_success_models.tex
sap_kyc_models.txt
sap_kyc_models.tex
```

**Key downstream output:**

```text
None
```

Script 08 re-estimates the final PAP/SAP models directly rather than relying on Script 05 outputs.

## `06_exchange_rates_fred.R`

Builds the daily FX-rate table used to convert local-currency costs into USD. The script can use FRED when a FRED API key is available and falls back on a public currency API where needed.

**Main inputs:**

```text
IADB_sap_observed_maximal_auto.rds
IADB_sap_per_protocol_maximal_auto.rds
IADB_sap_observed_first_pass.rds
```

These SAP files are used to infer the required FX date range if available.

**Main outputs:**

```text
IADB_fx_rates_daily.csv
IADB_fx_rates_daily_diagnostics.csv
IADB_fx_rates_daily_source_counts.csv
```

**Key downstream output:**

```text
IADB_fx_rates_daily.csv
```

This file is used by Script 07.

## `07_cost_time_sap_models.R`

Builds cost/time analysis datasets, merges daily FX rates, converts local-currency costs to USD, constructs cost and time outcomes, and runs cost/time SAP models.

**Main inputs:**

```text
IADB_sap_observed_first_pass.rds
IADB_sap_per_protocol.rds
IADB_sap_reviewed_submissions.rds
IADB_fx_rates_daily.csv
IADB_hourly_wage_lookup.csv
```

The wage lookup file is optional. If it is not available, the script uses the built-in wage lookup.

**Main outputs:**

```text
IADB_sap_observed_first_pass_cost_time.csv
IADB_sap_observed_first_pass_cost_time.rds
IADB_sap_per_protocol_cost_time.csv
IADB_sap_per_protocol_cost_time.rds
IADB_sap_reviewed_submissions_cost_time.csv
IADB_sap_reviewed_submissions_cost_time.rds
IADB_06_cost_time_models.rds
IADB_06_cost_time_sample_summary.csv
IADB_06_model_sample_summary.csv
IADB_06_cost_time_models_skipped.csv
```

**Key downstream outputs:**

```text
IADB_sap_observed_first_pass_cost_time.rds
IADB_sap_per_protocol_cost_time.rds
IADB_sap_reviewed_submissions_cost_time.rds
```

These files are used by Script 08.

## `08_generate_final_results.R`

Generates the final PAP/SAP-aligned model results. The script loads the success/KYC and cost/time analysis datasets, estimates primary and sensitivity models, applies CR2 cluster-robust inference, and produces multiple-testing correction tables.

**Main inputs:**

```text
IADB_sap_observed_first_pass.rds
IADB_sap_per_protocol.rds
IADB_sap_reviewed_submissions.rds
IADB_sap_observed_first_pass_cost_time.rds
IADB_sap_per_protocol_cost_time.rds
IADB_sap_reviewed_submissions_cost_time.rds
```

**Main outputs:**

```text
IADB_08_pap_primary_models_cr2.csv
IADB_08_pap_primary_models_cr2.rds
IADB_08_pap_all_models_cr2.csv
IADB_08_pap_all_models_cr2.rds
IADB_08_multiplicity_channel_family.csv
IADB_08_multiplicity_transaction_family.csv
IADB_08_model_sample_summary.csv
IADB_08_skipped_models.csv
IADB_08_primary_outcome_definition_log.csv
```

**Key downstream outputs:**

```text
IADB_08_pap_primary_models_cr2.csv
IADB_08_pap_all_models_cr2.csv
IADB_08_multiplicity_channel_family.csv
IADB_08_multiplicity_transaction_family.csv
IADB_08_primary_outcome_definition_log.csv
```

These files are used by Script 09.

## `09_evaluasish_barplots.R`

Creates the final client-facing bar plots from the Script 08 PAP/SAP results. The figures apply the robust reporting rule used in the final report: primary channel effects are colored significant only if the Romano-Wolf adjusted p-value is below 0.05 and the plotted 95 percent confidence interval excludes zero; transaction-characteristic effects are colored significant only if the Holm-adjusted p-value is below 0.05 and the plotted 95 percent confidence interval excludes zero.

**Main inputs:**

```text
IADB_08_pap_primary_models_cr2.csv
IADB_08_pap_all_models_cr2.csv
IADB_08_multiplicity_channel_family.csv
IADB_08_multiplicity_transaction_family.csv
IADB_08_primary_outcome_definition_log.csv
```

**Main outputs:**

```text
IADB_08_plot_primary_channel_effects_pap_adjusted_robust.png
IADB_08_plot_primary_channel_effects_pap_adjusted_robust.pdf
IADB_08_plot_primary_transaction_effects_pap_adjusted_robust.png
IADB_08_plot_primary_transaction_effects_pap_adjusted_robust.pdf
IADB_08_plot_channel_effects_by_sample_p05_sensitivity_robust.png
IADB_08_plot_channel_effects_by_sample_p05_sensitivity_robust.pdf
IADB_08_plot_channel_effects_by_model_p05_sensitivity_robust.png
IADB_08_plot_channel_effects_by_model_p05_sensitivity_robust.pdf
```

**Key downstream output:**

```text
None
```

These figures are the final client-facing visual outputs of the quantitative pipeline.

# Main outcomes

## Transaction success

`success = 1` if the transaction was completed. Rejected, incomplete, and abandoned transactions are coded as `success = 0`.

## KYC/AML burden

The primary KYC/AML outcome is `kyc_score`, a 0 to 3 measure:

```text
0 = no KYC/documentation observed
1 = basic identity documentation requested
2 = enhanced identity or address documentation requested
3 = high-stringency verification or enhanced due diligence
```

The richer 0 to 5 composite is also constructed in Script 01 but is treated as a sensitivity outcome rather than the primary KYC/AML outcome.

## Transaction cost

Cost variables are preserved in local currency in Script 01. USD conversion occurs only after FX-rate construction in Scripts 06 and 07.

The preferred cost outcome in the final model pipeline is the USD transaction cost among successful transactions.

## Time and duration

The pipeline distinguishes several time outcomes:

- `time_hours`: cleaned reported time from SurveyCTO.
- `transaction_duration_hours`: transaction execution duration based on exact-minute reports, when available.
- `interaction_time_hours`: active interaction burden, equal to travel time plus waiting time plus service time.
- `time_duration`: preferred duration outcome used in the Script 08 primary results.

# Samples

The final model pipeline uses three main analysis samples:

```text
main_strict_slot_level
per_protocol_strict_slot
reviewed_submissions
```

The primary success and KYC analyses use the valid-attempt sample. Cost and duration analyses use the cost/time datasets constructed in Script 07.

# Model specifications

The final PAP/SAP model pipeline estimates channel, amount, and delivery-mode contrasts. The core adjusted specification is:

```text
Y_ic = beta_1 MTO_ic + beta_2 Fintech_ic + beta_3 Crypto_ic
       + gamma_1 Amount250_ic + gamma_2 Online_ic
       + confederate fixed effects + error_ic
```

where `i` indexes transactions and `c` indexes confederates.

The omitted channel category is:

```text
Banks
```

The omitted transaction amount is:

```text
USD 100
```

The omitted delivery mode is:

```text
In-person
```

Script 08 also exports other PAP/SAP model variants, including unadjusted, adjusted, country fixed-effects, and preferred confederate fixed-effects specifications.

# Multiple testing

Script 08 creates two multiple-testing families:

```text
IADB_08_multiplicity_channel_family.csv
IADB_08_multiplicity_transaction_family.csv
```

The primary channel family uses Romano-Wolf adjusted p-values. The transaction-characteristic family uses Holm adjustment.

Script 09 uses these adjusted p-values when coloring the final client-facing figures.

# Replication notes

1. Run the scripts in numerical order.
2. Update the path block at the top of each script before running it on a new computer.
3. Do not rename intermediate output files unless all downstream scripts are updated accordingly.
4. Keep raw SurveyCTO exports, payment-tracking files, manual-review files, and any identifying information out of public repositories.
5. After each script, check that the main output files were created before proceeding.
6. Use the yellow-highlighted outputs in the replication documentation as the main checkpoints for successful replication.
