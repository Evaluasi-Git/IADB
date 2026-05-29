# Quantitative Analysis
This folder contains the core R pipeline for the IADB KYC/AML audit study. The scripts clean raw SurveyCTO data, merge SurveyCTO submissions to randomized transaction schedules and payment-tracking records, construct SAP analysis samples, build FX-adjusted cost outcomes, estimate the pre-specified SAP models, and export final tables and implementation/attrition diagnostics.

The pipeline should be run from the project root so that `here::here()` resolves paths correctly.

Reference sheet of required inputs for all scripts [here](https://docs.google.com/document/d/12rZm5D1ehirqKgC0H-VjnGBVI0SOFCW43aPwVMPy7S4/edit?usp=sharing).

## Script sequence
Run the scripts in the order below.
```r
source("01_data_cleaning_v2.R")
source("02_enriched_payment_schedule.R")
source("03_build_sap_dataset.R")
source("04_build_analysis_sample_and_attrition.R")

# Optional QA/recovery scripts. Run only when reviewing duplicate-slot recovery.
source("04b_duplicate_slot_recovery_audit.R")
source("04c_prepare_recovery_review.R")

source("05_run_sap_models.R")
source("06_exchange_rates_fred.R")
source("07_cost_time_sap_models.R")
source("08_generate_final_results.R")
source("09_attrition.R")
```
## Main folders
Expected project structure:
```text
Quantitative Analysis/
├── 01_data_cleaning_v2.R
├── 02_enriched_payment_schedule.R
├── 03_build_sap_dataset.R
├── 04_build_analysis_sample_and_attrition.R
├── 04b_duplicate_slot_recovery_audit.R
├── 04c_prepare_recovery_review.R
├── 05_run_sap_models.R
├── 06_exchange_rates_fred.R
├── 07_cost_time_sap_models.R
├── 08_generate_final_results.R
├── 09_attrition.R
├── data/
│   ├── raw/
│   ├── clean/
│   └── manual/
└── IADB_Survey_WIDE_may16.csv
```
The exact raw-data location can be changed at the top of each script. Current scripts use `here::here()` whenever possible so that paths are portable across machines.
Required R packages

The scripts use:
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
  "zoo"
))
```
`fredr` is only needed for the FX script. The FRED API key is optional but recommended:
```r
Sys.setenv(FRED_API_KEY = "your_key_here")
```
# Pipeline overview
`01_data_cleaning_v2.R`
Cleans the raw SurveyCTO wide file and constructs transaction-level variables.
**Main input:**
```text
IADB_Survey_WIDE_may16.csv
```
**Main outputs:**
```text
data/clean/IADB_surveycto_clean_may16.csv
data/clean/IADB_surveycto_clean_may16.rds
data/clean/IADB_surveycto_clean_diagnostics_may16.rds
data/clean/IADB_surveycto_balance_by_channel_may16.csv
```
**Key outputs include:**
- `success`: binary transaction completion outcome.
- `kyc_score`: primary 0–3 KYC outcome, using the SurveyCTO hidden score when available and constructed fallback otherwise.
- `kyc_score_composite_0_5`: richer 0–5 KYC/procedure composite, constructed only when at least five component scores are observed.
- `time_hours`: PAP-aligned transaction settlement time, defined for successful transactions only.
- `transaction_duration_hours`: SurveyCTO exact-minute duration (`i1_exact_minutes / 60`), not the PAP settlement-time outcome.
- `interaction_time_hours`: active interaction burden, equal to `(travel + waiting + service time) / 60`.
- `cost_local` and `total_cost_without_time_local`: local-currency cost outcomes, not yet converted to USD.

`02_enriched_payment_schedule.R`
Builds an enriched schedule by stacking randomized schedules and matching them to the internal payment-tracking schedule.

**Main inputs:**
```text
data/raw/[IADB] - Internal Payment Tracking - Payment Schedule.csv
IADB/data/randomization/master_schedule_*.csv
```

**Main output:**
```text
data/clean/sap_dataset_builder/IADB_payment_schedule_enriched_with_randomization.csv
```
This file recovers assigned treatment conditions for each transaction slot: channel, amount, delivery mode, assigned date, transaction order, and payment-tracking status.

`03_build_sap_dataset.R`
Builds the first schedule-level SAP dataset by merging cleaned SurveyCTO submissions with the enriched randomized/payment schedule.

**Main inputs:**
```text
data/clean/IADB_surveycto_clean_may16.csv
data/clean/sap_dataset_builder/IADB_payment_schedule_enriched_with_randomization.csv
```

**Main outputs:**
```text
data/clean/sap_dataset_builder/IADB_sap_schedule_level_base.csv
data/clean/sap_dataset_builder/IADB_sap_schedule_level_base.rds
data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.csv
data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
data/clean/sap_dataset_builder/IADB_03_surveycto_schedule_matched_full_audit.csv
data/clean/sap_dataset_builder/IADB_manual_schedule_match_review_to_complete.csv
data/clean/sap_dataset_builder/IADB_bad_survey_completion_review.csv
```
This script creates the schedule-level denominator and a conservative first-pass observed sample. It also exports manual-review files for ambiguous matches, duplicate IDs, and bad completions.

`04_build_analysis_sample_and_attrition.R`
Builds the maximal automated observed sample and attrition diagnostics.

**Main inputs:**
```text
data/clean/sap_dataset_builder/IADB_03_surveycto_schedule_matched_full_audit.csv
data/clean/sap_dataset_builder/IADB_sap_schedule_level_base.rds
data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
```

**Main outputs:**
```text
data/clean/sap_dataset_builder/IADB_sap_schedule_level_base_maximal_auto.csv
data/clean/sap_dataset_builder/IADB_sap_schedule_level_base_maximal_auto.rds
data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto.csv
data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto.rds
data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto.csv
data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto.rds
```
This script defines the core observed samples used in the SAP models:
- `main`: maximal automated SAP analysis sample.
- `clean`: main sample excluding observations flagged for final manual review.
- `per_protocol`: matched observations consistent with assigned protocol.
- `conservative`: conservative first-pass matched sample from Script 03.

`04b_duplicate_slot_recovery_audit.R` and `04c_prepare_recovery_review.R`
Optional QA scripts for duplicate-slot recovery. These scripts are not required for every run. Use them when the matching audit suggests that some SurveyCTO rows may have been assigned to duplicate slots or could be recovered into unused schedule slots.
**Outputs include:**
```text
data/clean/sap_dataset_builder/IADB_04b_top_recovery_candidates.csv
data/clean/sap_dataset_builder/IADB_04c_recovery_review_template.csv
```
These files support manual review. They should not be used to overwrite the SAP sample without a documented decision trail.

`05_run_sap_models.R`
Runs first-pass SAP models for transaction success and KYC burden.

**Main inputs:**
```text
data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto.rds
data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto.rds
data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
```

**Main outputs:**
```text
data/clean/sap_dataset_builder/sap_results_maximal_auto/sap_success_kyc_main_and_sensitivity.txt
data/clean/sap_dataset_builder/sap_results_maximal_auto/sap_success_kyc_main_and_sensitivity.tex
data/clean/sap_dataset_builder/sap_results_maximal_auto/sap_success_kyc_main_and_sensitivity.rds
```
The model specification is:
```text
Y_ij = beta_1 MTO_ij + beta_2 Fintech_ij + beta_3 Crypto_ij
       + beta_4 Amount250_ij + beta_5 Online_ij
       + confederate fixed effects + error_ij
```
Standard errors are clustered by confederate.
The omitted category is:
```text
Banks + lower transaction amount + in-person delivery
```
``06_exchange_rates_fred.R``
Builds the daily FX-rate table used to convert local-currency cost outcomes to USD.

**Main outputs:**
```text
data/manual/IADB_fx_rates_daily.csv
data/manual/IADB_fx_rates_daily_diagnostics.csv
```
The key FX variable is:
```text
fx_rate_local_per_usd
```
This means units of local currency per 1 USD. For example, `BRL = 5.20` means 1 USD = 5.20 Brazilian reais.

`07_cost_time_sap_models.R`
Builds USD cost outcomes, constructs time/cost samples, and estimates cost/time SAP models.

**Main inputs:**
```text
data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto.rds
data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto.rds
data/clean/sap_dataset_builder/IADB_sap_observed_first_pass.rds
data/manual/IADB_fx_rates_daily.csv
```
**Main outputs:**
```text
data/clean/sap_dataset_builder/IADB_sap_observed_maximal_auto_cost_time.rds
data/clean/sap_dataset_builder/IADB_sap_observed_clean_cost_time.rds
data/clean/sap_dataset_builder/IADB_sap_per_protocol_maximal_auto_cost_time.rds
data/clean/sap_dataset_builder/IADB_sap_observed_conservative_cost_time.rds
data/clean/sap_dataset_builder/sap_results_cost_fx_time/
```

**Main monetary outcome:**
```text
total_cost_without_time_usd
```

**Main time outcomes:**
- `settlement_time_hours`: PAP-aligned settlement time, built from `time_hours` in the cleaned SurveyCTO data.
- `interaction_time_hours`: active burden measure, equal to travel + waiting + service time, in hours.
- `transaction_duration_hours`: SurveyCTO exact-minute duration, based on `i1_exact_minutes / 60`. This is useful as a secondary reported-duration measure but should not be described as the PAP settlement-time outcome.

`08_generate_final_results.R`
Loads final analysis datasets and exports final `fixest::etable` outputs for success, KYC, cost, settlement time, interaction time, and reported exact duration. Those tables are for internal purposes only.

**Main outputs:**
```text
data/clean/sap_dataset_builder/final_etables/
```

**Outcome table groups:**
```text
IADB_08_success_models.*
IADB_08_kyc_models.*
IADB_08_main_outcomes_models.*
IADB_08_cost_any_attempt_models.*
IADB_08_cost_success_only_models.*
IADB_08_settlement_time_models.*
IADB_08_interaction_time_models.*
IADB_08_reported_exact_duration_models.*
IADB_08_all_models.*
```

`09_attrition.R`
Runs implementation and attrition checks using the schedule-level denominator rather than only observed SurveyCTO submissions.

**Main outputs:**
```text
data/clean/sap_dataset_builder/implementation_checks/
```
This script checks whether assigned channel, amount, or delivery mode predicts:
- being attempted;
- being funded but not attempted;
- being skipped/not attempted;
- unconditional success counting funded-not-attempted cases as failures;
- protocol deviation among attempted transactions.
- Outcome definitions

# Main Outcomes
## Transaction success
`success = 1` if the transaction was completed. Rejected, incomplete, and abandoned transactions are coded as `success = 0`.

## KYC burden
The primary KYC outcome is `kyc_score`, a 0–3 measure:
`0`: no KYC/documentation observed;
`1`: basic government ID requested;
`2`: enhanced identity/address documentation requested;
`3`: high-stringency verification, including biometrics, source-of-funds documentation, or source-of-funds questioning.
The richer 0–5 composite is `kyc_score_composite_0_5`. It averages observed procedural components only when at least five components are non-missing.

## Transaction cost
Cost variables are kept in local currency in Script 01. USD conversion happens only after FX-rate construction in Scripts 06 and 07.
The preferred cost outcome is:
```text
total_cost_without_time_usd
```
This excludes time costs. Time costs are calculated separately and should not be mechanically added to monetary costs unless explicitly described as a combined burden measure.

# Time
There are three distinct time outcomes:
- `settlement_time_hours` / `time_hours`: PAP-aligned settlement time. This is the time from transaction initiation to receipt/settlement confirmation and is defined only for successful transactions.
- `interaction_time_hours`: active interaction burden. This sums travel, waiting, and service time and converts minutes to hours.
- `transaction_duration_hours`: SurveyCTO exact-minute reported duration. This comes from `i1_exact_minutes / 60` and captures the respondent's exact-minute answer to the “entire transaction” duration question, if known.
Do not describe `transaction_duration_hours` as the PAP settlement-time outcome. The PAP-aligned time outcome is `time_hours` in the cleaning script and `settlement_time_hours` in the cost/time model scripts.

# Samples
The final model tables use four sample definitions:
- `main`: maximal automated matched SAP analysis sample.
- `clean`: main sample excluding observations flagged as requiring final manual review.
- `per_protocol`: matched observations consistent with assigned protocol and excluding protocol deviations.
- `conservative`: conservative first-pass matched sample before broader automated recovery/inclusion rules.
Sample sizes differ across outcome families because cost and time outcomes are only observed for subsets of transactions.

# Model specifications
All main SAP models use the same additive treatment specification:
```text
Y_ij = beta_1 MTO_ij + beta_2 Fintech_ij + beta_3 Crypto_ij
       + beta_4 Amount250_ij + beta_5 Online_ij
       + alpha_j + epsilon_ij
```
where `alpha_j` denotes confederate fixed effects.
The omitted category is:
```text
Banks + lower transaction amount + in-person delivery
```
The `Online` coefficient is an adjusted online-versus-in-person contrast, controlling for assigned channel, assigned amount, and confederate fixed effects. Because the model does not include channel-by-delivery interactions, it imposes a common online effect across channels and amounts.

TBD: Cedric is now working on heterogenous effects.

# Replication notes
1. Run scripts from the project root.
2. Do not change filenames in `data/clean/sap_dataset_builder/` unless downstream scripts are updated accordingly.
3. Keep raw SurveyCTO exports, payment-tracking files, and manual-review templates out of public repositories if they contain sensitive information.
4. Check diagnostics after each script before proceeding to the next script.
4. If manual-review decisions are made, archive the decision file and document the criteria used.
