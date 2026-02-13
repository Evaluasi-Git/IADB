# IADB KYC/AML Audit Study (Multi-Country)

This repository contains code and documentation for a multi-country **mystery-shopping audit study** assessing **KYC/AML compliance practices** across four remittance channels in Latin America and the Caribbean:
- **Bank** wire transfers  
- **Money Transfer Services (MTS)** (e.g., Western Union / MoneyGram)  
- **Fintech** platforms (e.g., Wise / Revolut)  
- **Cryptocurrency** exchanges (e.g., Coinbase / Binance)

## Study at a glance 
- **Sample:** 15 trained confederates in 10 countries  
- **Volume:** 40 cross-border transactions per confederate (**N = 600**) over a 12-week period  
- **Treatments (randomized schedule):** channel × amount × delivery method  
- **Amounts:** $100 and $250 (balanced within channel)  
- **Delivery:** in-person (Bank/MTS only) and online (Fintech/Crypto online-only)

## Co-primary outcomes
For each transaction, confederates document:
1. **Transaction success**
2. **KYC/AML compliance stringency**
3. **Transaction cost**
4. **Transaction time**

## Repository layout
```
├── code/
│ ├── 01_randomization/ # schedule generation + balance checks
│ ├── 02_operations/ # payment tracking, reminders, QC utilities
│ ├── 03_cleaning/ # cleaning + construction of outcomes
│ ├── 04_analysis/ # pre-specified models + tables/figures
│ └── 05_reporting/ # replication outputs / appendix tables
├── data/
│ ├── raw/ # NOT tracked (local only; sensitive)
│ ├── processed/ # access-controlled analytic files
│ └── outputs/ # safe tables/figures for sharing
├── docs/
│ ├── pap/ # pre-analysis plan + appendices (or pointers)
│ ├── codebook/ # variable definitions + coding rules
│ └── templates/ # transaction logs / field-note templates
└── README.md
```

## Getting started
### Requirements
- R (recommended)
- Common packages used in this repo include `tidyverse` and `lubridate` (see script headers for exact dependencies).

### Reproducibility
Where applicable, scripts:
- set explicit random seeds,
- write deterministic outputs to `data/processed/` or `data/outputs/`,
- include lightweight balance/sanity checks.

## Data access and security (important)
This project includes sensitive information about confederates and institutions. **Do not commit**:
- personally identifiable confederate data (names, contact info, IDs),
- linking files that map confederate IDs to identities,
- country-specific institution lists or identifiers that could enable re-identification,
- raw receipts/screenshots or other source documents with identifiers.

Use tiered data handling:
- **Tier 1 (Identifiable):** admin/compliance only, restricted access  
- **Tier 2 (Coded):** analysis-ready but access-controlled  
- **Tier 3 (De-identified):** aggregated outputs suitable for publication/replication

## Operational workflow (high level)
1. Confederates receive a pre-randomized transaction schedule.  
2. Research team sends funds to the confederate.  
3. Confederate executes the transaction per protocol.  
4. Confederate records outcomes and submits logs/receipts.  
5. Research team reviews submissions, codes KYC, and maintains the analytic database.

## Contributing
- Keep scripts modular and well-commented.
- Avoid hard-coding paths; use relative paths from the project root.
- Never commit sensitive data (see Data access and security section).

## Citation
Pre-Analysis Plan: *A Multi-Country Comparative Evaluation of KYC/AML Standards in Latin America*, **Version 0.6 (22 Oct 2025)**.

## License
Add the appropriate license for this repository (internal/private by default unless explicitly approved for public release).
