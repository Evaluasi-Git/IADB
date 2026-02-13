# Authors: Michael G. Findley and Cedric Antunes (Evaluasi) --------------------
# Date: 13 January, 2025 -------------------------------------------------------

# Cleaning the environment 
rm(list = ls())

# ------------------------------------------------------------------------------
# IADB KYC AUDIT STUDY: RANDOMIZATION + SCHEDULE GENERATOR ---------------------
# ------------------------------------------------------------------------------

# Required packages ------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ------------------------------------------------------------------------------
# Global configurations --------------------------------------------------------
# ------------------------------------------------------------------------------
GLOBAL_SEED <- 20251021

N_CONFEDERATES <- 15
N_TX_PER_CONF  <- 40

CHANNELS <- c("Bank", "MTS", "Fintech", "Crypto")
AMOUNTS  <- c(100, 250)

# Temporal balance: 4 blocks × 10 transactions, 
# each transaction channel appears 2–3 times per block
N_BLOCKS   <- 4
BLOCK_SIZE <- 10
MIN_PER_BLOCK <- 2
MAX_PER_BLOCK <- 3

# No clustering
MAX_CONSECUTIVE <- 2

# In-person delivery (Bank/MTS only)
INPERSON_PER_CHANNEL <- 2

# Phase intensity (per confederate)
# Weeks 1–4: 15 tx; Weeks 5–8: 15 tx; Weeks 9–12: 10 tx
PHASE_WEEK_SETS <- list(
  phase1 = 1:4,
  phase2 = 5:8,
  phase3 = 9:12
)

PHASE_COUNTS <- c(15, 15, 10)

# Scheduling calendar ----------------------------------------------------------
# Targetting the first week of February: Monday
STUDY_START_MONDAY <- as.Date("2026-02-02") 

# Prioritizing account openings in weeks 1–2 for channels 
# likely to require new accounts
EARLY_WEEKS_FOR_ACCOUNT_OPENING <- 1:2

REQUIRES_OPENING <- c(Bank = FALSE, 
                      MTS = FALSE, 
                      # Requires opening 
                      Fintech = TRUE, 
                      Crypto = TRUE)

# Spreading in-person across weeks (avoiding multiple in-person in same week)
# and "maximum one per week for first four weeks" 
MAX_INPERSON_PER_WEEK <- 1

# Respecting practical frequency constraints: at most one transaction per day
MAX_TX_PER_DAY <- 1

# Weekday-only scheduling (Mon–Fri) 
WEEKDAY_OFFSETS <- 0:4

# Output (common) paths
OUT_DIR <- "IADB/data/randomization"
dir.create(OUT_DIR, recursive = TRUE, 
           showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Helper functions -------------------------------------------------------------
# ------------------------------------------------------------------------------
check_consecutive <- function(x, max_consecutive = 2) {
  max(rle(x)$lengths) <= max_consecutive
}

# Balanicing
make_block_extras <- function(channels, n_blocks = 4, extras_per_block = 2, extras_per_channel = 2) {
  stopifnot(length(channels) * extras_per_channel == n_blocks * extras_per_block)
  
  pool <- rep(channels, extras_per_channel)
  
  attempts <- 0
  repeat {
    attempts <- attempts + 1
    sh <- sample(pool, length(pool), replace = FALSE)
    blocks <- split(sh, rep(1:n_blocks, each = extras_per_block))
    
    # Ensuring each block gets two DISTINCT channels as extras
    ok <- all(map_lgl(blocks, ~ length(unique(.x)) == extras_per_block))
    if (ok) return(blocks)
    
    if (attempts > 10000) stop("Failed to generate block extras.")
  }
}

# Generating sequence ----------------------------------------------------------
# - 10 per channel overall
# - per block (10) each channel 2–3
# - max 2 consecutive
# - at least one Fintech and one Crypto in early account-opening weeks 
generate_channel_sequence <- function(seed,
                                      channels = CHANNELS,
                                      n_blocks = N_BLOCKS,
                                      block_size = BLOCK_SIZE,
                                      max_consecutive = MAX_CONSECUTIVE) {
  set.seed(seed)
  
  extras <- make_block_extras(channels, n_blocks = n_blocks, extras_per_block = 2, extras_per_channel = 2)
  
  attempts <- 0
  repeat {
    attempts <- attempts + 1
    
    blocks <- map(1:n_blocks, function(b) {
      counts <- rep(MIN_PER_BLOCK, length(channels))
      names(counts) <- channels
      counts[extras[[b]]] <- counts[extras[[b]]] + 1  # two channels get +1 => 3
      sample(rep(names(counts), times = counts), size = block_size, replace = FALSE)
    })
    
    seq_all <- unlist(blocks, use.names = FALSE)
    
    if (!check_consecutive(seq_all, max_consecutive)) {
      if (attempts > 50000) stop("Failed to generate channel sequence with consecutive constraint.")
      next
    }
    
    # Hard check
    tab <- table(seq_all)
    if (all(tab[channels] == 10)) return(seq_all)
  }
}

# Assigning amounts within channel: exactly 5×100 and 5×250 for each channel
assign_amounts <- function(channels_seq, seed, amounts = AMOUNTS) {
  set.seed(seed)
  out <- numeric(length(channels_seq))
  
  for (ch in unique(channels_seq)) {
    idx <- which(channels_seq == ch)
    out[idx] <- sample(rep(amounts, each = 5), size = length(idx), replace = FALSE)
  }
  out
}

# Assigning weeks to transaction order with phase totals (15/15/10) and weekly spread
assign_weeks_with_phases <- function(channels_seq, seed) {
  set.seed(seed)
  
  # Helper: weekly counts for 4-week phase that sum to target
  make_phase_week_counts <- function(total) {
    # totals are fixed: 15 -> 4,4,4,3 ; 10 -> 3,3,2,2
    if (total == 15) return(sample(c(4,4,4,3), 4))
    if (total == 10) return(sample(c(3,3,2,2), 4))
    stop("Unsupported phase total.")
  }
  
  attempts <- 0
  repeat {
    attempts <- attempts + 1
    
    # Phase 1 (orders 1 to 15) -> weeks 1 to 4
    c1 <- make_phase_week_counts(15)
    w1 <- rep(PHASE_WEEK_SETS$phase1, times = c1)
    w1 <- sample(w1, length(w1), replace = FALSE)
    
    # Phase 2 (orders 16 to 30) -> weeks 5 to 8
    c2 <- make_phase_week_counts(15)
    w2 <- rep(PHASE_WEEK_SETS$phase2, times = c2)
    w2 <- sample(w2, length(w2), replace = FALSE)
    
    # Phase 3 (orders 31 to 40) -> weeks 9 to 12
    c3 <- make_phase_week_counts(10)
    w3 <- rep(PHASE_WEEK_SETS$phase3, times = c3)
    w3 <- sample(w3, length(w3), replace = FALSE)
    
    weeks <- c(w1, w2, w3)
    
    # Enforcing account-opening priority: earliest Fintech and Crypto in weeks 1–2
    fintech_weeks <- weeks[channels_seq == "Fintech"]
    crypto_weeks  <- weeks[channels_seq == "Crypto"]
    
    ok_opening <- (min(fintech_weeks) %in% EARLY_WEEKS_FOR_ACCOUNT_OPENING) &&
      (min(crypto_weeks) %in% EARLY_WEEKS_FOR_ACCOUNT_OPENING)
    
    if (ok_opening) return(weeks)
    
    if (attempts > 20000) stop("Failed to assign weeks satisfying account-opening constraint.")
  }
}

# Assigning weekday-only dates within each week, 
# with max 1 transaction per day 
assign_dates <- function(weeks_vec, channels_seq, seed) {
  set.seed(seed)
  
  n <- length(weeks_vec)
  dates <- rep(as.Date(NA), n)
  
  # For each week, pick unique weekdays (Mon–Fri) for the transactions that week
  for (w in sort(unique(weeks_vec))) {
    idx <- which(weeks_vec == w)
    n_w <- length(idx)
    
    if (n_w > length(WEEKDAY_OFFSETS)) {
      stop("Week has >5 transactions; cannot enforce weekday-only one-per-day rule.")
    }
    
    # Choose unique weekdays for the week (one transaction per day)
    offs <- sample(WEEKDAY_OFFSETS, size = n_w, replace = FALSE)
    dates[idx] <- STUDY_START_MONDAY + weeks(w - 1) + days(offs)
  }
  
  dates
}

# Choosing in-person transactions for Bank and MTS:
choose_inperson_indices <- function(channels_seq, weeks_vec, seed) {
  set.seed(seed)
  
  delivery <- rep("Online", length(channels_seq))
  
  bank_idx <- which(channels_seq == "Bank")
  mts_idx  <- which(channels_seq == "MTS")
  
  attempts <- 0
  repeat {
    attempts <- attempts + 1
    
    # Picking in-person candidates
    bank_pick <- sample(bank_idx, INPERSON_PER_CHANNEL, replace = FALSE)
    mts_pick  <- sample(mts_idx,  INPERSON_PER_CHANNEL, replace = FALSE)
    picks <- c(bank_pick, mts_pick)
    
    # Constraint: max in-person per week
    wk_counts <- table(weeks_vec[picks])
    ok_week <- all(wk_counts <= MAX_INPERSON_PER_WEEK)
    
    if (ok_week) {
      delivery[picks] <- "In-person"
      return(delivery)
    }
    
    if (attempts > 20000) stop("Failed to assign in-person transactions under week constraints.")
  }
}

# ------------------------------------------------------------------------------
# Generating confederates' schedule --------------------------------------------
# ------------------------------------------------------------------------------
generate_confederate_schedule <- function(confederate_id,
                                          country,
                                          global_seed = GLOBAL_SEED) {
  # Confederate-specific seed stream
  seed_base <- global_seed + confederate_id
  
  # Channel sequence (blocks + max consecutive)
  ch_seq <- generate_channel_sequence(seed = seed_base + 100)
  
  # Amounts within channel 
  amt <- assign_amounts(ch_seq, seed = seed_base + 200)
  
  # Week assignment with phase totals + account opening constraint
  wk <- assign_weeks_with_phases(ch_seq, seed = seed_base + 300)
  
  # Dates (weekday-only; max 1 tx/day)
  dt <- assign_dates(wk, ch_seq, seed = seed_base + 400)
  
  # Delivery methods (2 per Bank and 2 per MTS by default), spread across weeks
  delivery <- choose_inperson_indices(ch_seq, wk, seed = seed_base + 500)
  
  # Compile schedule
  schedule <- tibble(
    confederate_id    = confederate_id,
    country           = country,
    transaction_order = 1:N_TX_PER_CONF,
    block_10          = rep(1:N_BLOCKS, each = BLOCK_SIZE),
    phase             = case_when(
      wk %in% PHASE_WEEK_SETS$phase1 ~ 1L,
      wk %in% PHASE_WEEK_SETS$phase2 ~ 2L,
      wk %in% PHASE_WEEK_SETS$phase3 ~ 3L,
      TRUE ~ NA_integer_
    ),
    assigned_week     = wk,
    approximate_date  = dt,
    channel           = ch_seq,
    amount_usd        = amt,
    delivery_method   = delivery
  ) |>
    arrange(transaction_order)
  
  # Sanity checks
  # Channel totals
  stopifnot(all(table(schedule$channel)[CHANNELS] == 10))
  
  # Amount totals within channel
  for (ch in CHANNELS) {
    idx <- which(schedule$channel == ch)
    stopifnot(sum(schedule$amount_usd[idx] == 100) == 5)
    stopifnot(sum(schedule$amount_usd[idx] == 250) == 5)
  }
  
  # Block balance: 2–3 per channel per block of 10
  for (b in 1:N_BLOCKS) {
    sub <- schedule |> filter(block_10 == b)
    tab <- table(sub$channel)
    stopifnot(all(tab[CHANNELS] >= MIN_PER_BLOCK))
    stopifnot(all(tab[CHANNELS] <= MAX_PER_BLOCK))
  }
  
  # Max consecutive channels
  stopifnot(check_consecutive(schedule$channel, MAX_CONSECUTIVE))
  
  # Phase totals (15/15/10)
  stopifnot(sum(schedule$assigned_week %in% 1:4)  == 15)
  stopifnot(sum(schedule$assigned_week %in% 5:8)  == 15)
  stopifnot(sum(schedule$assigned_week %in% 9:12) == 10)
  
  # Account opening priority (Fintech + Crypto earliest in weeks 1–2)
  stopifnot(min(schedule$assigned_week[schedule$channel == "Fintech"]) %in% EARLY_WEEKS_FOR_ACCOUNT_OPENING)
  stopifnot(min(schedule$assigned_week[schedule$channel == "Crypto"])  %in% EARLY_WEEKS_FOR_ACCOUNT_OPENING)
  
  # Delivery totals
  stopifnot(sum(schedule$delivery_method[schedule$channel == "Bank"] == "In-person") == INPERSON_PER_CHANNEL)
  stopifnot(sum(schedule$delivery_method[schedule$channel == "MTS"]  == "In-person") == INPERSON_PER_CHANNEL)
  stopifnot(all(schedule$delivery_method[schedule$channel %in% c("Fintech", "Crypto")] == "Online"))
  
  # Spread in-person across weeks (max 1 per week)
  stopifnot(all(table(schedule$assigned_week[schedule$delivery_method == "In-person"]) <= MAX_INPERSON_PER_WEEK))
  
  # Weekday-only, max 1 tx/day
  stopifnot(all(wday(schedule$approximate_date, week_start = 1) %in% 1:5))
  stopifnot(all(table(schedule$approximate_date) <= MAX_TX_PER_DAY))
  
  schedule
}

# ------------------------------------------------------------------------------
# Confederates country mapping -------------------------------------------------
# ------------------------------------------------------------------------------
# Countries with 2 confederates: Mexico, Colombia, Brazil, Guatemala, El Salvador
# Countries with 1: Argentina, Chile, Peru, Costa Rica, Jamaica
confederates_df <- tibble(
  country = c(rep("Mexico", 2),
              rep("Colombia", 2),
              rep("Brazil", 2),
              rep("Guatemala", 2),
              rep("El Salvador", 2),
              "Argentina", "Chile", "Peru", "Costa Rica", "Jamaica")
) |>
  mutate(confederate_id = row_number()) |>
  select(confederate_id, country)

stopifnot(nrow(confederates_df) == N_CONFEDERATES)

# ------------------------------------------------------------------------------
# Generating all schedules and exporting ---------------------------------------
# ------------------------------------------------------------------------------
set.seed(GLOBAL_SEED)

all_schedules <- vector("list", N_CONFEDERATES)

for (i in seq_len(N_CONFEDERATES)) {
  conf_id <- confederates_df$confederate_id[i]
  ctry    <- confederates_df$country[i]
  
  cat(sprintf("Generating schedule for confederate %02d (%s)\n", conf_id, ctry))
  sched <- generate_confederate_schedule(conf_id, ctry, global_seed = GLOBAL_SEED)
  
  all_schedules[[i]] <- sched
  write_csv(sched, file.path(OUT_DIR, sprintf("confederate_%02d.csv", conf_id)))
}

master_schedule <- bind_rows(all_schedules) |>
  arrange(confederate_id, transaction_order)

write_csv(master_schedule, file.path(OUT_DIR, "master_schedule.csv"))

# ------------------------------------------------------------------------------
# Balance verification tables --------------------------------------------------
# ------------------------------------------------------------------------------
conf_balance <- master_schedule |>
  group_by(confederate_id) |>
  summarise(
    total = n(),
    bank  = sum(channel == "Bank"),
    mts   = sum(channel == "MTS"),
    fintech = sum(channel == "Fintech"),
    crypto  = sum(channel == "Crypto"),
    small_100 = sum(amount_usd == 100),
    large_250 = sum(amount_usd == 250),
    in_person = sum(delivery_method == "In-person"),
    online    = sum(delivery_method == "Online"),
    .groups = "drop"
  )

study_balance <- master_schedule |>
  summarise(
    total = n(),
    bank  = sum(channel == "Bank"),
    mts   = sum(channel == "MTS"),
    fintech = sum(channel == "Fintech"),
    crypto  = sum(channel == "Crypto"),
    small_100 = sum(amount_usd == 100),
    large_250 = sum(amount_usd == 250),
    in_person = sum(delivery_method == "In-person"),
    online    = sum(delivery_method == "Online")
  )
