manual_review <- bind_rows(
  analysis_df |>
    filter(is.na(transaction_uid) | is.na(confederate_id) | confederate_id == "") |>
    mutate(review_issue = "missing_confederate_or_uid"),

  analysis_df |>
    filter(flag_duplicate_transaction_uid) |>
    mutate(review_issue = "duplicate_transaction_uid"),

  analysis_df |>
    filter(success == 1 & flag_completed_but_no_received_confirmation) |>
    mutate(review_issue = "completed_but_not_confirmed_received")
) |>
  arrange(review_issue, confederate_id, transaction_id, submission_datetime) |>
  select(
    review_issue,
    transaction_uid,
    transaction_id,
    confederate_id,
    country_clean,
    channel,
    amount,
    delivery,
    transaction_outcome_label,
    success,
    j2_confirmed_received_num,
    time_hours,
    transaction_duration_hours,
    submission_datetime,
    reviewed_by_team,
    data_quality_flag,
    everything()
  )

write_csv(
  manual_review,
  file.path(output_dir, "IADB_manual_review_flags_apr23.csv")
)
