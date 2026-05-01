
# app.R
# Power-Duration Relationship App
# Version 1 rebuild based on the master build spec

# ------------------------------
# Packages
# ------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(scales)
library(tidyr)
library(DT)

# ------------------------------
# Global constants
# ------------------------------

test_durations <- c(
  "45 s" = 45,
  "3 min" = 180,
  "6 min" = 360,
  "10 min" = 600,
  "20 min" = 1200
)

prediction_durations <- c(
  "1 min" = 60,
  "2 min" = 120,
  "4 min" = 240,
  "8 min" = 480,
  "12 min" = 720,
  "15 min" = 900,
  "30 min" = 1800,
  "60 min" = 3600
)

history_required_cols <- c(
  "athlete_name", "test_date", "power_45s", "power_3min", "power_6min",
  "power_10min", "power_20min"
)

history_optional_cols <- c(
  "notes", "ergometer_type", "protocol_confirmed", "test_validity_flag",
  "estimated_cp", "estimated_w_prime", "riegel_S", "riegel_E",
  "predicted_power_1min", "predicted_power_2min", "predicted_power_4min",
  "predicted_power_8min", "predicted_power_12min", "predicted_power_15min",
  "predicted_power_30min", "predicted_power_60min", "warning_flags",
  "confidence_summary"
)

# ------------------------------
# Helper functions
# ------------------------------

empty_history <- function() {
  tibble(
    athlete_name = character(),
    test_date = as.Date(character()),
    power_45s = numeric(),
    power_3min = numeric(),
    power_6min = numeric(),
    power_10min = numeric(),
    power_20min = numeric(),
    notes = character(),
    ergometer_type = character(),
    protocol_confirmed = logical(),
    test_validity_flag = character(),
    estimated_cp = numeric(),
    estimated_w_prime = numeric(),
    riegel_S = numeric(),
    riegel_E = numeric(),
    predicted_power_1min = numeric(),
    predicted_power_2min = numeric(),
    predicted_power_4min = numeric(),
    predicted_power_8min = numeric(),
    predicted_power_12min = numeric(),
    predicted_power_15min = numeric(),
    predicted_power_30min = numeric(),
    predicted_power_60min = numeric(),
    warning_flags = character(),
    confidence_summary = character()
  )
}

build_example_upload_csv <- function() {
  tibble(
    athlete_name = "Sample Athlete",
    test_date = as.Date("2026-03-01"),
    power_45s = 620,
    power_3min = 410,
    power_6min = 360,
    power_10min = 330,
    power_20min = 295,
    ergometer_type = "Concept2 BikeErg",
    notes = "Example row for upload formatting. Replace with your own values."
  )
}

build_test_dataframe <- function(input_list) {
  tibble(
    athlete_name = trimws(as.character(input_list$athlete_name)),
    test_date = as.Date(input_list$test_date),
    power_45s = suppressWarnings(as.numeric(input_list$power_45s)),
    power_3min = suppressWarnings(as.numeric(input_list$power_3min)),
    power_6min = suppressWarnings(as.numeric(input_list$power_6min)),
    power_10min = suppressWarnings(as.numeric(input_list$power_10min)),
    power_20min = suppressWarnings(as.numeric(input_list$power_20min)),
    notes = trimws(as.character(input_list$notes)),
    ergometer_type = trimws(as.character(input_list$ergometer_type)),
    protocol_confirmed = isTRUE(input_list$protocol_confirmed),
    test_validity_flag = as.character(input_list$test_validity_flag)
  )
}

to_long_profile <- function(test_row) {
  tibble(
    duration_label = factor(names(test_durations), levels = names(test_durations)),
    duration_sec = unname(test_durations),
    power_w = c(
      test_row$power_45s,
      test_row$power_3min,
      test_row$power_6min,
      test_row$power_10min,
      test_row$power_20min
    )
  )
}

validate_test_values <- function(test_row, history_df = NULL) {
  watts <- c(
    test_row$power_45s,
    test_row$power_3min,
    test_row$power_6min,
    test_row$power_10min,
    test_row$power_20min
  )

  errors <- character()
  warnings <- character()
  flags <- character()

  if (is.na(test_row$athlete_name) || test_row$athlete_name == "") {
    errors <- c(errors, "Athlete name is required.")
  }

  if (is.na(test_row$test_date)) {
    errors <- c(errors, "Test date is required.")
  }

  if (any(is.na(watts))) {
    errors <- c(errors, "All five watt values are required.")
  }

  if (any(!is.finite(watts), na.rm = TRUE)) {
    errors <- c(errors, "All watt values must be valid numbers.")
  }

  if (any(watts <= 0, na.rm = TRUE)) {
    errors <- c(errors, "All watt values must be greater than zero.")
  }

  if (length(unique(watts[!is.na(watts)])) == 1 && sum(!is.na(watts)) == 5) {
    errors <- c(errors, "All five watt values are identical. Recheck the entries before continuing.")
  }

  if (sum(!is.na(watts)) == 5) {
    if (!(test_row$power_45s > test_row$power_3min &&
          test_row$power_3min > test_row$power_6min &&
          test_row$power_6min > test_row$power_10min &&
          test_row$power_10min > test_row$power_20min)) {
      errors <- c(errors, "Power must decrease as duration gets longer for this fixed test set. Recheck the entries or repeat the affected test.")
      flags <- c(flags, "order_violation")
    }

    drop_ratios <- c(
      test_row$power_3min / test_row$power_45s,
      test_row$power_6min / test_row$power_3min,
      test_row$power_10min / test_row$power_6min,
      test_row$power_20min / test_row$power_10min
    )

    if (any(drop_ratios > 0.98)) {
      warnings <- c(warnings, "One or more adjacent durations are unusually close together. This may reflect pacing, non-maximal effort, or a data-entry problem.")
      flags <- c(flags, "flat_segment")
    }

    if (any(drop_ratios < 0.65)) {
      warnings <- c(warnings, "One or more adjacent drops are unusually large. This may reflect pacing error, fatigue, or a typo.")
      flags <- c(flags, "sharp_drop")
    }

    if ((test_row$power_20min / test_row$power_45s) < 0.35) {
      warnings <- c(warnings, "Your 20-minute power is unusually low relative to your 45-second power. Interpret this profile cautiously.")
      flags <- c(flags, "low_20min_anchor")
    }

    if ((test_row$power_45s / test_row$power_3min) > 2.2) {
      warnings <- c(warnings, "Your 45-second power is unusually high relative to your 3-minute power. Check pacing or entry accuracy.")
      flags <- c(flags, "high_short_bias")
    }
  }

  if (!isTRUE(test_row$protocol_confirmed)) {
    warnings <- c(warnings, "Protocol confirmation is unchecked. Interpret results more cautiously if the full testing window was not followed.")
    flags <- c(flags, "protocol_unconfirmed")
  }

  if (!is.null(history_df) && nrow(history_df) > 0 && !is.na(test_row$athlete_name) && test_row$athlete_name != "") {
    same_athlete <- history_df %>%
      filter(athlete_name == test_row$athlete_name) %>%
      arrange(desc(test_date))

    if (nrow(same_athlete) > 0) {
      latest <- same_athlete[1, ]
      hist_watts <- c(latest$power_45s, latest$power_3min, latest$power_6min, latest$power_10min, latest$power_20min)
      pct_change <- abs((watts - hist_watts) / hist_watts)

      if (any(pct_change > 0.25, na.rm = TRUE)) {
        warnings <- c(warnings, "At least one duration differs substantially from your most recent saved test. This may be real improvement, fatigue, or a testing inconsistency.")
        flags <- c(flags, "history_mismatch")
      }
    }
  }

  confidence_summary <- if (length(errors) > 0) {
    "Do not use this profile until the blocking issues are fixed."
  } else if (length(warnings) > 0) {
    "Usable with caution. Review warnings before saving or interpreting the profile."
  } else {
    "Usable. No major data-quality issues were detected from the entered values."
  }

  retest_level <- if (length(errors) > 0) {
    "Retest recommended"
  } else if (length(warnings) >= 2) {
    "Retest suggested"
  } else if (length(warnings) == 1) {
    "Keep, but interpret cautiously"
  } else {
    "No retest signal"
  }

  list(
    errors = unique(errors),
    warnings = unique(warnings),
    flags = unique(flags),
    confidence_summary = confidence_summary,
    retest_level = retest_level
  )
}

fit_power_duration_model <- function(profile_long) {
  fit <- lm(log(power_w) ~ log(duration_sec), data = profile_long)

  curve_grid <- tibble(
    duration_sec = exp(seq(log(45), log(3600), length.out = 300))
  ) %>%
    mutate(pred_power_w = exp(predict(fit, newdata = .)))

  list(
    model = fit,
    curve = curve_grid,
    intercept = unname(coef(fit)[1]),
    slope = unname(coef(fit)[2]),
    a = exp(unname(coef(fit)[1])),
    b = unname(coef(fit)[2])
  )
}

predict_power_curve <- function(model_fit, durations_seconds) {
  tibble(
    duration_label = names(durations_seconds),
    duration_sec = unname(durations_seconds),
    predicted_power_w = exp(predict(model_fit$model, newdata = tibble(duration_sec = unname(durations_seconds))))
  )
}

estimate_secondary_metrics <- function(profile_long) {
  cp_data <- profile_long %>%
    filter(duration_sec >= 180) %>%
    mutate(work_j = power_w * duration_sec)

  if (nrow(cp_data) < 2) {
    return(list(cp_w = NA_real_, wprime_j = NA_real_))
  }

  fit <- lm(work_j ~ duration_sec, data = cp_data)
  cp_w <- unname(coef(fit)[["duration_sec"]])
  wprime_j <- unname(coef(fit)[["(Intercept)"]])

  if (!is.finite(cp_w) || cp_w <= 0) cp_w <- NA_real_
  if (!is.finite(wprime_j) || wprime_j <= 0) wprime_j <- NA_real_

  list(cp_w = cp_w, wprime_j = wprime_j)
}

build_riegel_metrics <- function(model_fit) {
  tibble(
    riegel_S = unname(model_fit$a),
    riegel_E = unname(model_fit$b + 1)
  )
}

build_summary_table <- function(test_row, model_fit, cp_metrics, validation_result) {
  riegel_metrics <- build_riegel_metrics(model_fit)

  tibble(
    metric = c(
      "Athlete",
      "Test date",
      "45 s power",
      "3 min power",
      "6 min power",
      "10 min power",
      "20 min power",
      "Estimated critical power",
      "Estimated W′",
      "Profile Power Level (Riegel S)",
      "Power-Duration Endurance (Riegel E)",
      "Confidence summary",
      "Retest guidance"
    ),
    value = c(
      test_row$athlete_name,
      as.character(test_row$test_date),
      paste0(round(test_row$power_45s, 0), " W"),
      paste0(round(test_row$power_3min, 0), " W"),
      paste0(round(test_row$power_6min, 0), " W"),
      paste0(round(test_row$power_10min, 0), " W"),
      paste0(round(test_row$power_20min, 0), " W"),
      ifelse(is.na(cp_metrics$cp_w), "Not available", paste0(round(cp_metrics$cp_w, 0), " W")),
      ifelse(is.na(cp_metrics$wprime_j), "Not available", paste0(round(cp_metrics$wprime_j / 1000, 1), " kJ")),
      ifelse(is.na(riegel_metrics$riegel_S), "Not available", paste0(round(riegel_metrics$riegel_S, 1), " W")),
      ifelse(is.na(riegel_metrics$riegel_E), "Not available", format(round(riegel_metrics$riegel_E, 3), nsmall = 3)),
      validation_result$confidence_summary,
      validation_result$retest_level
    )
  )
}

build_prediction_table <- function(model_fit) {
  predict_power_curve(model_fit, prediction_durations) %>%
    transmute(
      duration = duration_label,
      predicted_power_w = round(predicted_power_w, 0)
    )
}

build_training_targets <- function(pred_table, cp_metrics) {
  lookup <- setNames(pred_table$predicted_power_w, pred_table$duration)

  tibble(
    category = c(
      "Short-power repeats",
      "Anaerobic-capacity intervals",
      "VO2-type intervals",
      "Threshold-focused work",
      "Steady aerobic work"
    ),
    anchor = c(
      paste0(lookup[["1 min"]], " to ", lookup[["2 min"]], " W"),
      paste0(lookup[["2 min"]], " to ", lookup[["4 min"]], " W"),
      paste0(lookup[["4 min"]], " to ", lookup[["8 min"]], " W"),
      paste0(lookup[["12 min"]], " to ", lookup[["15 min"]], " W"),
      paste0(lookup[["30 min"]], " to ", lookup[["60 min"]], " W")
    ),
    use_case = c(
      "Hard short repeats with long recovery",
      "High-cost work where repeatability matters",
      "Intervals that stress hard aerobic power",
      "Sustained hard work anchored to longer efforts",
      "Longer steady sessions and controlled aerobic work"
    ),
    note = c(
      "Use as a starting range, not a single magic number.",
      "These efforts are highly fatiguing and sensitive to pacing.",
      "Choose a repeatable power, not the highest one-off effort.",
      "This is not a direct threshold diagnosis.",
      "Keep these practical and repeatable rather than maximal."
    ),
    cp_context = ifelse(
      is.na(cp_metrics$cp_w),
      "Secondary estimated CP not available",
      paste0("Secondary estimated CP: ", round(cp_metrics$cp_w, 0), " W")
    )
  )
}

make_profile_record <- function(test_row, model_fit, cp_metrics, pred_table, validation_result) {
  tibble(
    athlete_name = test_row$athlete_name,
    test_date = as.Date(test_row$test_date),
    power_45s = test_row$power_45s,
    power_3min = test_row$power_3min,
    power_6min = test_row$power_6min,
    power_10min = test_row$power_10min,
    power_20min = test_row$power_20min,
    notes = test_row$notes,
    ergometer_type = test_row$ergometer_type,
    protocol_confirmed = test_row$protocol_confirmed,
    test_validity_flag = test_row$test_validity_flag,
    estimated_cp = cp_metrics$cp_w,
    estimated_w_prime = cp_metrics$wprime_j,
    riegel_S = model_fit$a,
    riegel_E = model_fit$b + 1,
    predicted_power_1min = pred_table$predicted_power_w[pred_table$duration == "1 min"],
    predicted_power_2min = pred_table$predicted_power_w[pred_table$duration == "2 min"],
    predicted_power_4min = pred_table$predicted_power_w[pred_table$duration == "4 min"],
    predicted_power_8min = pred_table$predicted_power_w[pred_table$duration == "8 min"],
    predicted_power_12min = pred_table$predicted_power_w[pred_table$duration == "12 min"],
    predicted_power_15min = pred_table$predicted_power_w[pred_table$duration == "15 min"],
    predicted_power_30min = pred_table$predicted_power_w[pred_table$duration == "30 min"],
    predicted_power_60min = pred_table$predicted_power_w[pred_table$duration == "60 min"],
    warning_flags = paste(validation_result$flags, collapse = "; "),
    confidence_summary = validation_result$confidence_summary
  )
}

recompute_history_record <- function(row, history_df = empty_history()) {
  test_row <- tibble(
    athlete_name = as.character(row$athlete_name),
    test_date = as.Date(row$test_date),
    power_45s = as.numeric(row$power_45s),
    power_3min = as.numeric(row$power_3min),
    power_6min = as.numeric(row$power_6min),
    power_10min = as.numeric(row$power_10min),
    power_20min = as.numeric(row$power_20min),
    notes = if ("notes" %in% names(row)) as.character(row$notes) else "",
    ergometer_type = if ("ergometer_type" %in% names(row)) as.character(row$ergometer_type) else "",
    protocol_confirmed = if ("protocol_confirmed" %in% names(row)) as.logical(row$protocol_confirmed) else TRUE,
    test_validity_flag = if ("test_validity_flag" %in% names(row)) as.character(row$test_validity_flag) else "Complete"
  )

  validation_result <- validate_test_values(test_row, history_df)
  profile_long <- to_long_profile(test_row)
  model_fit <- fit_power_duration_model(profile_long)
  cp_metrics <- estimate_secondary_metrics(profile_long)
  pred_table <- build_prediction_table(model_fit)

  make_profile_record(
    test_row = test_row,
    model_fit = model_fit,
    cp_metrics = cp_metrics,
    pred_table = pred_table,
    validation_result = validation_result
  )
}

recompute_uploaded_history <- function(history_df) {
  if (nrow(history_df) == 0) {
    return(empty_history())
  }

  history_df <- history_df %>%
    arrange(test_date)

  computed_rows <- vector("list", nrow(history_df))
  prior_history <- empty_history()

  for (i in seq_len(nrow(history_df))) {
    current_row <- history_df[i, , drop = FALSE]
    computed_row <- recompute_history_record(current_row, prior_history)
    computed_rows[[i]] <- computed_row
    prior_history <- bind_rows(prior_history, computed_row) %>%
      arrange(test_date)
  }

  bind_rows(computed_rows) %>%
    select(all_of(names(empty_history())))
}

read_history_csv <- function(path) {
  raw <- read_csv(path, show_col_types = FALSE)
  validate_history_file(raw)
}

validate_history_file <- function(df) {
  missing_required <- setdiff(history_required_cols, names(df))
  if (length(missing_required) > 0) {
    stop(
      paste0(
        "Uploaded CSV is missing required columns: ",
        paste(missing_required, collapse = ", ")
      )
    )
  }

  out <- df %>%
    mutate(
      test_date = as.Date(test_date),
      power_45s = as.numeric(power_45s),
      power_3min = as.numeric(power_3min),
      power_6min = as.numeric(power_6min),
      power_10min = as.numeric(power_10min),
      power_20min = as.numeric(power_20min),
      protocol_confirmed = if ("protocol_confirmed" %in% names(.)) as.logical(protocol_confirmed) else TRUE
    ) %>%
    arrange(test_date)

  for (col in setdiff(names(empty_history()), names(out))) {
    template_col <- empty_history()[[col]]

    if (inherits(template_col, "Date")) {
      out[[col]] <- as.Date(rep(NA_character_, nrow(out)))
    } else if (is.logical(template_col)) {
      out[[col]] <- rep(NA, nrow(out))
    } else if (is.numeric(template_col)) {
      out[[col]] <- rep(NA_real_, nrow(out))
    } else {
      out[[col]] <- rep(NA_character_, nrow(out))
    }
  }

  out <- out %>%
    select(all_of(names(empty_history())))

  recompute_uploaded_history(out)
}

write_history_csv <- function(history_df, file) {
  write_csv(history_df, file)
}

prepare_history_trend_data <- function(history_df) {
  history_df %>%
    select(test_date, power_45s, power_3min, power_6min, power_10min, power_20min, estimated_cp) %>%
    pivot_longer(
      cols = -test_date,
      names_to = "metric",
      values_to = "power_w"
    ) %>%
    mutate(
      metric = factor(
        metric,
        levels = c("power_45s", "power_3min", "power_6min", "power_10min", "power_20min", "estimated_cp"),
        labels = c("45 s", "3 min", "6 min", "10 min", "20 min", "Estimated CP")
      )
    )
}

build_comparison_table <- function(current_row, history_df) {
  same_athlete <- history_df %>%
    filter(athlete_name == current_row$athlete_name) %>%
    filter(!(
      test_date == as.Date(current_row$test_date) &
        power_45s == current_row$power_45s &
        power_3min == current_row$power_3min &
        power_6min == current_row$power_6min &
        power_10min == current_row$power_10min &
        power_20min == current_row$power_20min
    )) %>%
    arrange(desc(test_date))

  if (nrow(same_athlete) == 0) {
    return(tibble(Message = "No prior saved test exists yet for this athlete. Save at least one earlier test to build comparisons."))
  }

  prior <- same_athlete[1, ]

  current_vals <- c(current_row$power_45s, current_row$power_3min, current_row$power_6min, current_row$power_10min, current_row$power_20min)
  prior_vals <- c(prior$power_45s, prior$power_3min, prior$power_6min, prior$power_10min, prior$power_20min)

  current_model_fit <- fit_power_duration_model(to_long_profile(current_row))
  current_riegel <- build_riegel_metrics(current_model_fit)

  comparison_rows <- tibble(
    metric = c("45 s power", "3 min power", "6 min power", "10 min power", "20 min power"),
    current_value = current_vals,
    prior_value = prior_vals
  ) %>%
    bind_rows(
      tibble(
        metric = c("Profile Power Level (Riegel S)", "Power-Duration Endurance (Riegel E)"),
        current_value = c(current_riegel$riegel_S, current_riegel$riegel_E),
        prior_value = c(prior$riegel_S, prior$riegel_E)
      )
    ) %>%
    mutate(
      absolute_change = current_value - prior_value,
      percent_change = if_else(
        is.na(prior_value) | prior_value == 0,
        NA_real_,
        100 * absolute_change / prior_value
      ),
      current_value = case_when(
        metric == "Power-Duration Endurance (Riegel E)" ~ format(round(current_value, 3), nsmall = 3),
        TRUE ~ as.character(round(current_value, 0))
      ),
      prior_value = case_when(
        metric == "Power-Duration Endurance (Riegel E)" ~ format(round(prior_value, 3), nsmall = 3),
        TRUE ~ as.character(round(prior_value, 0))
      ),
      absolute_change = case_when(
        metric == "Power-Duration Endurance (Riegel E)" ~ format(round(absolute_change, 3), nsmall = 3),
        TRUE ~ as.character(round(absolute_change, 0))
      ),
      percent_change = if_else(
        is.na(percent_change),
        "NA",
        paste0(format(round(percent_change, 1), nsmall = 1), "%")
      )
    )

  comparison_rows
}

format_duration_label <- function(sec) {
  if (sec < 60) {
    paste0(round(sec), " s")
  } else {
    paste0(round(sec / 60, 1), " min")
  }
}

protocol_html <- HTML(
  paste0(
    "<h3>11-Day Testing Window</h3>",
    "<p>Complete five maximal efforts across an 11-day testing window.</p>",
    "<table class='table table-bordered'>",
    "<thead><tr><th>Week</th><th>Day</th><th>Session</th></tr></thead>",
    "<tbody>",
    "<tr><td>1</td><td>Monday</td><td>45-second maximal effort</td></tr>",
    "<tr><td>1</td><td>Wednesday</td><td>3-minute maximal effort</td></tr>",
    "<tr><td>1</td><td>Friday</td><td>6-minute maximal effort</td></tr>",
    "<tr><td>2</td><td>Monday</td><td>10-minute maximal effort</td></tr>",
    "<tr><td>2</td><td>Thursday</td><td>20-minute maximal effort</td></tr>",
    "</tbody></table>",
    "<h4>Standardization Rules</h4>",
    "<ul>",
    "<li>Use the same ergometer and settings for every test.</li>",
    "<li>Test at about the same time of day when possible.</li>",
    "<li>Use the same warm-up before each test.</li>",
    "<li>Avoid hard conditioning during the testing window.</li>",
    "<li>The weekend is a recovery bridge, not a hard training block.</li>",
    "</ul>",
    "<h4>Warm-Up Template</h4>",
    "<ul>",
    "<li>5 minutes easy</li>",
    "<li>3 minutes moderate</li>",
    "<li>2 x 20 seconds strong with 1 minute easy between</li>",
    "<li>3 to 5 minutes easy before the test starts</li>",
    "</ul>",
    "<h4>Pacing Guidance</h4>",
    "<ul>",
    "<li><strong>45 s:</strong> Start hard, but do not empty the tank in the first 10 seconds.</li>",
    "<li><strong>3 min:</strong> Start assertively, then settle into a hard sustainable effort.</li>",
    "<li><strong>6 min:</strong> Pace hard but controlled. Avoid a reckless first minute.</li>",
    "<li><strong>10 min:</strong> Treat this as a hard sustained effort, not a sprint start with damage control later.</li>",
    "<li><strong>20 min:</strong> Start slightly under all-out and build if possible. This is the most pacing-sensitive test.</li>",
    "</ul>",
    "<h4>When to Delay or Repeat a Test</h4>",
    "<ul>",
    "<li>Delay a test if you are sick, unusually fatigued, or clearly not ready to give a maximal effort.</li>",
    "<li>Repeat a test if pacing failed badly or a result was clearly compromised by fatigue or equipment issues.</li>",
    "<li>Bad data are worse than delayed data.</li>",
    "</ul>",
    "<h4>What Counts as a Valid Result</h4>",
    "<ul>",
    "<li>The full target duration was completed.</li>",
    "<li>The effort was maximal for that duration.</li>",
    "<li>Conditions were reasonably standardized.</li>",
    "<li>The test was not obviously distorted by fatigue, illness, or setup problems.</li>",
    "</ul>"
  )
)

# ------------------------------
# UI
# ------------------------------

ui <- navbarPage(
  title = "Power-Duration App",

  tabPanel(
    "Testing Protocol",
    fluidPage(
      HTML(protocol_html)
    )
  ),

  tabPanel(
    "How to Use This App",
    fluidPage(
      h3("What This App Does"),
      p("This app estimates your power-duration profile using five maximal efforts."),
      p("It helps you track changes over time and generate practical training targets."),
      p("It does not directly measure physiological thresholds."),

      h3("Testing Workflow"),
      p("Complete the five required maximal efforts across the testing window shown in the Testing Protocol tab."),
      tags$ul(
        tags$li("45 seconds"),
        tags$li("3 minutes"),
        tags$li("6 minutes"),
        tags$li("10 minutes"),
        tags$li("20 minutes")
      ),
      p("Each effort should be a true maximal average effort for the full duration."),

      h3("Enter Data Manually"),
      p("Use the Current Test Entry tab to enter one full test profile."),
      p("Include all five power values. Add ergometer type and notes if they help you interpret the result."),
      p("Click Save Current Test to History to store the test and calculate the model outputs."),

      h3("Upload Data with a CSV"),
      p("Most first-time users will enter data manually. Use CSV import only if you want to restore prior history or manage your data outside the app."),
      p("This app does not store your data between sessions. If you want to track progress over time, download your history and re-upload it when you return."),
      tags$ol(
        tags$li("Click Download Example CSV if you want a simple starting file."),
        tags$li("Or use a previously downloaded history CSV if you want to continue from prior tests."),
        tags$li("Edit the core input fields you want to manage directly."),
        tags$li("Upload the file in the Import / Export tab."),
        tags$li("Add your new test values after upload if needed, then save again.")
      ),
      p("Required columns: athlete_name, test_date, power_45s, power_3min, power_6min, power_10min, power_20min."),
      p("Optional columns: ergometer_type, notes."),
      p("The app calculates derived values after upload."),
      p("Uploading a CSV replaces the current session history."),

      h3("Read the Outputs"),
      p("Use the Current Test Entry tab to view the current profile, summary values, and predicted powers."),
      p("Use the Historical Comparison tab to compare the current test to the most recent prior saved test for the same athlete and to view trends across saved tests."),
      p("Use the Training Targets tab to view practical training anchors derived from the profile."),

      h3("Important Notes"),
      tags$ul(
        tags$li("Use the same ergometer when possible."),
        tags$li("Use consistent testing conditions."),
        tags$li("Make each effort truly maximal."),
        tags$li("Estimated values are model-based outputs, not direct physiological measurements.")
      )
    )
  ),

  tabPanel(
    "Current Test Entry",
    sidebarLayout(
      sidebarPanel(
        h4("Enter Current Test"),
        p("New user? Start with the Testing Protocol tab."),
        textInput("athlete_name", "Athlete name"),
        dateInput("test_date", "Test date", value = Sys.Date()),
        textInput("ergometer_type", "Ergometer type (optional)", value = ""),
        textAreaInput("notes", "Notes (optional)", rows = 3, placeholder = "Pacing notes, fatigue, sleep, setup issues, or anything unusual"),
        checkboxInput("protocol_confirmed", "I followed the testing protocol closely", value = TRUE),
        selectInput(
          "test_validity_flag",
          "Test validity flag",
          choices = c("Complete", "Compromised", "Retest needed"),
          selected = "Complete"
        ),
        tags$hr(),
        numericInput("power_45s", "45 s average power (W)", value = NA, min = 1),
        numericInput("power_3min", "3 min average power (W)", value = NA, min = 1),
        numericInput("power_6min", "6 min average power (W)", value = NA, min = 1),
        numericInput("power_10min", "10 min average power (W)", value = NA, min = 1),
        numericInput("power_20min", "20 min average power (W)", value = NA, min = 1),
        tags$hr(),
        actionButton("save_test", "Save Current Test to History"),
        br(), br(),
        textOutput("save_status")
      ),
      mainPanel(
        uiOutput("validation_box"),
        plotOutput("pd_plot", height = "430px"),
        tags$small("Points = your tested results. Line = fitted profile estimate."),
        tags$br(),
        tags$small("A steeper drop from left to right means power falls off faster as duration increases."),
        tags$br(),
        tags$small("Best comparisons come from the same ergometer, setup, and test conditions."),
        br(),
        fluidRow(
          column(
            6,
            h4("Current Summary"),
            tags$small("Riegel S describes overall profile power level. Riegel E describes how well power holds up as duration increases."),
            tableOutput("summary_table"),
            tags$small("Profile Power Level (Riegel S): Higher = higher overall power level across the profile."),
            tags$br(),
            tags$small("Power-Duration Endurance (Riegel E): Higher = less drop-off as duration gets longer.")
          ),
          column(6, h4("Predicted Powers"), tableOutput("prediction_table"))
        )
      )
    )
  ),

  tabPanel(
    "Historical Comparison",
    fluidPage(
      h3("Saved Test History"),
      p("Select a saved row to delete it from the current session history."),
      DTOutput("history_table"),
      br(),
      fluidRow(
        column(4, actionButton("delete_selected_test", "Delete Selected Test")),
        column(8, uiOutput("history_action_status"))
      ),
      br(),
      h3("Current vs Most Recent Saved Test"),
      tags$small("Compare changes in tested power first. Use Riegel S and Riegel E as summary descriptors, not as stand-alone verdicts."),
      tags$br(),
      tags$small("Small changes may reflect normal test variation. Compare tests done on the same ergometer and under similar conditions."),
      tableOutput("comparison_table"),
      br(),
      h3("Historical Trends"),
      plotOutput("history_plot", height = "420px")
    )
  ),

  tabPanel(
    "Training Targets",
    fluidPage(
      h3("Practical Training Targets"),
      p("These outputs are practical anchors derived from your entered field-test profile."),
      p("They are not direct measurements of lactate threshold, ventilatory thresholds, or exact metabolic zones."),
      tags$small("These are practical starting ranges derived from your profile."),
      tags$br(),
      tags$small("Use repeatability and workout response to adjust them."),
      tableOutput("training_targets_table")
    )
  ),

  tabPanel(
    "Predicted Power by Duration",
    fluidPage(
      h3("Predicted Power by Duration"),
      numericInput("custom_duration_sec", "Duration (seconds)", value = 600, min = 1, step = 1),
      tags$small("Estimate from your fitted profile. Use it as a planning reference, not an exact prediction."),
      br(), br(),
      uiOutput("custom_duration_prediction")
    )
  ),

  tabPanel(
    "Import / Export",
    fluidPage(
      h3("CSV Import and Export"),
      p(strong("Important: This app does not store your data between sessions.")),
      p("If you want to track your progress over time, download your history and re-upload it when you return."),
      helpText("Workflow: Download your history -> save it -> upload it next time -> add your new test."),
      p("Use CSV files to replace the current session history or download your saved history."),
      p("Download the example CSV if you want a correctly formatted file to edit and upload."),
      helpText("The example file includes the fields you should manage directly, including ergometer type. The app calculates derived values after upload."),
      p(strong("About the downloaded CSV")),
      p("The downloaded file includes your original test data and additional calculated values."),
      p("You only need to edit the core input fields: athlete name, test date, power values, ergometer type, and notes."),
      p("All other fields will be recalculated automatically when you upload the file."),
      helpText("If you are unsure, start from the Example CSV instead of editing a previous download."),
      fluidRow(
        column(
          width = 6,
          fileInput("upload_history", "Upload history CSV", accept = ".csv"),
          helpText("In version 1, uploading a CSV replaces the current in-session history.")
        ),
        column(
          width = 6,
          downloadButton("download_example_csv", "Download Example CSV"),
          br(), br(),
          downloadButton("download_history", "Download history CSV")
        )
      )
    )
  )
)

# ------------------------------
# Server
# ------------------------------

server <- function(input, output, session) {

  history_data <- reactiveVal(empty_history())
  save_status_message <- reactiveVal("")

  current_inputs <- reactive({
    build_test_dataframe(list(
      athlete_name = input$athlete_name,
      test_date = input$test_date,
      power_45s = input$power_45s,
      power_3min = input$power_3min,
      power_6min = input$power_6min,
      power_10min = input$power_10min,
      power_20min = input$power_20min,
      notes = input$notes,
      ergometer_type = input$ergometer_type,
      protocol_confirmed = input$protocol_confirmed,
      test_validity_flag = input$test_validity_flag
    ))
  })

  current_validation <- reactive({
    validate_test_values(current_inputs(), history_data())
  })

  validated_test <- reactive({
    req(length(current_validation()$errors) == 0)
    current_inputs()
  })

  current_profile <- reactive({
    to_long_profile(validated_test())
  })

  current_model_fit <- reactive({
    fit_power_duration_model(current_profile())
  })

  current_secondary_metrics <- reactive({
    estimate_secondary_metrics(current_profile())
  })

  current_predictions <- reactive({
    build_prediction_table(current_model_fit())
  })

  current_training_targets <- reactive({
    build_training_targets(current_predictions(), current_secondary_metrics())
  })

  output$validation_box <- renderUI({
    v <- current_validation()

    if (length(v$errors) == 0 && length(v$warnings) == 0) {
      return(
        tags$div(
          style = "padding:10px; background-color:#eef7ee; border:1px solid #cfe5cf; margin-bottom:15px;",
          tags$strong("Status: "),
          v$confidence_summary
        )
      )
    }

    tags$div(
      style = "padding:10px; background-color:#fff4e5; border:1px solid #e5c58a; margin-bottom:15px;",
      tags$strong("Input checks"),
      if (length(v$errors) > 0) {
        tags$div(
          tags$p(tags$strong("Blocking issues")),
          tags$ul(lapply(v$errors, tags$li))
        )
      },
      if (length(v$warnings) > 0) {
        tags$div(
          tags$p(tags$strong("Warnings")),
          tags$ul(lapply(v$warnings, tags$li))
        )
      },
      tags$p(tags$strong("Retest guidance: "), v$retest_level)
    )
  })

  output$pd_plot <- renderPlot({
    validate(
      need(length(current_validation()$errors) == 0, "Fix the blocking issues to view the power-duration profile.")
    )

    profile_df <- current_profile() %>%
      mutate(label_y = power_w + if_else(duration_sec == 45, 0, 8))
    model_fit <- current_model_fit()
    curve_df <- model_fit$curve

    riegel_s <- as.numeric(unname(model_fit$a))
    riegel_e <- as.numeric(unname(model_fit$b + 1))

    annotation_label <- paste0(
      "S: ", round(riegel_s, 0), " W (overall level)\n",
      "E: ", round(riegel_e, 3), " (endurance / drop-off)"
    )

    ggplot() +
      geom_line(
        data = curve_df,
        aes(x = duration_sec, y = pred_power_w),
        linewidth = 1
      ) +
      geom_point(
        data = profile_df,
        aes(x = duration_sec, y = power_w),
        size = 3
      ) +
      geom_text(
        data = profile_df,
        aes(x = duration_sec, y = label_y, label = paste0(round(power_w, 0), " W")),
        vjust = -0.7,
        size = 4
      ) +
      annotate(
        "text",
        x = max(curve_df$duration_sec),
        y = max(curve_df$pred_power_w),
        label = annotation_label,
        hjust = 1.05,
        vjust = 1.2,
        size = 4,
        alpha = 0.8
      ) +
      scale_x_log10(
        breaks = c(45, 180, 360, 600, 1200, 1800, 3600),
        labels = c("45 s", "3 min", "6 min", "10 min", "20 min", "30 min", "60 min")
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.08)),
        labels = label_number()
      ) +
      labs(
        title = "Power-Duration Profile",
        subtitle = "Measured test points with a descriptive fitted curve",
        x = "Duration",
        y = "Average power (W)",
        caption = "The curve is a descriptive fit for practical use. It is not a direct physiological measurement."
      ) +
      theme_minimal(base_size = 13)
  })

  output$summary_table <- renderTable({
    validate(
      need(length(current_validation()$errors) == 0, "Fix the blocking issues to view the summary.")
    )
    build_summary_table(
      current_inputs(),
      current_model_fit(),
      current_secondary_metrics(),
      current_validation()
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$prediction_table <- renderTable({
    validate(
      need(length(current_validation()$errors) == 0, "Fix the blocking issues to view predicted powers.")
    )
    current_predictions()
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$custom_duration_prediction <- renderUI({
    if (length(current_validation()$errors) > 0) {
      return(tags$p("Enter a valid current test profile to view this estimate."))
    }

    duration_sec <- input$custom_duration_sec

    if (is.null(duration_sec) || !is.finite(duration_sec) || duration_sec <= 0) {
      return(tags$p("Enter a duration greater than 0 seconds."))
    }

    if (duration_sec < 45 || duration_sec > 3600) {
      return(tags$p("Enter a duration between 45 and 3600 seconds for a restrained estimate."))
    }

    predicted_power_w <- exp(
      predict(
        current_model_fit()$model,
        newdata = tibble(duration_sec = duration_sec)
      )
    )

    tags$div(
      h4(paste0(round(predicted_power_w, 0), " W")),
      tags$small(paste0("Estimated average power for ", round(duration_sec, 0), " seconds."))
    )
  })

  observeEvent(input$save_test, {
    v <- current_validation()

    if (length(v$errors) > 0) {
      showNotification("Current test was not saved. Fix the blocking issues first.", type = "error")
      return(NULL)
    }

    record <- make_profile_record(
      test_row = current_inputs(),
      model_fit = current_model_fit(),
      cp_metrics = current_secondary_metrics(),
      pred_table = current_predictions(),
      validation_result = current_validation()
    )

    updated <- bind_rows(history_data(), record) %>%
      arrange(test_date)

    history_data(updated)
    save_status_message("Test saved to session history. Download your CSV if you want to keep this data.")
    showNotification("Current test saved to session history.", type = "message")
  })

  observeEvent(input$upload_history, {
    req(input$upload_history)

    uploaded <- tryCatch(
      read_history_csv(input$upload_history$datapath),
      error = function(e) {
        showNotification(paste("Upload failed:", e$message), type = "error")
        return(NULL)
      }
    )

    req(!is.null(uploaded))
    history_data(uploaded)
    save_status_message("")
    showNotification("History uploaded successfully. Session history was replaced.", type = "message")
  })

  output$save_status <- renderText({
    save_status_message()
  })

  output$history_action_status <- renderUI({
    hist <- history_data()

    if (nrow(hist) == 0) {
      return(tags$span("No saved tests to manage yet."))
    }

    if (is.null(input$history_table_rows_selected) || length(input$history_table_rows_selected) == 0) {
      return(tags$span("Select one saved row if you want to delete it."))
    }

    selected_row <- input$history_table_rows_selected[1]
    selected_record <- hist[selected_row, ]

    tags$span(
      paste0(
        "Selected: ",
        selected_record$athlete_name,
        " | ",
        as.character(selected_record$test_date)
      )
    )
  })

  observeEvent(input$delete_selected_test, {
    hist <- history_data()

    if (nrow(hist) == 0) {
      showNotification("There are no saved tests to delete.", type = "warning")
      return(NULL)
    }

    selected <- input$history_table_rows_selected

    if (is.null(selected) || length(selected) == 0) {
      showNotification("Select one saved row before deleting.", type = "warning")
      return(NULL)
    }

    selected_row <- selected[1]
    deleted_record <- hist[selected_row, ]

    updated <- hist[-selected_row, , drop = FALSE]
    history_data(updated)

    showNotification(
      paste0(
        "Deleted saved test for ",
        deleted_record$athlete_name,
        " on ",
        as.character(deleted_record$test_date),
        "."
      ),
      type = "message"
    )
  })

  output$download_example_csv <- downloadHandler(
    filename = function() {
      "power_duration_example_upload.csv"
    },
    content = function(file) {
      write_csv(build_example_upload_csv(), file)
    }
  )

  output$download_history <- downloadHandler(
    filename = function() {
      paste0("power_duration_history_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_history_csv(history_data(), file)
    }
  )

  output$history_table <- renderDT({
    hist <- history_data()

    if (nrow(hist) == 0) {
      return(
        datatable(
          tibble(Message = "No saved history yet. Save a current test or upload a CSV."),
          options = list(dom = "t", paging = FALSE),
          rownames = FALSE,
          selection = "none"
        )
      )
    }

    datatable(
      hist %>%
        transmute(
          athlete_name,
          test_date = as.character(test_date),
          `45 s` = power_45s,
          `3 min` = power_3min,
          `6 min` = power_6min,
          `10 min` = power_10min,
          `20 min` = power_20min,
          `Estimated CP` = round(estimated_cp, 0),
          `Profile Power Level (Riegel S)` = round(riegel_S, 0),
          `Power-Duration Endurance (Riegel E)` = round(riegel_E, 3),
          `Confidence` = confidence_summary
        ),
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE,
      selection = "single"
    )
  })

  output$comparison_table <- renderTable({
    validate(
      need(length(current_validation()$errors) == 0, "Fix the blocking issues to compare the current test against history.")
    )
    build_comparison_table(current_inputs(), history_data())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$history_plot <- renderPlot({
    hist <- history_data()

    validate(
      need(nrow(hist) > 0, "No saved history yet.")
    )

    trend_df <- prepare_history_trend_data(hist)

    p <- ggplot(trend_df, aes(x = test_date, y = power_w, color = metric, group = metric))

    if (dplyr::n_distinct(trend_df$test_date) > 1) {
      p <- p + geom_line(linewidth = 0.9)
    }

    p +
      geom_point(size = 2) +
      scale_y_continuous(labels = label_number()) +
      labs(
        title = "Historical Trends",
        subtitle = "All fixed-duration powers plus estimated critical power",
        x = "Test date",
        y = "Average power (W)",
        color = "Metric"
      ) +
      theme_minimal(base_size = 13)
  })

  output$training_targets_table <- renderTable({
    validate(
      need(length(current_validation()$errors) == 0, "Fix the blocking issues to generate training targets.")
    )
    current_training_targets() %>%
      select(category, anchor, use_case, note)
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

# ------------------------------
# App launch
# ------------------------------

shinyApp(ui = ui, server = server)
