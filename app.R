
# app.R
# Power-Duration Relationship App
# Version 1.1. See CHANGELOG.md for changes and docs/DECISIONS.md for the
# reasoning behind the rules in this file.

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

# ------------------------------
# Validation thresholds (see docs/DECISIONS.md, D-005 to D-009)
# ------------------------------

# Retest levels from least to most serious. Position 1 means no issues.
retest_levels <- c(
  "No retest signal",
  "Keep, but interpret cautiously",
  "Retest suggested",
  "Retest recommended"
)

# One plain-language sentence per retest level, in the same order (D-009).
confidence_sentences <- c(
  "Usable. No data-quality issues were detected from the entered values.",
  "Usable. Minor issues were noted; interpret the profile with some caution.",
  "Usable, but one or more results look unusual. Repeating the flagged test would improve confidence.",
  "Model outputs are withheld for this testing block. Correct the entry or complete a replacement block for a normal profile."
)

max_20min_power_w <- 600                        # D-006: above this, likely not watts
outlier_ratio_limits <- c(3.0, 2.0, 2.0, 2.0)   # D-005: shorter / longer, per adjacent pair
near_flat_decline <- 0.01                       # D-007: adjacent decline at or below 1%
steep_ratio_45s_3min <- 2.0                     # D-007: 45 s / 3 min above this (up to 3.0)
steep_decline_limits <- c(NA, 0.25, 0.20, 0.20) # D-007: 3-6, 6-10, 10-20 min declines
curve_shape_limit <- 0.04                       # D-007: interior points vs quadratic fit
history_mismatch_limit <- 0.08                  # D-007: any duration vs baseline record

# Context flags and their retest levels (D-008):
# 1 = Keep, but interpret cautiously; 2 = Retest suggested; 3 = Retest recommended.
context_flag_levels <- c(
  "Interrupted or failed effort" = 3,
  "Illness" = 2,
  "Hard training between tests" = 2,
  "Pacing problems" = 2,
  "Equipment or setup issues" = 2,
  "Poor sleep" = 1,
  "Unusual soreness" = 1,
  "Disrupted warm-up" = 1
)

# Equipment dropdown options (D-014). "Not specified" is stored as blank.
equipment_options <- c(
  "Not specified",
  "Rogue Echo Bike v1",
  "Rogue Echo Bike v2",
  "Rogue Echo Bike v3",
  "Rogue Echo Rower",
  "AssaultBike Classic",
  "AssaultBike Pro X",
  "AssaultBike Elite",
  "AssaultRower",
  "Concept2 Rower",
  "Concept2 SkiErg",
  "Concept2 BikeErg",
  "Wattbike Atom",
  "Wattbike AtomX",
  "Wattbike Proton",
  "Wattbike AirPro",
  "Wattbike Nucleus",
  "Other"
)

history_required_cols <- c(
  "athlete_name", "test_date", "power_45s", "power_3min", "power_6min",
  "power_10min", "power_20min"
)

history_optional_cols <- c(
  "notes", "ergometer_type", "protocol_confirmed", "test_validity_flag", "context_flags",
  "estimated_cp", "estimated_w_prime", "power_law_a", "power_law_b",
  "predicted_power_1min", "predicted_power_2min", "predicted_power_4min",
  "predicted_power_8min", "predicted_power_12min", "predicted_power_15min",
  "predicted_power_30min", "predicted_power_60min", "warning_flags",
  "confidence_summary", "retest_level"
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
    context_flags = character(),
    estimated_cp = numeric(),
    estimated_w_prime = numeric(),
    power_law_a = numeric(),
    power_law_b = numeric(),
    predicted_power_1min = numeric(),
    predicted_power_2min = numeric(),
    predicted_power_4min = numeric(),
    predicted_power_8min = numeric(),
    predicted_power_12min = numeric(),
    predicted_power_15min = numeric(),
    predicted_power_30min = numeric(),
    predicted_power_60min = numeric(),
    warning_flags = character(),
    confidence_summary = character(),
    retest_level = character()
  )
}

# The example upload file shows every column a user can edit (D-012).
build_example_upload_csv <- function() {
  tibble(
    athlete_name = "Sample Athlete",
    test_date = as.Date("2026-03-02"),
    power_45s = 620,
    power_3min = 410,
    power_6min = 360,
    power_10min = 330,
    power_20min = 295,
    ergometer_type = "Concept2 BikeErg",
    protocol_confirmed = TRUE,
    test_validity_flag = "Complete",
    context_flags = "Poor sleep",
    notes = "Example row. Replace with your own values. Separate multiple context flags with a semicolon."
  )
}

build_test_dataframe <- function(input_list) {
  tibble(
    athlete_name = trimws(as.character(input_list$athlete_name)),
    # An emptied date field arrives as a zero-length value. Convert it to NA
    # so the row stays intact and validation can report the missing date.
    test_date = if (length(input_list$test_date) == 0) as.Date(NA) else as.Date(input_list$test_date),
    power_45s = suppressWarnings(as.numeric(input_list$power_45s)),
    power_3min = suppressWarnings(as.numeric(input_list$power_3min)),
    power_6min = suppressWarnings(as.numeric(input_list$power_6min)),
    power_10min = suppressWarnings(as.numeric(input_list$power_10min)),
    power_20min = suppressWarnings(as.numeric(input_list$power_20min)),
    notes = trimws(as.character(input_list$notes)),
    # "Not specified" in the dropdown is stored as blank (D-014).
    ergometer_type = if (identical(input_list$ergometer_type, "Not specified")) "" else trimws(as.character(input_list$ergometer_type)),
    protocol_confirmed = isTRUE(input_list$protocol_confirmed),
    test_validity_flag = as.character(input_list$test_validity_flag),
    # Checked context flags, stored as one field separated by "; " (D-008).
    context_flags = paste(input_list$context_flags, collapse = "; ")
  )
}

# The unique profile key is the athlete name plus the block start date (D-013).
# Names are trimmed, internal spaces collapsed, and lowercased, so
# "Sam Smith" and "sam  smith" on the same date are the same profile.
normalize_athlete_name <- function(name) {
  tolower(gsub("\\s+", " ", trimws(as.character(name))))
}

make_profile_key <- function(athlete_name, test_date) {
  paste(normalize_athlete_name(athlete_name), as.character(as.Date(test_date)), sep = "|")
}


# Equipment is compared case-insensitively, and blank or missing means
# "not specified". Two tests are comparable when they name the same
# equipment or both leave it unspecified. "Other" is never comparable,
# since two "Other" machines may differ (D-010).
normalize_equipment <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x
}

equipment_comparable <- function(a, b) {
  a <- normalize_equipment(a)
  b <- normalize_equipment(b)
  a == b & a != "other"
}

describe_equipment <- function(x) {
  if (normalize_equipment(x) == "") "unspecified equipment" else as.character(x)
}


# Match stored equipment text to a dropdown option, ignoring case (D-012).
# Blank means not specified; anything unrecognized becomes "Other".
match_equipment <- function(x) {
  key <- normalize_equipment(x)
  if (key == "") return("Not specified")
  hit <- equipment_options[tolower(equipment_options) == key]
  if (length(hit) == 1) hit else "Other"
}

# Context flags are stored as one text field separated by "; " (D-008).
# Split it back into individual flag names.
split_context_flags <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") return(character())
  parts <- trimws(strsplit(as.character(x), ";")[[1]])
  parts[parts != ""]
}


# Parsers for uploaded CSV values (D-012). Each returns a clean value, or NA
# (NULL for context flags) when a value isn't recognized, so the row can be
# reported by number.

# Dates are accepted as 2026-03-02 or 3/2/2026, since spreadsheet apps often
# rewrite dates in the second form when saving a CSV.
parse_upload_date <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(as.Date(NA))
  if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", x)) return(as.Date(substr(x, 1, 10), format = "%Y-%m-%d"))
  if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", x)) return(as.Date(x, format = "%m/%d/%Y"))
  if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", x)) return(as.Date(x, format = "%m/%d/%y"))
  as.Date(NA)
}

parse_protocol_confirmed <- function(x) {
  x <- tolower(trimws(as.character(x)))
  if (is.na(x) || x == "") return(FALSE)
  if (x %in% c("true", "yes", "1")) return(TRUE)
  if (x %in% c("false", "no", "0")) return(FALSE)
  NA
}

parse_validity_flag <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return("Complete")
  allowed <- c("Complete", "Compromised", "Retest needed")
  hit <- allowed[tolower(allowed) == tolower(x)]
  if (length(hit) == 1) hit else NA_character_
}

parse_context_flags <- function(x) {
  parts <- split_context_flags(x)
  known <- names(context_flag_levels)
  matched <- known[match(tolower(parts), tolower(known))]
  if (any(is.na(matched))) NULL else matched
}

# The baseline for history checks and comparisons is the most recent earlier
# test for the same athlete that produced a normal profile and used
# comparable equipment (D-010). When the most recent earlier normal test used
# different equipment, the returned note says so instead of comparing wattage.
find_baseline <- function(test_row, history_df) {
  none <- list(baseline = NULL, note = NULL)

  if (is.null(history_df) || nrow(history_df) == 0 || is.na(test_row$test_date)) {
    return(none)
  }

  earlier_normal <- history_df %>%
    filter(
      normalize_athlete_name(athlete_name) == normalize_athlete_name(test_row$athlete_name),
      test_date < test_row$test_date,
      !(retest_level %in% "Retest recommended")
    ) %>%
    arrange(desc(test_date))

  if (nrow(earlier_normal) == 0) {
    return(none)
  }

  comparable <- equipment_comparable(earlier_normal$ergometer_type, test_row$ergometer_type)

  note <- NULL
  if (!comparable[1]) {
    note <- paste0(
      "Your most recent earlier test (", format(earlier_normal$test_date[1]), ") was on ",
      describe_equipment(earlier_normal$ergometer_type[1]), ", and this one is on ",
      describe_equipment(test_row$ergometer_type),
      ". Results on equipment that can't be confirmed as the same aren't compared by wattage."
    )
  }

  list(
    baseline = if (any(comparable)) earlier_normal[which(comparable)[1], ] else NULL,
    note = note
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

# Check a test entry in 3 layers (see docs/DECISIONS.md, D-005 to D-010):
# 1. Hard input errors block calculation and saving.
# 2. Ordering failures, suspected entry errors, and sessions the user marked
#    Compromised or Retest needed withhold model outputs. Raw values can be
#    saved as a compromised record.
# 3. Soft warnings allow outputs but raise the retest level.
validate_test_values <- function(test_row, history_df = NULL) {
  watts <- c(
    test_row$power_45s, test_row$power_3min, test_row$power_6min,
    test_row$power_10min, test_row$power_20min
  )
  duration_names <- names(test_durations)

  errors <- character()
  withheld_reasons <- character()
  warnings <- character()
  warning_levels <- numeric()
  notes <- character()
  flags <- character()

  # Record a soft warning with its retest level:
  # 1 = Keep, but interpret cautiously; 2 = Retest suggested.
  add_warning <- function(message, level, flag) {
    warnings <<- c(warnings, message)
    warning_levels <<- c(warning_levels, level)
    flags <<- c(flags, flag)
  }

  # Layer 1: hard input errors (D-006).
  if (is.na(test_row$athlete_name) || test_row$athlete_name == "") {
    errors <- c(errors, "Athlete name is required.")
  }

  if (is.na(test_row$test_date)) {
    errors <- c(errors, "Testing block start date is required.")
  }

  if (any(is.na(watts) | !is.finite(watts))) {
    errors <- c(errors, "All 5 watt values are required and must be numbers.")
  } else if (any(watts <= 0)) {
    errors <- c(errors, "All watt values must be greater than zero.")
  } else if (watts[5] > max_20min_power_w) {
    errors <- c(errors, paste0(
      "The 20 min value (", round(watts[5]), " W) is above ", max_20min_power_w,
      " W, beyond world-class 20-minute power. Check that you entered average watts, not calories or pace."
    ))
  }

  # Layer 2: ordering failures and suspected entry errors (D-005, D-017).
  # Checked pair by pair, so each message names the 2 values involved.
  if (length(errors) == 0) {
    for (i in 1:4) {
      shorter <- watts[i]
      longer <- watts[i + 1]

      if (shorter <= longer) {
        withheld_reasons <- c(withheld_reasons, paste0(
          "The ", duration_names[i], " value (", round(shorter), " W) should be higher than the ",
          duration_names[i + 1], " value (", round(longer), " W). Correct the entry, or retest the ",
          duration_names[i], " or ", duration_names[i + 1], " effort."
        ))
        flags <- c(flags, "order_violation")
      } else if (shorter / longer > outlier_ratio_limits[i]) {
        withheld_reasons <- c(withheld_reasons, paste0(
          "The ", duration_names[i], " value (", round(shorter), " W) is more than ",
          outlier_ratio_limits[i], " times the ", duration_names[i + 1], " value (",
          round(longer), " W). One of these is likely an entry error."
        ))
        flags <- c(flags, "suspected_entry_error")
      }
    }
  }

  has_pattern_block <- length(withheld_reasons) > 0

  # A session the user marked Compromised or Retest needed keeps its raw
  # values but gets no model outputs (D-009).
  if (test_row$test_validity_flag %in% c("Compromised", "Retest needed")) {
    withheld_reasons <- c(withheld_reasons, paste0(
      "You marked this session \"", test_row$test_validity_flag,
      "\", so model outputs are withheld. A replacement testing block is needed for a normal profile."
    ))
    flags <- c(flags, "user_marked")
  }

  # Context flags (D-008). An interrupted or failed effort withholds outputs,
  # like a compromised session. The other flags raise the retest level below.
  reported_flags <- split_context_flags(test_row$context_flags)

  if ("Interrupted or failed effort" %in% reported_flags) {
    withheld_reasons <- c(
      withheld_reasons,
      "You reported an interrupted or failed effort, so model outputs are withheld. If you repeated that effort and entered the new result, uncheck this flag."
    )
    flags <- c(flags, "interrupted_effort")
  }

  # Layer 3: soft warnings (D-007), only for profiles that will be modeled.
  if (length(errors) == 0 && length(withheld_reasons) == 0) {
    declines <- 1 - watts[2:5] / watts[1:4]

    for (i in 1:4) {
      if (declines[i] <= near_flat_decline) {
        add_warning(paste0(
          "Your ", duration_names[i], " and ", duration_names[i + 1], " results are within ",
          near_flat_decline * 100, "% of each other. Check the entries, and whether both efforts were maximal."
        ), 1, "near_flat")
      }
    }

    ratio_45s_3min <- watts[1] / watts[2]
    if (ratio_45s_3min > steep_ratio_45s_3min) {
      add_warning(paste0(
        "Your 45 s power is ", round(ratio_45s_3min, 1), " times your 3 min power, steeper than most profiles. ",
        "Check the entry. If it matches your earlier tests, it likely reflects your profile."
      ), 1, "steep_45s_3min")
    }

    for (i in 2:4) {
      if (declines[i] > steep_decline_limits[i]) {
        add_warning(paste0(
          "Power drops ", round(declines[i] * 100), "% from ", duration_names[i], " to ",
          duration_names[i + 1], ", above the app's ", steep_decline_limits[i] * 100,
          "% check. This can reflect pacing, fatigue, or an entry error."
        ), 2, "steep_decline")
      }
    }

    # Curve shape: compare the 3 interior results with a smooth quadratic
    # curve (in log-log space) fitted to all 5. This is a quality check only;
    # the displayed curve still comes from the power-law fit.
    log_duration <- log(unname(test_durations))
    shape_fit <- lm(log(watts) ~ log_duration + I(log_duration^2))
    shape_gap <- exp(residuals(shape_fit)) - 1
    for (i in 2:4) {
      if (abs(shape_gap[i]) > curve_shape_limit) {
        add_warning(paste0(
          "Your ", duration_names[i], " result is ", round(abs(shape_gap[i]) * 100), "% ",
          if (shape_gap[i] > 0) "above" else "below",
          " the smooth curve fitted to your 5 results. Check the entry, or consider repeating that test."
        ), 2, "curve_shape")
      }
    }

    if (!isTRUE(test_row$protocol_confirmed)) {
      add_warning(
        "Protocol adherence isn't confirmed. Interpret results more cautiously if the schedule or standardization rules weren't followed.",
        1, "protocol_unconfirmed"
      )
    }

    for (flag_name in intersect(reported_flags, names(context_flag_levels))) {
      add_warning(
        paste0("You reported \"", flag_name, "\" for this testing block. It may have affected your results."),
        context_flag_levels[[flag_name]], "context_flag"
      )
    }

    baseline_info <- find_baseline(test_row, history_df)
    if (!is.null(baseline_info$note)) {
      notes <- c(notes, baseline_info$note)
    }
    baseline <- baseline_info$baseline
    if (!is.null(baseline)) {
      baseline_watts <- c(
        baseline$power_45s, baseline$power_3min, baseline$power_6min,
        baseline$power_10min, baseline$power_20min
      )
      if (any(abs(watts / baseline_watts - 1) > history_mismatch_limit, na.rm = TRUE)) {
        add_warning(paste0(
          "At least one result differs by more than ", history_mismatch_limit * 100,
          "% from your previous comparable test (", format(baseline$test_date),
          "). This may reflect real change, fatigue, or a testing inconsistency."
        ), 2, "history_mismatch")
      }
    }
  }

  outputs_withheld <- length(errors) > 0 || length(withheld_reasons) > 0
  level <- if (outputs_withheld) 3 else max(c(0, warning_levels))

  list(
    errors = errors,
    withheld_reasons = withheld_reasons,
    has_pattern_block = has_pattern_block,
    warnings = warnings,
    notes = notes,
    flags = unique(flags),
    outputs_withheld = outputs_withheld,
    retest_level = retest_levels[level + 1],
    confidence_summary = confidence_sentences[level + 1]
  )
}

fit_power_duration_model <- function(profile_long) {
  fit <- lm(log(power_w) ~ log(duration_sec), data = profile_long)

  # Include the 5 test durations in the grid so the curve passes exactly
  # through 20 min, where the plot switches to the extrapolated style.
  curve_grid <- tibble(
    duration_sec = sort(unique(c(
      exp(seq(log(45), log(3600), length.out = 300)),
      unname(test_durations)
    )))
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

  # CP and W′ are reported as a pair. If either is non-finite or not
  # positive, both are unavailable rather than showing a misleading half
  # (Resolved spec §1).
  if (!is.finite(cp_w) || cp_w <= 0 || !is.finite(wprime_j) || wprime_j <= 0) {
    return(list(cp_w = NA_real_, wprime_j = NA_real_))
  }

  list(cp_w = cp_w, wprime_j = wprime_j)
}

build_summary_table <- function(test_row, model_fit, cp_metrics, validation_result) {
  tibble(
    metric = c(
      "Athlete",
      "Block start date",
      "45 s power",
      "3 min power",
      "6 min power",
      "10 min power",
      "20 min power",
      "Estimated critical power",
      "Estimated W′",
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
      validation_result$confidence_summary,
      validation_result$retest_level
    )
  )
}

build_prediction_table <- function(model_fit) {
  predict_power_curve(model_fit, prediction_durations) %>%
    transmute(
      duration = duration_label,
      predicted_power_w = round(predicted_power_w, 0),
      # Durations past the 20 min test are extrapolations, not interpolations.
      estimate_type = if_else(
        duration_sec > max(test_durations),
        "Extrapolated estimate",
        "Modeled estimate"
      )
    )
}

build_training_targets <- function(pred_table, cp_metrics) {
  # Look up a predicted power by its duration label, e.g. "4 min".
  lookup <- setNames(pred_table$predicted_power_w, pred_table$duration)

  # Show each anchor as its own value, e.g. "4 min: 352 W". Combining two
  # anchors into one range would hide that they refer to different durations.
  # Anchors beyond the 20 min test are marked as extrapolated.
  format_anchor <- function(label) {
    is_extrapolated <- prediction_durations[[label]] > max(test_durations)
    paste0(
      label, ": ", lookup[[label]], " W",
      if (is_extrapolated) " (extrapolated)" else ""
    )
  }

  tibble(
    category = c(
      "Short-power repeats",
      "Anaerobic-capacity intervals",
      "VO2-type intervals",
      "Threshold-focused work",
      "Steady aerobic work"
    ),
    anchor_1 = c(
      format_anchor("1 min"),
      format_anchor("2 min"),
      format_anchor("4 min"),
      format_anchor("12 min"),
      format_anchor("30 min")
    ),
    anchor_2 = c(
      format_anchor("2 min"),
      format_anchor("4 min"),
      format_anchor("8 min"),
      format_anchor("15 min"),
      format_anchor("60 min")
    ),
    use_case = c(
      "Hard short repeats with long recovery",
      "High-cost work where repeatability matters",
      "Intervals that stress hard aerobic power",
      "Sustained hard work anchored to longer efforts",
      "Longer steady sessions and controlled aerobic work"
    ),
    note = c(
      "A starting reference. Adjust to what you can repeat across the session.",
      "Highly fatiguing and pacing-sensitive. Adjust to what you can repeat.",
      "Choose a power you can hold on every repeat.",
      "An estimated reference for sustained work. It does not identify a physiological threshold.",
      "Both values extend beyond your 20 min test. Treat them as lower-confidence planning references."
    ),
    cp_context = ifelse(
      is.na(cp_metrics$cp_w),
      "Secondary estimated CP not available",
      paste0("Secondary estimated CP: ", round(cp_metrics$cp_w, 0), " W")
    )
  )
}

# Build one saved-history record. Raw values are always kept. When model
# outputs are withheld (D-009), the model arguments are left out and every
# model-derived field is empty (NA).
make_profile_record <- function(test_row, validation_result, model_fit = NULL,
                                cp_metrics = NULL, pred_table = NULL) {
  predicted_at <- function(label) {
    if (is.null(pred_table)) NA_real_ else pred_table$predicted_power_w[pred_table$duration == label]
  }

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
    context_flags = test_row$context_flags,
    estimated_cp = if (is.null(cp_metrics)) NA_real_ else cp_metrics$cp_w,
    estimated_w_prime = if (is.null(cp_metrics)) NA_real_ else cp_metrics$wprime_j,
    power_law_a = if (is.null(model_fit)) NA_real_ else model_fit$a,
    power_law_b = if (is.null(model_fit)) NA_real_ else model_fit$b,
    predicted_power_1min = predicted_at("1 min"),
    predicted_power_2min = predicted_at("2 min"),
    predicted_power_4min = predicted_at("4 min"),
    predicted_power_8min = predicted_at("8 min"),
    predicted_power_12min = predicted_at("12 min"),
    predicted_power_15min = predicted_at("15 min"),
    predicted_power_30min = predicted_at("30 min"),
    predicted_power_60min = predicted_at("60 min"),
    warning_flags = paste(validation_result$flags, collapse = "; "),
    confidence_summary = validation_result$confidence_summary,
    retest_level = validation_result$retest_level
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
    protocol_confirmed = if ("protocol_confirmed" %in% names(row)) as.logical(row$protocol_confirmed) else FALSE,
    test_validity_flag = if ("test_validity_flag" %in% names(row)) as.character(row$test_validity_flag) else "Complete",
    context_flags = if ("context_flags" %in% names(row) && !is.na(row$context_flags)) as.character(row$context_flags) else ""
  )

  validation_result <- validate_test_values(test_row, history_df)

  # Withheld outputs: keep the raw values only, with no fitted model (D-009).
  if (isTRUE(validation_result$outputs_withheld)) {
    return(make_profile_record(test_row = test_row, validation_result = validation_result))
  }

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

# Save a record to history. Any record with the same profile key is removed
# first, so saving never creates a duplicate. Every record is then
# recomputed in date order, because a change to one record can change the
# history checks on later ones (D-011).
upsert_history_record <- function(history_df, record) {
  record_key <- make_profile_key(record$athlete_name, record$test_date)
  existing_keys <- make_profile_key(history_df$athlete_name, history_df$test_date)
  kept <- history_df[existing_keys != record_key, , drop = FALSE]
  recompute_uploaded_history(bind_rows(kept, record))
}

# Read an uploaded history CSV. Every column is read as text first, so each
# value can be checked and reported by row number before anything is used.
read_history_csv <- function(path) {
  raw <- read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
  validate_history_file(raw)
}

# Validate an uploaded history file (D-012). Returns the recomputed history
# and the number of rows imported as compromised. Any problem stops the
# upload with a message naming the rows, and existing history is unchanged.
validate_history_file <- function(df) {
  missing_required <- setdiff(history_required_cols, names(df))
  if (length(missing_required) > 0) {
    stop(paste0("The file is missing required columns: ", paste(missing_required, collapse = ", "), "."))
  }

  if (nrow(df) == 0) {
    stop("The file has no test rows.")
  }

  # Optional columns may be absent; treat them as blank.
  optional_text <- function(col) {
    if (col %in% names(df)) df[[col]] else rep(NA_character_, nrow(df))
  }

  problems <- character()
  rows <- vector("list", nrow(df))
  keys <- rep(NA_character_, nrow(df))
  n_compromised <- 0

  for (i in seq_len(nrow(df))) {
    row_problems <- character()

    raw_date <- df$test_date[i]
    test_date <- parse_upload_date(raw_date)
    date_unreadable <- !is.na(raw_date) && trimws(raw_date) != "" && is.na(test_date)
    if (date_unreadable) {
      row_problems <- c(row_problems, paste0("test_date \"", raw_date, "\" isn't a date like 2026-03-02 or 3/2/2026"))
    }

    protocol <- parse_protocol_confirmed(optional_text("protocol_confirmed")[i])
    validity <- parse_validity_flag(optional_text("test_validity_flag")[i])
    flags <- parse_context_flags(optional_text("context_flags")[i])

    if (is.na(protocol)) row_problems <- c(row_problems, "protocol_confirmed must be TRUE/FALSE, yes/no, or 1/0")
    if (is.na(validity)) row_problems <- c(row_problems, "test_validity_flag must be Complete, Compromised, or Retest needed")
    if (is.null(flags)) row_problems <- c(row_problems, "context_flags contains an unrecognized flag")

    equipment <- match_equipment(optional_text("ergometer_type")[i])
    notes <- optional_text("notes")[i]

    test_row <- tibble(
      athlete_name = trimws(df$athlete_name[i]),
      test_date = test_date,
      power_45s = suppressWarnings(as.numeric(df$power_45s[i])),
      power_3min = suppressWarnings(as.numeric(df$power_3min[i])),
      power_6min = suppressWarnings(as.numeric(df$power_6min[i])),
      power_10min = suppressWarnings(as.numeric(df$power_10min[i])),
      power_20min = suppressWarnings(as.numeric(df$power_20min[i])),
      notes = if (is.na(notes)) "" else notes,
      ergometer_type = if (equipment == "Not specified") "" else equipment,
      protocol_confirmed = isTRUE(protocol),
      test_validity_flag = if (is.na(validity)) "Complete" else validity,
      context_flags = if (is.null(flags)) "" else paste(flags, collapse = "; ")
    )

    # Hard input errors reject the file (D-006). The validator's messages end
    # with periods, which are dropped so they join cleanly with "; ".
    check <- validate_test_values(test_row)
    input_errors <- check$errors
    if (date_unreadable) {
      input_errors <- input_errors[input_errors != "Testing block start date is required."]
    }
    row_problems <- c(row_problems, sub("\\.$", "", input_errors))

    if (!is.na(test_row$athlete_name) && test_row$athlete_name != "" && !is.na(test_row$test_date)) {
      keys[i] <- make_profile_key(test_row$athlete_name, test_row$test_date)
    }

    # Spreadsheet row numbers: row 1 is the header, so data starts at row 2.
    if (length(row_problems) > 0) {
      problems <- c(problems, paste0("Row ", i + 1, ": ", paste(row_problems, collapse = "; "), "."))
      next
    }

    # Ordering failures and likely entry errors import as compromised (D-012).
    if (check$has_pattern_block && test_row$test_validity_flag == "Complete") {
      test_row$test_validity_flag <- "Compromised"
      n_compromised <- n_compromised + 1
    }

    rows[[i]] <- test_row
  }

  for (k in unique(keys[!is.na(keys) & duplicated(keys)])) {
    problems <- c(problems, paste0(
      "Rows ", paste(which(keys == k) + 1, collapse = " and "),
      ": same athlete and start date. Each athlete and start date can appear only once."
    ))
  }

  if (length(problems) > 0) {
    shown <- head(problems, 5)
    more <- length(problems) - length(shown)
    stop(paste0(
      "The file wasn't uploaded, and your current history is unchanged. ",
      paste(shown, collapse = " | "),
      if (more > 0) paste0(" | ...and ", more, " more.") else ""
    ))
  }

  list(
    history = recompute_uploaded_history(bind_rows(rows)),
    n_compromised = n_compromised
  )
}

# Empty values are written as blank cells, which read cleanly in a
# spreadsheet and come back as empty on upload.
write_history_csv <- function(history_df, file) {
  write_csv(history_df, file, na = "")
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

# Compare the current entry with its baseline: the most recent earlier test
# for the same athlete with a normal profile on comparable equipment (D-010).
build_comparison_table <- function(current_row, history_df) {
  baseline_info <- find_baseline(current_row, history_df)
  prior <- baseline_info$baseline

  if (is.null(prior)) {
    return(tibble(Message = paste(
      c("No earlier comparable saved test exists yet for this athlete.", baseline_info$note),
      collapse = " "
    )))
  }

  current_vals <- c(current_row$power_45s, current_row$power_3min, current_row$power_6min, current_row$power_10min, current_row$power_20min)
  prior_vals <- c(prior$power_45s, prior$power_3min, prior$power_6min, prior$power_10min, prior$power_20min)

  comparison <- tibble(
    Duration = names(test_durations),
    Current = round(current_vals),
    Previous = round(prior_vals),
    `Change (W)` = round(current_vals - prior_vals),
    `Change (%)` = sprintf("%+.1f%%", 100 * (current_vals / prior_vals - 1))
  )

  # Name the previous column by its start date so the comparison is explicit.
  names(comparison)[3] <- paste0("Previous (", format(prior$test_date), ")")
  comparison
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
    "<p>Complete 5 maximal efforts over about 11 days, shortest to longest. The weekdays below are a suggested cadence. If your schedule differs, keep similar spacing: the first 3 tests 2 days apart, then 3 days before each of the last 2.</p>",
    "<p>The day of your 45-second test is your testing block start date. Enter it on the Current Test Entry tab.</p>",
    "<table class='table table-bordered'>",
    "<thead><tr><th>Week</th><th>Suggested day</th><th>Session</th></tr></thead>",
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
    "</ul>",
    "<h4>Recording and Saving Your Results</h4>",
    "<ol>",
    "<li>Write down the average watts at the end of each test. Use the watts reading, not calories or pace.</li>",
    "<li>After the final test, enter all 5 results on the Current Test Entry tab.</li>",
    "<li>Review any warnings, then save the test to history.</li>",
    "<li>Download your history CSV from the Import / Export tab. The app does not keep data after you close it.</li>",
    "<li>Next time, upload that file before entering your new block, so comparisons and history checks work.</li>",
    "</ol>",
    "<h4>What the Results Mean</h4>",
    "<p>Your profile is an estimate based on your field tests. The app does not directly measure lactate threshold, ventilatory thresholds, VO2max, or metabolic zones.</p>"
  )
)

# ------------------------------
# UI
# ------------------------------

ui <- navbarPage(
  title = "Power-Duration App",
  id = "main_tabs",
  # On narrow screens, tabs collapse into a menu button instead of stacking.
  collapsible = TRUE,

  tabPanel(
    "Current Test Entry",
    sidebarLayout(
      sidebarPanel(
        h4("Enter Current Test"),
        p("New user? Read the Testing Protocol tab before your first testing block."),
        p("After the block, enter all 5 results here. The profile updates as you type. Save it to add it to this session's history."),
        textInput("athlete_name", "Athlete name"),
        dateInput("test_date", "Testing block start date", value = Sys.Date()),
        helpText("The day you did the 45-second test."),
        selectInput("ergometer_type", "Ergometer", choices = equipment_options, selected = "Not specified"),
        checkboxInput("protocol_confirmed", "I followed the testing protocol closely", value = FALSE),
        checkboxGroupInput(
          "context_flags",
          "Did anything affect this testing block? (optional)",
          choices = names(context_flag_levels)
        ),
        textAreaInput("notes", "Notes (optional)", rows = 3, placeholder = "Anything else worth recording about this block"),
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
        uiOutput("completion_status"),
        uiOutput("validation_box"),
        plotOutput("pd_plot", height = "430px"),
        tags$small("Points = your tested results. Solid line = fitted estimate within your tested range. Dashed line = extrapolated estimate beyond 20 min."),
        tags$br(),
        tags$small("A steeper drop from left to right means power falls off faster as duration increases."),
        tags$br(),
        tags$small("Best comparisons come from the same ergometer, setup, and test conditions."),
        br(),
        fluidRow(
          column(
            6,
            h4("Current Summary"),
            tableOutput("summary_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "Historical Comparison",
    fluidPage(
      h3("Saved Test History"),
      p("Select a saved row to load it into the entry form for editing, or to delete it."),
      DTOutput("history_table"),
      br(),
      fluidRow(
        column(
          4,
          actionButton("load_selected_test", "Load Selected Test for Editing"),
          actionButton("delete_selected_test", "Delete Selected Test")
        ),
        column(8, uiOutput("history_action_status"))
      ),
      br(),
      h3("Current vs Previous Comparable Test"),
      tags$small("The comparison uses your most recent earlier test with a normal profile on the same equipment."),
      tags$br(),
      tags$small("Small changes may reflect normal test variation. Compare tests done on the same ergometer and under similar conditions."),
      tableOutput("comparison_table"),
      h4("Profile shape: current vs previous comparable test"),
      plotOutput("overlay_plot", height = "380px"),
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
      tags$small("Each category shows 2 anchor durations with an estimated power for each. Treat them as practical starting references derived from your profile."),
      tags$br(),
      tags$small("Use repeatability and workout response to adjust them."),
      tableOutput("training_targets_table"),
      h4("Predicted power by duration"),
      tags$small("Values from 1 to 15 min are modeled from your 5 tests. The 30 and 60 min values extend beyond your longest test and carry more uncertainty."),
      tableOutput("prediction_table")
    )
  ),

  tabPanel(
    "Testing Protocol",
    fluidPage(
      HTML(protocol_html)
    )
  ),
  
  tabPanel(
    "Import / Export",
    fluidPage(
      h3("CSV Import and Export"),
      p(strong("This app does not store your data between sessions.")),
      p("To track progress over time, download your history before closing, then upload it when you return. Uploading replaces the current session history."),
      h4("Columns"),
      p("Required: athlete_name, test_date (your testing block start date), power_45s, power_3min, power_6min, power_10min, power_20min."),
      p("Optional: ergometer_type, protocol_confirmed (TRUE or FALSE), test_validity_flag (Complete, Compromised, or Retest needed), context_flags (separate several with a semicolon), and notes. Equipment names that don't match the app's list are saved as Other."),
      p("Dates can be written as 2026-03-02 or 3/2/2026. Calculated columns in a downloaded file are ignored and recalculated on upload."),
      h4("What the upload checks"),
      tags$ul(
        tags$li("Each athlete and start date can appear only once."),
        tags$li("A row with a missing or invalid value stops the upload, and the message names the row. Your current history stays as it was."),
        tags$li("A row whose values break the expected order, or look like an entry error, is imported as a compromised record.")
      ),
      helpText("If you're unsure of the format, start from the Example CSV."),
      fluidRow(
        column(
          width = 6,
          fileInput("upload_history", "Upload history CSV", accept = ".csv")
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
      test_validity_flag = input$test_validity_flag,
      context_flags = input$context_flags
    ))
  })

  current_validation <- reactive({
    validate_test_values(current_inputs(), history_data())
  })

  validated_test <- reactive({
    req(!current_validation()$outputs_withheld)
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

  # Completion status: how many of the 5 results are entered, and what the
  # user should do next. Shown at the top of Current Test Entry.
  output$completion_status <- renderUI({
    test_row <- current_inputs()
    watts <- c(
      test_row$power_45s, test_row$power_3min, test_row$power_6min,
      test_row$power_10min, test_row$power_20min
    )
    n_entered <- sum(!is.na(watts))
    has_name <- !is.na(test_row$athlete_name) && test_row$athlete_name != ""
    has_date <- !is.na(test_row$test_date)
    n_errors <- length(current_validation()$errors)

    next_action <- if (n_entered < 5 || !has_name || !has_date) {
      n_missing <- 5 - n_entered
      missing <- c(
        if (!has_name) "athlete name",
        if (!has_date) "start date",
        if (n_missing > 0) paste0(n_missing, " more test result", if (n_missing == 1) "" else "s")
      )
      paste0("Next: enter ", paste(missing, collapse = ", "), ".")
    } else if (n_errors > 0) {
      "Next: fix the blocking issues listed below."
    } else if (current_validation()$has_pattern_block) {
      "Next: correct the entry, or save the raw values as a compromised record. See the messages below."
    } else if (current_validation()$outputs_withheld) {
      "Next: save the raw values for your records. A replacement testing block is needed for a normal profile."
    } else {
      "Next: review the profile, then save it to history. Download your history from Import / Export to keep it."
    }

    tags$div(
      style = "padding:10px; background-color:#f4f6f8; border:1px solid #d8dee4; margin-bottom:15px;",
      tags$strong(paste0(n_entered, " of 5 test results entered. ")),
      next_action
    )
  })
  
  output$validation_box <- renderUI({
    v <- current_validation()

    green_style <- "padding:10px; background-color:#eef7ee; border:1px solid #cfe5cf; margin-bottom:15px;"
    amber_style <- "padding:10px; background-color:#fff4e5; border:1px solid #e5c58a; margin-bottom:15px;"

    # Nothing to report: a short green status line.
    if (length(v$errors) == 0 && length(v$withheld_reasons) == 0 &&
        length(v$warnings) == 0 && length(v$notes) == 0) {
      return(tags$div(style = green_style, tags$strong("Status: "), v$confidence_summary))
    }

    # Offer the compromised-save route only for ordering failures and
    # suspected entry errors on a session still marked Complete.
    offer_compromised_save <- v$has_pattern_block &&
      identical(current_inputs()$test_validity_flag, "Complete")

    tags$div(
      style = amber_style,
      tags$strong("Input checks"),
      if (length(v$errors) > 0) tags$div(
        tags$p(tags$strong("Blocking issues")),
        tags$ul(lapply(v$errors, tags$li))
      ),
      if (length(v$withheld_reasons) > 0) tags$div(
        tags$p(tags$strong("Model outputs withheld")),
        tags$ul(lapply(v$withheld_reasons, tags$li)),
        if (offer_compromised_save) tags$p(
          "To keep these raw values for review, set the Test validity flag to Compromised, then save."
        )
      ),
      if (length(v$warnings) > 0) tags$div(
        tags$p(tags$strong("Warnings")),
        tags$ul(lapply(v$warnings, tags$li))
      ),
      if (length(v$notes) > 0) tags$div(
        tags$p(tags$strong("Notes")),
        tags$ul(lapply(v$notes, tags$li))
      ),
      tags$p(tags$strong("Retest guidance: "), v$retest_level, ". ", v$confidence_summary)
    )
  })

  output$pd_plot <- renderPlot({
    v <- current_validation()
    validate(
      need(length(v$errors) == 0, "Fix the blocking issues to view your results.")
    )

    # Outputs withheld: plot the entered values only, with no fitted curve,
    # so the problem is visible without implying a modeled profile.
    if (v$outputs_withheld) {
      raw_df <- to_long_profile(current_inputs())
      return(
        ggplot(raw_df, aes(x = duration_sec, y = power_w)) +
          geom_point(size = 3) +
          geom_text(aes(label = paste0(round(power_w, 0), " W")), vjust = -1, size = 4) +
          scale_x_log10(breaks = unname(test_durations), labels = names(test_durations)) +
          scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)), labels = label_number()) +
          labs(
            title = "Entered Results",
            subtitle = "No fitted curve: model outputs are withheld. See the messages above.",
            x = "Duration",
            y = "Average power (W)"
          ) +
          theme_minimal(base_size = 13)
      )
    }

    profile_df <- current_profile() %>%
      mutate(label_y = power_w + if_else(duration_sec == 45, 0, 8))
    model_fit <- current_model_fit()
    curve_df <- model_fit$curve

    # The longest test is 20 min (1200 s). Beyond it, the curve is
    # extrapolation, so it is drawn dashed over a shaded band.
    longest_test_sec <- max(test_durations)
    curve_tested <- curve_df %>% filter(duration_sec <= longest_test_sec)
    curve_extrapolated <- curve_df %>% filter(duration_sec >= longest_test_sec)

    ggplot() +
      annotate(
        "rect",
        xmin = longest_test_sec, xmax = Inf,
        ymin = -Inf, ymax = Inf,
        fill = "grey92"
      ) +
      annotate(
        "text",
        x = sqrt(longest_test_sec * 3600),
        y = Inf,
        label = "Extrapolated\nbeyond 20 min",
        vjust = 1.5,
        size = 3.5,
        colour = "grey40"
      ) +
      geom_line(
        data = curve_tested,
        aes(x = duration_sec, y = pred_power_w),
        linewidth = 1
      ) +
      geom_line(
        data = curve_extrapolated,
        aes(x = duration_sec, y = pred_power_w),
        linewidth = 1,
        linetype = "dashed",
        colour = "grey45"
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
      need(!current_validation()$outputs_withheld, "Resolve the messages above to view the summary.")
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
      need(!current_validation()$outputs_withheld, "Resolve the messages above to view predicted powers.")
    )
    current_predictions() %>%
      transmute(
        Duration = duration,
        `Estimated power (W)` = predicted_power_w,
        `Estimate type` = estimate_type
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 0)

  # Holds a record while the user decides whether to replace a saved test
  # with the same athlete and start date.
  pending_record <- reactiveVal(NULL)

  observeEvent(input$save_test, {
    v <- current_validation()

    if (length(v$errors) > 0) {
      showNotification("Current test was not saved. Fix the blocking issues first.", type = "error")
      return(NULL)
    }

    # Ordering failures and suspected entry errors save only as an explicit
    # compromised record (D-006).
    if (v$has_pattern_block && identical(current_inputs()$test_validity_flag, "Complete")) {
      showNotification(
        "Not saved. Correct the entry, or set the Test validity flag to Compromised to save the raw values for review.",
        type = "error", duration = 8
      )
      return(NULL)
    }

    # Withheld outputs: save the raw values only (D-009).
    record <- if (v$outputs_withheld) {
      make_profile_record(test_row = current_inputs(), validation_result = v)
    } else {
      make_profile_record(
        test_row = current_inputs(),
        validation_result = v,
        model_fit = current_model_fit(),
        cp_metrics = current_secondary_metrics(),
        pred_table = current_predictions()
      )
    }

    record_key <- make_profile_key(record$athlete_name, record$test_date)
    existing_keys <- make_profile_key(history_data()$athlete_name, history_data()$test_date)

    # Same athlete and start date already saved: ask before replacing.
    if (record_key %in% existing_keys) {
      pending_record(record)
      showModal(modalDialog(
        title = "Replace saved test?",
        paste0(
          "A saved test already exists for ", record$athlete_name,
          " with a block start date of ", format(record$test_date),
          ". Replace it with the values currently entered?"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_replace", "Replace saved test")
        )
      ))
      return(NULL)
    }

    history_data(upsert_history_record(history_data(), record))
    save_status_message("Test saved to session history. Download your CSV if you want to keep this data.")
    showNotification("Current test saved to session history.", type = "message")
  })

  observeEvent(input$confirm_replace, {
    record <- pending_record()
    req(!is.null(record))

    history_data(upsert_history_record(history_data(), record))
    pending_record(NULL)
    removeModal()
    save_status_message("Saved test replaced. Download your CSV if you want to keep this data.")
    showNotification("Saved test replaced.", type = "message")
  })

  observeEvent(input$upload_history, {
    req(input$upload_history)

    # Error messages stay open until closed, since they can list several rows.
    uploaded <- tryCatch(
      read_history_csv(input$upload_history$datapath),
      error = function(e) {
        showNotification(paste("Upload failed:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      }
    )

    req(!is.null(uploaded))
    history_data(uploaded$history)
    save_status_message("")

    n_tests <- nrow(uploaded$history)
    message <- paste0(
      "History uploaded: ", n_tests, if (n_tests == 1) " test" else " tests",
      ". Session history was replaced."
    )
    if (uploaded$n_compromised > 0) {
      message <- paste0(
        message, " ", uploaded$n_compromised,
        " with ordering problems or likely entry errors were imported as compromised records."
      )
    }
    showNotification(message, type = "message", duration = 10)
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
      return(tags$span("Select one saved row to load or delete it."))
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

    # Load the selected saved record into the entry form for editing. Saving it
  # again under the same athlete and start date asks to replace the original.
  observeEvent(input$load_selected_test, {
    hist <- history_data()
    selected <- input$history_table_rows_selected

    if (nrow(hist) == 0 || is.null(selected) || length(selected) == 0) {
      showNotification("Select one saved row before loading.", type = "warning")
      return(NULL)
    }

    record <- hist[selected[1], ]

    # Optional text fields can be missing in uploaded files; show them as blank.
    text_or_blank <- function(x) if (is.na(x)) "" else as.character(x)

    updateTextInput(session, "athlete_name", value = record$athlete_name)
    updateDateInput(session, "test_date", value = record$test_date)
    updateSelectInput(session, "ergometer_type", selected = match_equipment(record$ergometer_type))
    updateTextAreaInput(session, "notes", value = text_or_blank(record$notes))
    updateCheckboxInput(session, "protocol_confirmed", value = isTRUE(record$protocol_confirmed))
    updateCheckboxGroupInput(session, "context_flags", selected = split_context_flags(record$context_flags))
    updateSelectInput(
      session, "test_validity_flag",
      selected = if (is.na(record$test_validity_flag)) "Complete" else record$test_validity_flag
    )
    updateNumericInput(session, "power_45s", value = record$power_45s)
    updateNumericInput(session, "power_3min", value = record$power_3min)
    updateNumericInput(session, "power_6min", value = record$power_6min)
    updateNumericInput(session, "power_10min", value = record$power_10min)
    updateNumericInput(session, "power_20min", value = record$power_20min)

    updateNavbarPage(session, "main_tabs", selected = "Current Test Entry")
    save_status_message(paste0(
      "Loaded the saved test for ", record$athlete_name,
      " (", format(record$test_date), "). Edit the values, then save to replace it. ",
      "Changing the name or start date saves a separate record instead."
    ))
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

    # Recompute the rest, since removing a record can change the history
    # checks on later ones (D-011).
    updated <- recompute_uploaded_history(hist[-selected_row, , drop = FALSE])
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
          `Block start date` = as.character(test_date),
          `45 s` = power_45s,
          `3 min` = power_3min,
          `6 min` = power_6min,
          `10 min` = power_10min,
          `20 min` = power_20min,
          `Estimated CP` = round(estimated_cp, 0),
          `Validity` = test_validity_flag,
          `Retest guidance` = retest_level
        ),
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE,
      selection = "single"
    )
  })

  output$comparison_table <- renderTable({
    validate(
      need(!current_validation()$outputs_withheld, "Resolve the messages above to compare the current test against history.")
    )
    build_comparison_table(current_inputs(), history_data())
  }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 0)

    # Profile-shape comparison (D-016): the current fitted curve and points
  # over the baseline's, within the tested range only (45 s to 20 min).
  output$overlay_plot <- renderPlot({
    validate(
      need(!current_validation()$outputs_withheld, "Enter a test that produces a normal profile to compare shapes.")
    )

    baseline <- find_baseline(current_inputs(), history_data())$baseline
    validate(
      need(!is.null(baseline), "No earlier comparable saved test exists yet for this athlete.")
    )

    baseline_label <- paste0("Previous (", format(baseline$test_date), ")")
    longest_test_sec <- max(test_durations)

    points_df <- bind_rows(
      current_profile() %>% mutate(test = "Current"),
      to_long_profile(baseline) %>% mutate(test = baseline_label)
    )

    curves_df <- bind_rows(
      current_model_fit()$curve %>% mutate(test = "Current"),
      fit_power_duration_model(to_long_profile(baseline))$curve %>% mutate(test = baseline_label)
    ) %>%
      filter(duration_sec <= longest_test_sec)

    ggplot() +
      geom_line(data = curves_df, aes(x = duration_sec, y = pred_power_w, colour = test), linewidth = 1) +
      geom_point(data = points_df, aes(x = duration_sec, y = power_w, colour = test), size = 3) +
      scale_colour_manual(values = setNames(c("black", "grey60"), c("Current", baseline_label))) +
      scale_x_log10(breaks = unname(test_durations), labels = names(test_durations)) +
      scale_y_continuous(labels = label_number()) +
      labs(
        title = "Profile Shape Comparison",
        subtitle = "Fitted curves within the tested range (45 s to 20 min)",
        x = "Duration",
        y = "Average power (W)",
        colour = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })
  
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
        x = "Testing block start date",
        y = "Average power (W)",
        color = "Metric"
      ) +
      theme_minimal(base_size = 13)
  })

  output$training_targets_table <- renderTable({
    validate(
      need(!current_validation()$outputs_withheld, "Resolve the messages above to generate training targets.")
    )
    current_training_targets() %>%
      transmute(
        Category = category,
        `First anchor` = anchor_1,
        `Second anchor` = anchor_2,
        Use = use_case,
        Note = note
      )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
}

# ------------------------------
# App launch
# ------------------------------

shinyApp(ui = ui, server = server)
