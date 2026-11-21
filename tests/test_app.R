# tests/test_app.R
# Automated checks for app.R. They cover the validation rules, model outputs,
# history baseline, saving, CSV import and export, and the server logic.
#
# Run from the project folder (the one containing app.R):
#   source("tests/test_app.R")
#
# Each check prints PASS or FAIL. Run this before every release, and add a
# check whenever a rule in docs/DECISIONS.md changes.

suppressPackageStartupMessages(library(shiny))

# Load everything in app.R except the final shinyApp() call, so the helper
# functions and server can be tested without launching the app.
app_lines <- readLines("app.R")
app_lines <- app_lines[!grepl("^shinyApp\\(", app_lines)]
suppressPackageStartupMessages(eval(parse(text = app_lines), envir = globalenv()))

results <- data.frame(check = character(), pass = logical(), detail = character())

check <- function(name, ok, detail = "") {
  results[nrow(results) + 1, ] <<- list(name, isTRUE(ok), paste(detail, collapse = " "))
}

# A one-row test entry, in the same shape the entry form produces.
entry <- function(w, name = "Sam", date = "2026-03-02", erg = "", flag = "Complete",
                  protocol = TRUE, flags = "") {
  tibble(
    athlete_name = name, test_date = as.Date(date),
    power_45s = w[1], power_3min = w[2], power_6min = w[3],
    power_10min = w[4], power_20min = w[5],
    notes = "", ergometer_type = erg, protocol_confirmed = protocol,
    test_validity_flag = flag, context_flags = flags
  )
}

typical <- c(620, 410, 360, 330, 295)

# ------------------------------
# Validation rules (D-005 to D-009)
# ------------------------------

v <- validate_test_values(entry(typical))
check("Typical profile: no retest signal", v$retest_level == "No retest signal")

v <- validate_test_values(entry(c(620, 410, 360, 345, 295)))
check("10 min 4% above the smooth curve: curve-shape warning", "curve_shape" %in% v$flags)

v <- validate_test_values(entry(c(620, 410, 360, 330, 335)))
check("20 min above 10 min: outputs withheld as a pattern block",
      v$outputs_withheld && v$has_pattern_block && v$retest_level == "Retest recommended")

v <- validate_test_values(entry(c(1900, 410, 360, 330, 295)))
check("45 s more than 3x the 3 min value: suspected entry error", v$has_pattern_block)

v <- validate_test_values(entry(c(1200, 410, 360, 330, 295)))
check("45 s / 3 min ratio of 2.9: warned, not blocked",
      !v$outputs_withheld && "steep_45s_3min" %in% v$flags)

v <- validate_test_values(entry(c(620, 410, 360, 330, 601)))
check("20 min of 601 W: hard error (600 W cap)", length(v$errors) == 1)

v <- validate_test_values(entry(c(620, 410, 0, 330, 295)))
check("A zero value: hard error", length(v$errors) == 1)

v <- validate_test_values(entry(typical, name = ""))
check("Missing athlete name: hard error", length(v$errors) == 1)

v <- validate_test_values(entry(typical, date = NA))
check("Missing start date: hard error", length(v$errors) == 1)

v <- validate_test_values(entry(typical, flag = "Retest needed"))
check("Marked Retest needed: withheld, not a pattern block",
      v$outputs_withheld && !v$has_pattern_block)

v <- validate_test_values(entry(typical, flags = "Interrupted or failed effort"))
check("Interrupted effort flag: outputs withheld", v$outputs_withheld)

v <- validate_test_values(entry(typical, flags = "Illness; Poor sleep"))
check("Illness flag: retest suggested", v$retest_level == "Retest suggested")

v <- validate_test_values(entry(typical, flags = "Poor sleep"))
check("Poor sleep flag: keep, but interpret cautiously", v$retest_level == "Keep, but interpret cautiously")

v <- validate_test_values(entry(typical, protocol = FALSE))
check("Protocol unconfirmed: keep, but interpret cautiously", v$retest_level == "Keep, but interpret cautiously")

v <- validate_test_values(entry(c(620, 410, 360, 358, 295)))
check("6 and 10 min within 1%: near-flat warning", "near_flat" %in% v$flags)

v <- validate_test_values(entry(c(620, 410, 300, 280, 250)))
check("27% drop from 3 to 6 min: steep-decline warning", "steep_decline" %in% v$flags)

# ------------------------------
# Model outputs
# ------------------------------

m <- estimate_secondary_metrics(to_long_profile(entry(c(700, 360, 350, 345, 344))))
check("CP and W' are both available or both missing", is.na(m$cp_w) == is.na(m$wprime_j))

predictions <- build_prediction_table(fit_power_duration_model(to_long_profile(entry(typical))))
check("8 prediction durations, 30 and 60 min marked extrapolated",
      nrow(predictions) == 8 &&
        all(predictions$estimate_type[predictions$duration %in% c("30 min", "60 min")] == "Extrapolated estimate"))

# ------------------------------
# History baseline (D-010) and saving (D-011, D-013)
# ------------------------------

history <- recompute_uploaded_history(bind_rows(
  make_profile_record(entry(typical, date = "2026-01-05", erg = "Concept2 BikeErg"),
                      validate_test_values(entry(typical))),
  make_profile_record(entry(c(620, 410, 360, 330, 335), date = "2026-02-02", erg = "Concept2 BikeErg", flag = "Compromised"),
                      validate_test_values(entry(c(620, 410, 360, 330, 335), flag = "Compromised")))
))

b <- find_baseline(entry(c(640, 425, 372, 340, 304), erg = "concept2 bikeerg"), history)
check("Baseline skips the compromised record and matches equipment case-insensitively",
      !is.null(b$baseline) && b$baseline$test_date == as.Date("2026-01-05"))

b <- find_baseline(entry(c(640, 425, 372, 340, 304), erg = "Wattbike Atom"), history)
check("Different equipment: a note instead of a baseline", is.null(b$baseline) && !is.null(b$note))

b <- find_baseline(entry(typical, date = "2025-12-01", erg = "Concept2 BikeErg"), history)
check("No earlier record: no baseline", is.null(b$baseline))

check("Compromised record keeps raw values with empty model fields",
      is.na(history$estimated_cp[2]) && is.na(history$predicted_power_1min[2]) && history$power_20min[2] == 335)

replaced <- upsert_history_record(history, make_profile_record(
  entry(c(630, 415, 365, 335, 300), name = "sam", date = "2026-01-05", erg = "Concept2 BikeErg"),
  validate_test_values(entry(c(630, 415, 365, 335, 300)))
))
check("Saving the same name (any case) and date replaces the record",
      nrow(replaced) == 2 && replaced$power_45s[replaced$test_date == as.Date("2026-01-05")] == 630)

ann_watts <- c(500, 330, 290, 270, 240)
two_athletes <- recompute_uploaded_history(bind_rows(
  make_profile_record(entry(typical, name = "Sam Smith", date = "2026-01-05"), validate_test_values(entry(typical))),
  make_profile_record(entry(typical, name = "sam  smith", date = "2026-02-02"), validate_test_values(entry(typical))),
  make_profile_record(entry(ann_watts, name = "Ann", date = "2026-01-12"), validate_test_values(entry(ann_watts)))
))

choices <- athlete_choices(two_athletes)
check("Athlete list: one entry per athlete, names matched as in D-013",
      length(choices) == 2 && setequal(unname(choices), c("ann", "sam smith")))

trend <- prepare_history_trend_data(two_athletes, "sam smith")
check("Trends use only the chosen athlete",
      dplyr::n_distinct(trend$test_date) == 2 && !(as.Date("2026-01-12") %in% trend$test_date))

# ------------------------------
# CSV import and export (D-012)
# ------------------------------

raw_columns <- c("athlete_name", "test_date", "power_45s", "power_3min", "power_6min",
                 "power_10min", "power_20min", "ergometer_type", "protocol_confirmed",
                 "test_validity_flag", "context_flags")

csv_path <- tempfile(fileext = ".csv")
write_history_csv(history, csv_path)
round_trip <- read_history_csv(csv_path)
check("Download then upload keeps every raw value",
      isTRUE(all.equal(as.data.frame(history[raw_columns]),
                       as.data.frame(round_trip$history[raw_columns]), check.attributes = FALSE)))
check("Download then upload recomputes the same results",
      isTRUE(all.equal(history$estimated_cp, round_trip$history$estimated_cp)) &&
        identical(history$retest_level, round_trip$history$retest_level))

example_path <- tempfile(fileext = ".csv")
write_csv(build_example_upload_csv(), example_path)
example <- read_history_csv(example_path)
check("Example CSV uploads, with one ergometer row and one running row",
      nrow(example$history) == 2 && setequal(example$history$sport, c("Ergometer", "Running")) &&
        example$history$context_flags[example$history$sport == "Running"] == "Wind")

upload <- function(lines, slash_order = "auto") {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  tryCatch(read_history_csv(path, slash_order), error = function(e) conditionMessage(e))
}
header <- "athlete_name,test_date,power_45s,power_3min,power_6min,power_10min,power_20min"

r <- upload(c(
  "athlete_name,test_date,power_45s,power_3min,power_6min,power_10min,power_20min,notes,ergometer_type,protocol_confirmed,test_validity_flag,estimated_cp,riegel_S,riegel_E",
  "Sam,2026-03-02,620,410,360,330,295,NA,Concept2 BikeErg,TRUE,Complete,290,1200,0.8"
))
check("Version 1.0 export uploads, legacy Riegel columns ignored",
      is.list(r) && !("riegel_S" %in% names(r$history)) && r$history$protocol_confirmed)

r <- upload(c(header, "Sam,3/2/2026,620,410,360,330,295", "Ann,4/6/26,500,330,290,270,240"), "mdy")
check("US-style dates upload when month-first is chosen",
      is.list(r) && all(sort(r$history$test_date) == as.Date(c("2026-03-02", "2026-04-06"))))

r <- upload(c(header, "Sam,3/2/2026,620,410,360,330,295", "Ann,4/6/26,500,330,290,270,240"))
check("Dates that could be read 2 ways: upload stops for a choice",
      is.character(r) && grepl("month-first or day-first", r))

r <- upload(c(header, "Sam,02/03/2026,620,410,360,330,295"), "dmy")
check("Day-first chosen: 02/03/2026 reads as March 2",
      is.list(r) && r$history$test_date == as.Date("2026-03-02"))

r <- upload(c(header, "Sam,25/03/2026,620,410,360,330,295", "Ann,02/04/2026,500,330,290,270,240"))
check("A day above 12 in first place: whole file read day-first",
      is.list(r) && all(sort(r$history$test_date) == as.Date(c("2026-03-25", "2026-04-02"))))

r <- upload(c(header, "Sam,25/03/2026,620,410,360,330,295", "Ann,03/25/2026,500,330,290,270,240"))
check("Mixed day-first and month-first dates: upload rejected",
      is.character(r) && grepl("mixes", r))

r <- upload(c(header, "Sam,2026-03-02,620,410,360,330,335"))
check("Ordering failure uploads as compromised",
      is.list(r) && r$n_compromised == 1 && r$history$test_validity_flag == "Compromised")

r <- upload(c(paste0(header, ",ergometer_type"), "Sam,2026-03-02,620,410,360,330,295,Mystery Bike"))
check("Unrecognized equipment uploads as Other", is.list(r) && r$history$ergometer_type == "Other")

r <- upload(c(header, "Sam,2026-03-02,abc,410,360,330,295"))
check("Non-numeric watts: upload rejected, naming row 2", is.character(r) && grepl("Row 2", r))

r <- upload(c(paste0(header, ",context_flags"), "Sam,2026-03-02,620,410,360,330,295,Bad sleep"))
check("Unknown context flag: upload rejected", is.character(r) && grepl("context_flags", r))

r <- upload(c(header, "Sam Smith,2026-03-02,620,410,360,330,295", "sam  smith,2026-03-02,630,415,365,335,300"))
check("Duplicate athlete and start date: upload rejected", is.character(r) && grepl("Rows 2 and 3", r))

r <- upload(c("athlete_name,test_date,power_45s", "Sam,2026-03-02,600"))
check("Missing required columns: upload rejected", is.character(r) && grepl("missing required columns", r))

# ------------------------------
# Running (D-020 to D-022)
# ------------------------------

# A one-row running entry. Distances are in meters, as stored.
run_entry <- function(d, name = "Sam", date = "2026-03-02", setting = "", unit = "m",
                      flag = "Complete", protocol = TRUE, flags = "") {
  tibble(
    athlete_name = name, test_date = as.Date(date), sport = "Running",
    distance_45s_m = d[1], distance_3min_m = d[2], distance_6min_m = d[3],
    distance_10min_m = d[4], distance_20min_m = d[5], distance_unit = unit,
    notes = "", run_setting = setting, protocol_confirmed = protocol,
    test_validity_flag = flag, context_flags = flags
  )
}

run_typical <- c(270, 870, 1610, 2580, 4990)
speed_10min <- run_typical[4] / 600

check("Pace: 4.0 m/s is 4:10 /km, and 8:00 /mi converts correctly",
      format_pace(4.0) == "4:10 /km" && format_pace(1609.344 / 480, "mi") == "8:00 /mi")

v <- validate_test_values(run_entry(run_typical))
check("Running: typical profile has no retest signal", v$retest_level == "No retest signal")

v <- validate_test_values(run_entry(c(run_typical[1:4], speed_10min * 1.01 * 1200)))
check("Running: 20 min 1% faster than 10 min is a small reversal, kept with caution",
      !v$outputs_withheld && "small_reversal" %in% v$flags && grepl("/km", paste(v$warnings, collapse = " ")))

v <- validate_test_values(run_entry(c(run_typical[1:4], speed_10min * 1.03 * 1200)))
check("Running: 20 min 3% faster than 10 min withholds outputs",
      v$outputs_withheld && v$has_pattern_block && "order_violation" %in% v$flags)

v <- validate_test_values(run_entry(run_typical / 10))
check("Running: 20 min distance under 1,000 m is a hard error", length(v$errors) == 1)

v <- validate_test_values(run_entry(c(run_typical[1:4], 9000)))
check("Running: 20 min distance over 8,000 m is a hard error", length(v$errors) == 1)

v <- validate_test_values(run_entry(c(600, run_typical[2:5])))
check("Running: 45 s more than 2x as fast as 3 min is a suspected entry error",
      v$has_pattern_block && "suspected_entry_error" %in% v$flags)

v <- validate_test_values(run_entry(c(run_typical[1:4], speed_10min * 0.85 * 1200)))
check("Running: 15% speed drop from 10 to 20 min is a steep-decline warning", "steep_decline" %in% v$flags)

v <- validate_test_values(run_entry(c(270, 870, 1610, 2590, 5000)))
check("Running: no near-flat warning", !("near_flat" %in% v$flags))

v <- validate_test_values(run_entry(run_typical, flags = "Wind"))
check("Running: Wind flag means retest suggested", v$retest_level == "Retest suggested")

run_record <- recompute_history_record(run_entry(run_typical, unit = "km", setting = "Track"))
check("Running record: CS, D', and predicted speeds filled; power columns empty",
      run_record$estimated_cs_m_per_s > 3.5 && run_record$estimated_cs_m_per_s < 4.5 &&
        run_record$estimated_d_prime_m > 0 && !is.na(run_record$predicted_speed_4min_m_per_s) &&
        is.na(run_record$power_45s) && is.na(run_record$estimated_cp) &&
        is.na(run_record$predicted_power_4min) && run_record$distance_unit == "km")

ergo_record <- recompute_history_record(entry(typical))
check("Ergometer record: running columns empty, sport set",
      ergo_record$sport == "Ergometer" && is.na(ergo_record$distance_45s_m) &&
        is.na(ergo_record$estimated_cs_m_per_s) && !is.na(ergo_record$estimated_cp))

both <- upsert_history_record(upsert_history_record(empty_history(), ergo_record), run_record)
check("Same athlete and date in both sports: 2 separate records", nrow(both) == 2)

run_history <- recompute_uploaded_history(bind_rows(
  make_profile_record(entry(typical, date = "2026-01-05"), validate_test_values(entry(typical))),
  make_profile_record(run_entry(run_typical, date = "2026-01-12", setting = "Track"),
                      validate_test_values(run_entry(run_typical)))
))
b <- find_baseline(run_entry(run_typical, date = "2026-03-02", setting = "Track"), run_history)
check("Running baseline: same sport and setting, skipping ergometer tests",
      !is.null(b$baseline) && b$baseline$test_date == as.Date("2026-01-12"))

b <- find_baseline(run_entry(run_typical, date = "2026-03-02", setting = "Outdoor (GPS watch)"), run_history)
check("Running baseline: Track and Outdoor aren't comparable, so a note instead",
      is.null(b$baseline) && grepl("compared by pace", b$note))

b <- find_baseline(entry(typical, date = "2026-03-02"), run_history)
check("Ergometer baseline ignores running tests",
      !is.null(b$baseline) && b$baseline$test_date == as.Date("2026-01-05"))

entered <- build_test_dataframe(list(
  athlete_name = "Sam", test_date = as.Date("2026-03-02"), sport = "Running", distance_unit = "km",
  distance_45s = 0.27, distance_3min = 0.87, distance_6min = 1.61, distance_10min = 2.58,
  distance_20min = 4.99, power_45s = 620, notes = "", run_setting = "Not specified",
  protocol_confirmed = TRUE, test_validity_flag = "Complete", context_flags = NULL
))
check("Entry form: km converted to meters, power ignored, Not specified stored blank",
      entered$distance_20min_m == 4990 && is.na(entered$power_45s) && entered$run_setting == "")

run_header <- "athlete_name,test_date,sport,distance_45s_m,distance_3min_m,distance_6min_m,distance_10min_m,distance_20min_m"

csv_path <- tempfile(fileext = ".csv")
write_history_csv(both, csv_path)
round_trip <- read_history_csv(csv_path)
check("Mixed-sport history: download then upload keeps both records",
      nrow(round_trip$history) == 2 &&
        isTRUE(all.equal(round_trip$history$estimated_cs_m_per_s, both$estimated_cs_m_per_s)))

r <- upload(c(run_header, "Sam,2026-03-02,Running,270,870,1610,2580,4990"))
check("Running row uploads, with distance_unit defaulting to m",
      is.list(r) && r$history$sport == "Running" && r$history$distance_unit == "m")

r <- upload(c(paste0(header, ",sport"), "Sam,2026-03-02,620,410,360,330,295,Ergometer", "Ann,2026-03-02,,,,,,Running"))
check("Running rows need the distance columns",
      is.character(r) && grepl("missing required columns: distance_45s_m", r))

r <- upload(c(paste0(header, ",sport"), "Sam,2026-03-02,620,410,360,330,295,Rowing"))
check("Unknown sport: upload rejected", is.character(r) && grepl("sport must be", r))

r <- upload(c(paste0(run_header, ",distance_unit"), "Sam,2026-03-02,Running,270,870,1610,2580,4990,yards"))
check("Unknown distance unit: upload rejected", is.character(r) && grepl("distance_unit", r))

r <- upload(c(paste0(header, ",context_flags"), "Sam,2026-03-02,620,410,360,330,295,Wind"))
check("Running-only flag on an ergometer row: upload rejected", is.character(r) && grepl("context_flags", r))

r <- upload(c(paste0(run_header, ",run_setting"), "Sam,2026-03-02,Running,270,870,1610,2580,4990,track"))
check("Running setting matched ignoring case", is.list(r) && r$history$run_setting == "Track")

# Pace display (build step 8). These entries use km, so pace shows per km.
run_km <- run_entry(run_typical, unit = "km", setting = "Track")
run_profile <- to_long_profile(run_km)
run_fit <- fit_power_duration_model(run_profile)
run_cs <- estimate_secondary_metrics(run_profile)

check("Rates show as pace for running and watts for ergometers",
      format_rate(4, run_km) == "4:10 /km" && format_rate(300, entry(typical)) == "300 W" &&
        rate_axis_title(run_entry(run_typical, unit = "mi")) == "Pace (per mile)")

summary_rows <- build_summary_table(run_km, run_fit, run_cs, validate_test_values(run_km))
check("Running summary: distances with pace, CS as pace, D' in meters",
      "20 min distance" %in% summary_rows$metric &&
        summary_rows$value[summary_rows$metric == "20 min distance"] == "4.99 km (4:00 /km)" &&
        grepl("/km$", summary_rows$value[summary_rows$metric == "Estimated critical speed"]) &&
        grepl(" m$", summary_rows$value[startsWith(summary_rows$metric, "Estimated D")]))

targets <- build_training_targets(build_prediction_table(run_fit, 3), run_cs, run_km)
check("Running training targets show pace", all(grepl("^[0-9]+ min: [0-9]+:[0-9]{2} /km", targets$anchor_1)))

run_older <- recompute_history_record(run_entry(c(265, 860, 1590, 2550, 4930), date = "2026-01-12",
                                                unit = "km", setting = "Track"))
comparison <- build_comparison_table(run_km, upsert_history_record(empty_history(), run_older))
check("Running comparison: change in seconds per km, faster is negative",
      "Change (s per km)" %in% names(comparison) && all(grepl("^-[0-9]+ s$", comparison$`Change (s per km)`)))

mixed <- upsert_history_record(upsert_history_record(run_history, run_older), recompute_history_record(run_km))
trend <- prepare_history_trend_data(mixed, "sam", "Running")
check("Running trends: running blocks only, with estimated CS",
      !any(trend$test_date == as.Date("2026-01-05")) && "Estimated CS" %in% trend$metric)

shown <- format_history_table(mixed)
check("History table: watts for ergometer rows, pace for running rows",
      grepl(" W$", shown$`20 min`[shown$Sport == "Ergometer"][1]) &&
        all(grepl(" /(km|mi)$", shown$`20 min`[shown$Sport == "Running"])))

# ------------------------------
# Server logic
# ------------------------------

all_outputs <- c(
  "completion_status", "validation_box", "pd_plot", "summary_table", "prediction_table",
  "training_targets_table", "comparison_table", "overlay_plot", "history_plot",
  "overlay_plot_area", "history_plot_area",
  "history_table", "history_action_status", "save_status"
)

# Returns the outputs that failed with a real error. Validation messages,
# such as "Fix the blocking issues", count as working.
broken_outputs <- function(session) {
  broken <- character()
  for (name in all_outputs) {
    result <- tryCatch(
      { session$output[[name]]; "ok" },
      error = function(e) {
        if (inherits(e, c("shiny.silent.error", "validation"))) "ok" else conditionMessage(e)
      }
    )
    if (result != "ok") broken <- c(broken, paste0(name, ": ", result))
  }
  broken
}

# Warnings are suppressed here because the trend chart skips the empty CP of
# compromised records, which ggplot reports as "Removed rows" warnings.
suppressWarnings(testServer(server, {
  set_watts <- function(w) {
    session$setInputs(power_45s = w[1], power_3min = w[2], power_6min = w[3],
                      power_10min = w[4], power_20min = w[5])
  }

  session$setInputs(athlete_name = "", test_date = as.Date("2026-03-02"),
                    ergometer_type = "Not specified", notes = "", protocol_confirmed = FALSE,
                    test_validity_flag = "Complete", context_flags = NULL)
  set_watts(rep(NA, 5))
  broken <- broken_outputs(session)
  check("Server: empty form shows every output without crashing", length(broken) == 0, broken)

  box <- paste(unlist(output$validation_box), collapse = " ")
  check("Server: empty form shows no input-check box", !grepl("Blocking issues", box))

  set_watts(typical)
  box <- paste(unlist(output$validation_box), collapse = " ")
  check("Server: missing name shows the error without retest guidance",
        grepl("Athlete name is required", box) && !grepl("Retest guidance", box))
  set_watts(rep(NA, 5))

  session$setInputs(test_date = NULL)
  broken <- broken_outputs(session)
  check("Server: cleared date doesn't crash", length(broken) == 0, broken)

  session$setInputs(test_date = as.Date("2026-03-02"), athlete_name = "Sam Smith",
                    protocol_confirmed = TRUE, ergometer_type = "Concept2 BikeErg")
  set_watts(typical)
  broken <- broken_outputs(session)
  check("Server: valid entry shows every output", length(broken) == 0, broken)

  session$setInputs(save_test = 1)
  check("Server: save adds a record", nrow(history_data()) == 1)

  session$setInputs(power_20min = 300, save_test = 2)
  check("Server: saving the same key asks first", nrow(history_data()) == 1 && !is.null(pending_record()))

  session$setInputs(confirm_replace = 1)
  check("Server: confirming replaces the record", nrow(history_data()) == 1 && history_data()$power_20min == 300)

  session$setInputs(test_date = as.Date("2026-04-06"))
  set_watts(c(620, 410, 360, 330, 335))
  session$setInputs(save_test = 3)
  check("Server: ordering failure isn't saved while marked Complete", nrow(history_data()) == 1)

  broken <- broken_outputs(session)
  check("Server: withheld entry shows every output without crashing", length(broken) == 0, broken)

  session$setInputs(test_validity_flag = "Compromised", save_test = 4)
  check("Server: saves as compromised with empty CP",
        nrow(history_data()) == 2 && is.na(history_data()$estimated_cp[2]))

  session$setInputs(test_validity_flag = "Complete", test_date = as.Date("2026-05-04"))
  set_watts(c(700, 460, 400, 365, 325))
  check("Server: history check uses the earlier normal test, skipping the compromised one",
        "history_mismatch" %in% current_validation()$flags)

  session$setInputs(trend_athlete = "sam smith")

  broken <- broken_outputs(session)
  check("Server: comparison table and overlay render", length(broken) == 0, broken)

  session$setInputs(save_test = 5, history_table_rows_selected = 1, load_selected_test = 1)
  check("Server: loading a record shows the loaded-test note", grepl("Loaded the saved test", save_status_message()))

  session$setInputs(delete_selected_test = 1)
  remaining <- history_data()
  latest_flags <- remaining$warning_flags[remaining$test_date == as.Date("2026-05-04")]
  check("Server: delete recalculates the remaining records",
        nrow(remaining) == 2 && !grepl("history_mismatch", latest_flags))

  download_path <- tempfile(fileext = ".csv")
  write_history_csv(history_data(), download_path)
  session$setInputs(upload_history = list(datapath = download_path, name = "history.csv"))
  check("Server: uploading a downloaded history works", nrow(history_data()) == 2)

  bad_path <- tempfile(fileext = ".csv")
  writeLines(c(header, "Sam,2026-03-02,abc,410,360,330,295"), bad_path)
  session$setInputs(upload_history = list(datapath = bad_path, name = "bad.csv"))
  check("Server: a rejected upload leaves history unchanged", nrow(history_data()) == 2)

  session$setInputs(
    sport = "Running", athlete_name = "Ann", test_date = as.Date("2026-06-01"),
    distance_unit = "km", run_setting = "Track", running_context_flags = "Wind", context_flags = NULL,
    distance_45s = 0.27, distance_3min = 0.87, distance_6min = 1.61, distance_10min = 2.58, distance_20min = 4.99
  )
  check("Server: running entry reads km as meters and includes running conditions",
        current_inputs()$sport == "Running" && current_inputs()$distance_20min_m == 4990 &&
          current_inputs()$context_flags == "Wind")

  broken <- broken_outputs(session)
  check("Server: running entry shows every output without crashing", length(broken) == 0, broken)

  session$setInputs(save_test = 6)
  check("Server: running entry saves as a running record",
        nrow(history_data()) == 3 && sum(history_data()$sport == "Running") == 1)
      
  check("Server: running predictions keep decimals for pace",
        any(current_predictions()$predicted_power_w != round(current_predictions()$predicted_power_w)))

  session$setInputs(trend_athlete = "ann", trend_sport = "Running")
  broken <- broken_outputs(session)
  area <- paste(unlist(output$history_plot_area), collapse = " ")
  check("Server: running trends ask for a second running block",
        length(broken) == 0 && grepl("second testing block in this sport", area), broken)
      
  session$setInputs(sport = "Ergometer")
  check("Server: running conditions are ignored for ergometer blocks", current_inputs()$context_flags == "")
}))

# ------------------------------
# Results
# ------------------------------

for (i in seq_len(nrow(results))) {
  cat(if (results$pass[i]) "PASS  " else "FAIL  ", results$check[i],
      if (!results$pass[i] && results$detail[i] != "") paste0("\n      ", results$detail[i]) else "",
      "\n", sep = "")
}
cat("\n", sum(results$pass), " of ", nrow(results), " checks passed.\n", sep = "")
