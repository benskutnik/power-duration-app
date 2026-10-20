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
check("Example CSV uploads", nrow(example$history) == 1 && example$history$context_flags == "Poor sleep")

upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  tryCatch(read_history_csv(path), error = function(e) conditionMessage(e))
}
header <- "athlete_name,test_date,power_45s,power_3min,power_6min,power_10min,power_20min"

r <- upload(c(
  "athlete_name,test_date,power_45s,power_3min,power_6min,power_10min,power_20min,notes,ergometer_type,protocol_confirmed,test_validity_flag,estimated_cp,riegel_S,riegel_E",
  "Sam,2026-03-02,620,410,360,330,295,NA,Concept2 BikeErg,TRUE,Complete,290,1200,0.8"
))
check("Version 1.0 export uploads, legacy Riegel columns ignored",
      is.list(r) && !("riegel_S" %in% names(r$history)) && r$history$protocol_confirmed)

r <- upload(c(header, "Sam,3/2/2026,620,410,360,330,295", "Ann,4/6/26,500,330,290,270,240"))
check("US-style dates upload", is.list(r) && all(r$history$test_date == as.Date(c("2026-03-02", "2026-04-06"))))

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
# Server logic
# ------------------------------

all_outputs <- c(
  "completion_status", "validation_box", "pd_plot", "summary_table", "prediction_table",
  "training_targets_table", "comparison_table", "overlay_plot", "history_plot",
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
