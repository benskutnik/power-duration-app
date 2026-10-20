# Power-Duration Relationship App Blueprint

## 1. Executive Summary

This project is a user-facing Shiny app built in RStudio for home-gym athletes using ergometers. The app estimates and tracks a user's power-duration relationship from five maximal average-power tests completed across an 11-day testing window.

The app is designed to do four things well:

1. collect valid field-test power data
2. visualize the user's power-duration profile
3. compare current and historical results
4. generate practical power-based training targets

The app is not a lab substitute. It should be useful, honest, and simple.

---

## 2. Product Goal

Build a Shiny app that helps an athlete or coach answer these questions:

- What does this athlete's current power-duration profile look like?
- How has it changed over time?
- What practical training targets can be estimated from this profile?
- Was the test data good enough to trust?

### Core value proposition

Use five structured maximal ergometer efforts to produce a clear, trackable, and practically useful power-duration profile for training.

---

## 3. Target User

### Primary user
Home-gym athletes using ergometers who want a repeatable way to assess and track performance.

### Secondary user
Coaches who want a simple field-based assessment workflow for athlete monitoring and training guidance.

### Assumptions about the user
- They can collect average wattage from an ergometer.
- They can follow a simple testing protocol.
- They want practical outputs more than academic outputs.
- They are not assumed to understand critical power modeling or exercise physiology.

---

## 4. Version 1 Scope

### Included in version 1
- Manual input for five average-power values
- Current profile visualization
- Fitted power-duration curve
- Secondary summary metrics such as estimated critical power and W′
- Historical result storage in-session
- CSV upload and download
- Historical trend views
- Practical training targets
- User-facing protocol page
- Basic validation and warning logic

### Excluded from version 1
- Heart rate inputs
- Body composition metrics
- Cloud accounts or user login
- Database storage
- Wearable integrations
- Automatic ergometer import
- Lab-grade threshold claims
- Complex coach dashboards
- Multi-athlete management
- Machine-learning features
- Recovery or readiness scoring

---

## 5. Scientific Guardrails

### Safe claims
The app may claim that it:
- estimates a user's power-duration relationship from field testing
- provides a practical performance profile
- tracks changes in power outputs over time
- estimates practical training targets from the measured profile
- includes critical power and W′ as secondary estimated metrics

### Unsafe claims
The app should not claim that it:
- directly measures lactate threshold
- directly measures ventilatory thresholds
- determines exact metabolic zones
- diagnoses physiology
- replaces laboratory testing
- produces exact individualized training prescriptions

### Approved terminology
Use:
- estimated
- practical training targets
- power-duration profile
- field-based assessment
- current profile
- historical trend
- secondary metric
- estimated critical power

Avoid or discourage:
- proven zone
- exact threshold
- direct measurement
- metabolic truth
- personalized physiology engine
- validated Zone 2 from this test alone

### User-facing uncertainty language
Preferred style:
- "This app estimates your power-duration profile from field testing."
- "Training targets are practical estimates, not direct laboratory measures."
- "Critical power is included as a secondary estimated metric."

---

## 6. Testing Methodology

## Test durations
The app is built around five maximal average-power efforts:

- 45 seconds
- 3 minutes
- 6 minutes
- 10 minutes
- 20 minutes

### Why this set was chosen
- 45 seconds captures short-duration power
- 3, 6, and 10 minutes strengthen the middle of the curve
- 20 minutes anchors the longer end without the burden of a 30-minute test
- this set is stronger than 45 s / 3 min / 10 min / 30 min because it fills the largest mid-duration gap

### Modeling philosophy
The app should prioritize a broad descriptive power-duration curve as the main display.

#### Primary model
- power-law model for the full displayed curve

#### Secondary metrics
- estimated critical power
- estimated W′

### Interpretation rule
The broad curve is the main user-facing representation. Critical power and W′ are useful secondary summaries, not the sole logic of the app.

---

## 7. 11-Day Testing Window

Complete five maximal efforts across an 11-day testing window structured around two workweeks, a weekend break, and deliberate recovery between the more fatiguing efforts.

## Weekly schedule

| Week | Day | Session |
|---|---|---|
| 1 | Monday | 45-second maximal effort |
| 1 | Wednesday | 3-minute maximal effort |
| 1 | Friday | 6-minute maximal effort |
| 2 | Monday | 10-minute maximal effort |
| 2 | Thursday | 20-minute maximal effort |

### Why this order and spacing were chosen
- the tests move from shortest to longest
- the 3-, 6-, and 10-minute efforts are separated enough to reduce carryover fatigue
- the 20-minute test is last because it is the most pacing-sensitive and most likely to be affected by residual fatigue
- the weekend acts as a recovery bridge, not a training block

## Standardization rules
Users should be instructed to:
- use the same ergometer for all tests
- use the same machine settings each time
- test at about the same time of day
- use the same warm-up before each effort
- avoid hard training during the testing window
- record average watts for the full effort
- postpone a test if fatigue, illness, poor sleep, or poor readiness would distort performance

## Warm-up template
- 5 minutes easy
- 3 minutes moderate
- 2 x 20 seconds strong with 1 minute easy between
- 3 to 5 minutes easy before the test

## Recovery guidance
Between test days:
- avoid hard conditioning
- reduce or modify lower-body strength work
- keep weekend training easy to moderate at most
- prioritize freshness for the next scheduled test

## Valid-result rule
A test is valid when:
- it was performed as a maximal effort for the full duration
- conditions were reasonably standardized
- pacing was not obviously reckless or passive
- fatigue did not clearly distort the result

---

## 8. Data Model

## Required input fields
- athlete_name
- test_date
- power_45s
- power_3min
- power_6min
- power_10min
- power_20min

## Optional input fields
- notes
- ergometer_type
- protocol_confirmed
- test_validity_flag

## Derived fields
- estimated_cp
- estimated_w_prime
- power_law_a
- power_law_b
- predicted_power_1min
- predicted_power_5min
- predicted_power_15min
- predicted_power_30min
- predicted_power_60min
- warning_flags

## Recommended CSV columns
- athlete_name
- test_date
- power_45s
- power_3min
- power_6min
- power_10min
- power_20min
- estimated_cp
- estimated_w_prime
- power_law_a
- power_law_b
- notes

### Future-proofing notes
Keep naming consistent and machine-readable. Prefer snake_case. Do not hard-code future optional fields into the core plotting logic.

---

## 9. Core Outputs

## Current profile outputs
- plotted test points
- fitted power-duration curve
- summary table of entered powers
- summary table of estimated metrics
- predicted powers at selected durations

## Historical outputs
- saved test table
- trend chart for 45 s, 3 min, 6 min, 10 min, 20 min
- trend chart or table for estimated critical power
- optional overlay of current and selected past curves

## Training outputs
Training outputs should be framed as practical target categories, not hard physiological zones.

Recommended categories:
- short-power repeats
- anaerobic-capacity intervals
- VO2-type intervals
- threshold-focused work
- steady aerobic work

Recommended predicted durations:
- 1 minute
- 2 minutes
- 4 minutes
- 8 minutes
- 12 minutes
- 15 minutes
- 30 minutes
- 60 minutes

---

## 10. App Information Architecture

## Page structure

### 1. Current Profile
Purpose:
- collect the current test inputs
- show the main power-duration plot
- show summary outputs

Contents:
- athlete information inputs
- five wattage inputs
- note field
- main plot
- model summary
- validation warnings
- save test button

### 2. History
Purpose:
- let users review saved tests and compare progress over time

Contents:
- history table
- CSV upload
- CSV download
- trend charts
- selected test comparison

### 3. Training Targets
Purpose:
- translate the curve into practical power targets

Contents:
- target categories
- predicted powers at useful durations
- short explanatory text
- uncertainty/disclaimer text

### 4. Protocol
Purpose:
- standardize testing quality

Contents:
- 11-day testing window
- warm-up instructions
- pacing guidance
- delay and retest guidance
- validity rules

---

## 11. Visualization Plan

## Main plot
The main plot should show:
- current test points
- fitted power-duration curve
- optional selected historic curves

### Recommended design choices
- x-axis: duration on a log scale
- y-axis: power in watts
- points: current entered test values
- line: main fitted curve
- historic comparison: lighter or secondary lines
- annotations: minimal and useful only

### What not to do
- do not overload the plot with too many labels
- do not place long physiological explanations on the chart
- do not show every derived metric directly on the plot

## Historical visualization
Show trend charts for:
- each fixed-duration power
- estimated critical power

Keep historical comparisons simple and easy to scan.

---

## 12. Validation Rules

## Hard-stop rules
Block save or model fitting if:
- any required value is missing
- any required value is zero or negative
- test date is missing
- all entered powers are identical

## Soft-warning rules
Warn but allow save if:
- longer-duration power is higher than shorter-duration power
- 6-minute power is unusually close to 3-minute power
- 20-minute power is unusually depressed relative to the rest of the profile
- the profile shape suggests pacing failure or accumulated fatigue

## Suggested warning language
- "These values do not follow a typical power-duration pattern. Review your entries."
- "This result may reflect pacing error, fatigue, or a non-maximal effort."
- "Consider repeating the affected duration if the test did not reflect your best effort."

---

## 13. Shiny Architecture

## MVP file structure
Version 1 can be built as a single `app.R` file with clear internal sections:
- libraries
- constants
- helper functions
- UI
- server
- app launch

## Key reactive objects
- current_test
- validated_test
- model_fit
- secondary_metrics
- predictions
- training_targets
- history_data

## Helper functions needed
- build_current_test_data()
- validate_test_inputs()
- fit_power_law_model()
- estimate_cp_metrics()
- generate_prediction_table()
- generate_training_targets()
- prepare_history_trend_data()
- create_warning_messages()

## Future modularization path
Later versions can split into:
- `global.R`
- `R/helpers.R`
- `R/models.R`
- `R/validation.R`
- `R/plots.R`
- `app.R`

---

## 14. Build Roadmap

## Phase 1: MVP
- current test inputs
- main plot
- model summary
- save to history
- history table
- protocol tab

## Phase 2: Better comparison
- historic trend charts
- curve overlay by selected dates
- stronger warning logic
- cleaner training-target display

## Phase 3: Refinement
- modularized code
- better copy and help text
- richer export options
- more polished UI

## Deliberately postponed
- authentication
- database storage
- API integrations
- heart rate
- multi-user features

---

## 15. Immediate Next Decisions

The next decisions to lock are:

1. exact user-facing wording for the app introduction and disclaimer
2. exact summary metrics to show by default
3. exact predicted durations to display on the training targets page
4. exact warning thresholds for suspicious test patterns
5. MVP plot and table layout

---

## 16. Working Principle

This app should be built around one operating principle:

**Make the outputs useful enough to guide training, but modest enough to remain scientifically honest.**
