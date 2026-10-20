# 10 - Master Build Spec

## Locked Project Decisions

- App type: user-facing Shiny app in RStudio for home-gym athletes using ergometers.
- Core purpose: estimate and track a practical power-duration profile from repeated field tests.
- Fixed test durations: 45 seconds, 3 minutes, 6 minutes, 10 minutes, 20 minutes.
- Testing window: 11 days.
- Fixed test order:
  - Week 1 Monday: 45-second maximal effort
  - Week 1 Wednesday: 3-minute maximal effort
  - Week 1 Friday: 6-minute maximal effort
  - Week 2 Monday: 10-minute maximal effort
  - Week 2 Thursday: 20-minute maximal effort
- Protocol logic:
  - tests move from shortest to longest
  - recovery is built in between the middle and longer efforts
  - the weekend acts as a recovery bridge
  - the 20-minute test is last because it is the most pacing-sensitive and most likely to be affected by residual fatigue
- Primary displayed model: broad descriptive power-duration curve using a power-law fit.
- Secondary metrics: estimated critical power and estimated W′ only.
- Interpretation rule: the broad power-duration profile is the primary product; CP and W′ must remain secondary.
- Version 1 build structure: single `app.R` file.
- Version 1 storage model: in-session history plus CSV import/export.
- Default app tone: plain English, practical, conservative, and learning-friendly.

## Version 1 Required Features

- Manual input for athlete name, test date, and five average-power values.
- Current profile visualization showing measured test points and fitted curve.
- Historical storage of repeated testing blocks.
- Historical comparison across testing dates.
- Practical training targets anchored to tested performance and model outputs.
- User-facing protocol page explaining the testing window and test execution.
- Basic but meaningful validation, warnings, and retest guidance.
- CSV import and export for saved history.
- Readable Shiny code with comments and clear sectioning.
- Ability to preserve raw test values and compare them over time.

## Version 1 Exclusions

- Heart rate inputs or heart-rate-driven logic.
- Body composition metrics.
- Direct claims to measure lactate threshold, ventilatory thresholds, VO2max, or exact metabolic zones.
- Adaptive training plans or automated programming engines.
- Wearable integrations or live ergometer integrations.
- Cloud accounts, authentication, or database storage.
- Multi-user coach dashboards.
- Machine learning features.
- Readiness, recovery, or nutrition scoring.
- Arbitrary user-defined test durations as the default workflow.
- Mobile app packaging.
- Social or gamified features.
- Any output that implies medical, diagnostic, or lab-grade validity.

## Scientific Guardrails

### Safe claims
- The app estimates a user's power-duration relationship from five field tests.
- The app provides a practical performance profile across fixed durations.
- The app tracks changes in field-tested performance over time.
- The app estimates practical training targets from measured performance.
- The app includes estimated critical power and W′ as secondary metrics.

### Unsafe claims
- The app directly measures lactate threshold.
- The app directly measures ventilatory thresholds.
- The app determines exact metabolic zones.
- The app diagnoses physiology.
- The app replaces laboratory testing.
- The app produces exact individualized prescriptions.
- The app reveals true biological breakpoints or exact internal energy-system behavior.

### Approved terminology
- estimate
- estimated
- profile
- power-duration profile
- field-tested
- field-based assessment
- practical training targets
- practical training anchors
- model-derived
- current profile
- historical trend
- secondary metric
- estimated critical power

### Avoided terminology
- exact threshold
- direct measurement
- proven zone
- validated Zone 2 from this test alone
- metabolic truth
- personalized physiology engine
- scientifically determines
- this is your true critical power
- this tells you exactly how to train

### Uncertainty language
- This app estimates your power-duration profile from field testing.
- Training targets are practical estimates, not direct laboratory measures.
- Critical power is included as a secondary estimated metric.
- Modeled values should be interpreted alongside your actual tested results.
- Use these results to guide training, not to infer exact physiology.

## App Pages / Tabs

### 1. Current Test Entry
Must contain:
- athlete name
- test date
- optional session notes
- five wattage inputs
- validation or warning messages
- save current test action
- main power-duration plot
- concise summary outputs

Must avoid:
- dense secondary charts
- long physiological explanations
- clutter that hides the main test-entry workflow

### 2. Historical Comparison
Must contain:
- saved test table
- simple trend views across test dates
- current vs previous comparison logic
- absolute and percentage change by duration
- profile-shape comparison where feasible

Must avoid:
- overly dense multi-layer charts
- comparison views that require expert interpretation

### 3. Training Targets
Must contain:
- practical target categories
- predicted powers at selected durations
- short explanation of how to use the targets
- clear uncertainty/disclaimer text

Must avoid:
- hard physiological zone claims
- excessive jargon
- precision that the data do not support

### 4. Testing Protocol
Must contain:
- 11-day schedule
- warm-up guidance
- pacing guidance
- recovery guidance
- retest guidance
- validity rules
- standardization guidance

Must avoid:
- decorative copy without operational instruction
- vague advice that leaves test execution ambiguous

### 5. Import / Export
Must contain:
- CSV upload
- CSV download
- simple explanation of what import/export does

Must avoid:
- complex file-management workflows
- hidden assumptions about data replacement or merge logic

## Required Inputs

- `athlete_name` — text — identifies the athlete or profile owner.
- `test_date` — date — anchors the testing block in history.
- `power_45s` — numeric — average power for the 45-second effort.
- `power_3min` — numeric — average power for the 3-minute effort.
- `power_6min` — numeric — average power for the 6-minute effort.
- `power_10min` — numeric — average power for the 10-minute effort.
- `power_20min` — numeric — average power for the 20-minute effort.

## Optional Inputs

- `notes` — text — context on test quality or unusual conditions.
- `ergometer_type` — text or select input — identifies machine type when useful.
- `protocol_confirmed` — logical — indicates whether the user followed the testing protocol.
- `test_validity_flag` — text or select input — allows marking a session as complete, compromised, or retest-needed.

## Required Outputs

### Current-profile outputs
- plot of the five measured test points
- fitted power-duration curve
- summary table of entered powers
- summary table of key estimated metrics
- concise confidence or warning messages

### Historical outputs
- saved-history table
- trend chart for the five fixed durations
- trend display for estimated critical power
- current vs prior comparison view
- absolute and percentage change by duration where feasible

### Training outputs
- practical target categories
- predicted powers for selected durations
- short explanation of how to apply them
- uncertainty statement

### Protocol outputs
- clear user-facing instructions for completing the testing window correctly

## Training Target Rules

- Training outputs must be framed as practical guidance, not exact physiology.
- Use a hybrid structure:
  - duration-based targets as the primary framing
  - workout categories as the organizing labels
- Do not make numbered zones the main product.
- Keep targets anchored to tested performance and model estimates.
- Present targets as working anchors or ranges, not exact truths.

### Required target categories
- short-power repeats
- anaerobic-capacity intervals
- VO2-type intervals
- threshold-focused work
- steady aerobic work

### Recommended predicted durations
- 1 minute
- 2 minutes
- 4 minutes
- 8 minutes
- 12 minutes
- 15 minutes
- 30 minutes
- 60 minutes

### Training-target phrasing rules
- Say what the target is useful for.
- Say that it is estimated.
- Do not imply direct threshold determination.
- Do not imply that one model output fully explains training needs.

## Validation and Warning Rules

### Hard-stop rules
Block calculation or saving when:
- any required value is missing
- any required value is non-numeric
- any required value is zero or negative
- mixed or unclear units are entered
- duplicate duration values are being overwritten without explicit confirmation
- descending duration order is broken:
  - 45 s must be greater than 3 min
  - 3 min must be greater than 6 min
  - 6 min must be greater than 10 min
  - 10 min must be greater than 20 min
- one value is grossly out of scale in a way that strongly suggests a typo

### Soft-warning rules
Warn but allow continuation when:
- the drop between adjacent durations is unusually small
- the drop between adjacent durations is unusually large
- one result creates a visible curve kink
- a result differs substantially from recent history
- the user reports poor sleep, illness, soreness, disrupted warm-up, pacing error, or equipment problems
- the profile is incomplete or flagged as lower-confidence

### Pattern-based suspicion rules
Flag likely:
- pacing error
- fatigue distortion
- non-maximal effort
- data-entry mistakes

### Retest guidance levels
- Retest recommended
- Retest suggested
- Keep, but interpret cautiously

### Warning-system rules
- Warnings must be actionable.
- The app must not pretend to know the exact cause of a suspicious result.
- The app may say the pattern is unusual and confidence is reduced.
- The app must never silently smooth away contradictions in the raw data.

## Data Fields and CSV Rules

### Core record type
Version 1 should store one record per completed testing block.

### Required stored fields
- `athlete_name`
- `test_date`
- `power_45s`
- `power_3min`
- `power_6min`
- `power_10min`
- `power_20min`

### Optional stored fields
- `notes`
- `ergometer_type`
- `protocol_confirmed`
- `test_validity_flag`

### Derived stored fields
- `estimated_cp`
- `estimated_w_prime`
- `power_law_a`
- `power_law_b`
- `predicted_power_1min`
- `predicted_power_5min`
- `predicted_power_15min`
- `predicted_power_30min`
- `predicted_power_60min`
- `warning_flags`
- `confidence_summary`

### CSV rules
Recommended CSV columns:
- `athlete_name`
- `test_date`
- `power_45s`
- `power_3min`
- `power_6min`
- `power_10min`
- `power_20min`
- `estimated_cp`
- `estimated_w_prime`
- `power_law_a`
- `power_law_b`
- `notes`

Implementation rules:
- preserve raw entered values
- do not overwrite raw data with derived values
- keep observed and modeled values separate
- use simple, stable snake_case names
- keep import/export simple
- in version 1, importing a CSV should replace in-session history unless explicitly changed later

## Shiny Architecture Rules

### App structure
Build version 1 as a single `app.R` file with these sections:
1. package imports
2. global constants and settings
3. helper functions
4. UI definition
5. server definition
6. app launch

### UI structure
Use a top-level `navbarPage()` layout with these tabs:
- Current Test Entry
- Historical Comparison
- Training Targets
- Testing Protocol
- Import / Export

### Server responsibility areas
- input capture and validation
- profile estimation and calculations
- output rendering
- history management
- user messaging and safeguards

### Core reactive objects
- `current_inputs()`
- `validated_tests()`
- `current_profile()`
- `profile_curve_data()`
- `current_summary_metrics()`
- `training_targets()`
- `history_data()`
- `selected_history_profiles()`
- `comparison_data()`

### Required helper functions
- `build_test_dataframe()`
- `validate_test_values()`
- `fit_power_duration_model()`
- `predict_power_curve()`
- `estimate_secondary_metrics()`
- `build_training_targets()`
- `make_profile_record()`
- `append_history_record()`
- `validate_history_file()`
- `read_history_csv()`
- `write_history_csv()`
- `format_duration_label()`

### Code rules
- Keep business logic out of the UI where possible.
- Keep calculations and validation in reusable functions.
- Name things for readability, not brevity.
- Every derived output must be traceable in code.
- Fail safely with clear fallback messages.
- Do not hard-code hidden assumptions.
- Use reactive logic intentionally and sparingly.
- Prefer base R or widely understood packages unless a dependency clearly adds value.
- Comment decisions and rules, not obvious syntax.
- Make version 1 stable before making it clever.

## Open Questions

- Exact summary metrics to show by default on the current-profile page.
- Exact thresholds for soft-warning logic, especially percent-drop and history-mismatch cutoffs.
- Exact layout of comparison views on the Historical Comparison tab.
- Exact presentation format for training targets: table only, cards, or simple grouped sections.
- Whether CSV history should be stored in wide format, long format, or exported in one primary format plus one optional advanced format.
- Whether version 1 should allow editing saved history rows directly in-app or only through overwrite / re-save flows.

## Immediate Use Rule

Use this file as the source of truth for version 1.

Do not revise the app further from memory. Compare the MVP directly against this spec and mark each item as:
- matches
- missing
- violates
- defer to v2
