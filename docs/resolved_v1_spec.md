# **Power Duration Shiny — Resolved Version 1 Specification**

**Status:** Authoritative decisions resolving conflicts between `10_master_build_spec.md`, the earlier project memos, and the current prototype.

Where this document explicitly resolves a conflict, these decisions supersede the conflicting text in the existing documents.

## **1\. Primary model and outputs**

The primary output remains a descriptive power-duration profile based on five field tests: 45 seconds, 3 minutes, 6 minutes, 10 minutes, and 20 minutes.

The primary model is a log-log linear fit:

`log(power) = alpha + beta * log(duration_seconds)`

Equivalently:

`predicted_power = A * duration_seconds^B`

where `A = exp(alpha)` and `B = beta`.

The app will display the five observed test results and the fitted curve. Observed results must be visually distinguishable from model estimates.

The curve may extend to 60 minutes, but the portion beyond 20 minutes must be visually distinguished as extrapolation. The 30- and 60-minute estimates must be explicitly labeled as extrapolated estimates.

### **Remove Riegel S/E from Version 1**

Remove `riegel_S` and `riegel_E` from the user interface, historical comparisons, saved records, and CSV schema.

The current prototype’s labels “Profile Power Level (Riegel S)” and “Power-Duration Endurance (Riegel E)” are not sufficiently justified for this app’s specific model. They also conflict with the master spec’s decision that estimated CP and W′ are the only secondary metrics.

Retain `power_law_a` and `power_law_b` as internal model parameters if useful. Define them consistently as the multiplicative coefficient `A` and exponent `B`. Do not present them as standalone performance scores.

### **Critical power and W′**

Estimate CP and W′ from a linear work-versus-time fit using the 3-, 6-, 10-, and 20-minute tests. The 45-second test is excluded.

Both remain secondary, model-derived estimates. If either estimate is non-finite or non-positive, report both as unavailable rather than displaying a potentially misleading partial pair. The primary power-duration profile may still be shown if its input data pass validation.

The app must not imply that this field-test procedure is equivalent to a validated laboratory protocol or the 3-minute all-out cycling test.

## **2\. Fixed prediction durations**

Use exactly these eight prediction durations throughout the interface, model outputs, saved derived fields, and CSV export:

1. 1 minute  
2. 2 minutes  
3. 4 minutes  
4. 8 minutes  
5. 12 minutes  
6. 15 minutes  
7. 30 minutes  
8. 60 minutes

Remove the stale `predicted_power_5min` field from the master spec and implementation. Do not add a 5-minute prediction to Version 1\.

The app must not offer arbitrary user-defined duration predictions. Remove the separate “Predicted Power by Duration” tab and place the fixed-duration prediction table on the Training Targets page.

## **3\. Training targets**

Use the five categories already specified in the master build spec:

| Category | Duration anchors |
| ----- | ----- |
| Short-power repeats | 1 and 2 minutes |
| Anaerobic-capacity intervals | 2 and 4 minutes |
| VO₂-type intervals | 4 and 8 minutes |
| Threshold-focused work | 12 and 15 minutes |
| Steady aerobic work | 30 and 60 minutes |

Display the predicted wattage for each anchor duration separately. Do not combine predictions from different durations into a single wattage range without explaining that the endpoints refer to different effort durations.

The 30- and 60-minute values are extrapolations from tests that end at 20 minutes. They must be identified as lower-confidence planning references, not validated steady-aerobic training prescriptions.

Training categories are practical organizing labels, not diagnoses of physiological systems or thresholds. Targets are starting references that users should adjust based on repeatability, workout response, and coaching judgment.

The earlier `06_training_targets.md` category names and expanded duration list are superseded for Version 1\.

## **4\. Navigation and page structure**

Use exactly the five tabs specified in the master build spec:

1. Current Test Entry  
2. Historical Comparison  
3. Training Targets  
4. Testing Protocol  
5. Import / Export

The “How to Use This App” content should be condensed into brief contextual instructions on the entry and import/export pages, with the full testing workflow explained on the Testing Protocol page.

The separate custom-duration prediction page should be removed. The fixed prediction table belongs on Training Targets.

The earlier information-architecture proposal for Overview, Enter Test Results, Profile & Targets, History, and Testing Protocol is superseded. Completion status and the next action should be visible on Current Test Entry rather than requiring a separate Overview tab.

## **5\. Data model and date semantics**

Use one wide-format row per testing block. This is the authoritative Version 1 storage and CSV format. Data may be reshaped into long format temporarily for plotting and modeling, but a four-table normalized database is out of scope.

### **Required fields**

* `athlete_name`  
* `test_date`  
* `power_45s`  
* `power_3min`  
* `power_6min`  
* `power_10min`  
* `power_20min`

### **Optional fields**

* `notes`  
* `ergometer_type`  
* `protocol_confirmed`  
* `test_validity_flag`  
* `context_flags`

`context_flags` is added so the app can record structured reports of poor sleep, illness, unusual soreness, hard training between tests, interrupted efforts or warm-ups, pacing problems, and equipment or setup issues. A single optional multi-select control is sufficient; this does not require a complex questionnaire.

### **Definition of `test_date`**

`test_date` means the **start date of the testing block**, which is the first Monday in the prescribed schedule. Label the input “Testing block start date (first Monday).”

The five scheduled test dates are offsets of 0, 2, 4, 7, and 10 calendar days from this date.

Version 1 will not store separate dates for each test. The app therefore cannot independently verify the actual date of each effort; protocol confirmation and notes provide the available context.

### **Explicit exclusions**

Do not add body mass, W/kg, heart rate, RPE, cadence, individual test dates, athlete IDs, or a normalized multi-table database to Version 1\. These appear in earlier planning documents but are not part of the controlling scope.

## **6\. Saving, editing, and duplicate handling**

Treat the combination of trimmed `athlete_name` and `test_date` as the unique profile key.

* A new athlete/block-start-date combination creates a new record.  
* Saving a record with an existing key requires an explicit replace-or-cancel confirmation.  
* The user must be able to select a historical record, load it into the entry form, edit it, and save it as a replacement.  
* Do not silently append a second record with the same key.  
* Do not allow duplicate athlete/block-start-date keys in an uploaded CSV. Reject the upload with a clear explanation so the user can correct it.

This resolves the master spec’s open question about editing and its vague reference to duplicate duration values. In a wide-format record, each duration has its own field; the meaningful duplicate check is for a duplicate testing-block record.

## **7\. Validation and compromised sessions**

The app must distinguish invalid input from a complete but suspicious or compromised test set.

### **Hard input errors**

Block calculation and saving until corrected when:

* The athlete name or block start date is missing.  
* Any of the five wattage values is missing, non-numeric, non-finite, zero, or negative.  
* Units are unclear or inconsistent.  
* A value is grossly out of scale and strongly suggests a data-entry error.

For the gross-outlier check, flag a value that is more than twice, or less than half, its immediate neighboring test value or values. For an interior point, compare against both neighbors; for an endpoint, compare against its sole neighbor. Treat this as a conservative data-entry safeguard, not proof that the performance is physiologically impossible.

### **Descending-order violation**

A clean profile requires:

`45 s > 3 min > 6 min > 10 min > 20 min`

If this ordering fails, block model calculation and normal profile saving. The user must either correct the entry or explicitly save the five raw values as a compromised record.

A compromised raw record may be retained for review, but it must not receive a fitted profile, CP/W′ estimates, predicted powers, or training targets. The app must not silently smooth, reorder, or correct the raw values.

### **User-marked compromised sessions**

The `test_validity_flag` choices are:

* Complete  
* Compromised  
* Retest needed

“Complete” means the five required efforts have been recorded; it does not guarantee that they were maximal or well executed.

If the user marks a session “Compromised” or “Retest needed,” allow the raw record to be retained when the input values themselves are valid, but suppress model-derived outputs. Explain that a replacement testing block is needed for a normal profile.

Do not introduce a partial-profile mode in Version 1\. All five valid numeric inputs are required.

## **8\. Soft-warning thresholds**

The following are operational Version 1 heuristics. They are not validated universal physiological cutoffs and should be reviewed after the app is tested with real users.

### **Near-flat decline**

Warn when the decline between any adjacent pair of tests is 2% or less. Calculate decline as:

`1 - (longer_duration_power / shorter_duration_power)`

### **Very steep decline**

Warn when the decline exceeds:

* 30% between 45 seconds and 3 minutes.  
* 20% between 3 and 6 minutes, 6 and 10 minutes, or 10 and 20 minutes.

These duration-specific thresholds avoid applying the same rule indiscriminately to the very different 45-second-to-3-minute interval and the longer-duration intervals.

### **Curve-shape warning**

After fitting the power-law model, warn if any observed test point differs from the model-predicted power at that duration by more than 10% in relative terms. This is a heuristic for an unusual profile shape, not a validity test.

### **Historical mismatch**

Warn if any duration differs by more than 8% from the most recent comparable profile for the same athlete.

A comparison is considered equipment-comparable when both records identify the same ergometer type, or both omit it. If the equipment types differ, do not issue an automated historical-mismatch warning based on wattage alone; instead, disclose the equipment difference.

### **Context and protocol warnings**

Warn when the user reports any of the structured conditions in `context_flags`, or has not confirmed protocol adherence. Set `protocol_confirmed` to unchecked by default so the user must affirm it.

A soft warning does not automatically invalidate the test. The app should explain what looks unusual, what to review, and whether a retest is suggested.

## **9\. Retest guidance**

Use the following hierarchy:

* **Retest recommended:** A hard input problem, a rank-order violation, a gross suspected entry error, or a user-reported failed/interrupted effort. Do not show model outputs when the profile is blocked or explicitly marked compromised.  
* **Retest suggested:** A very steep drop, a curve-shape warning, a historical mismatch, or a reported condition likely to have materially affected performance.  
* **Keep, but interpret cautiously:** Only minor warnings are present, such as a near-flat decline or unconfirmed protocol adherence.  
* **No retest signal:** No warnings were triggered.

The app must not claim certainty about fatigue, pacing, motivation, effort maximality, or the cause of a performance change. Warnings describe patterns and reduce confidence; they do not diagnose causes.

The earlier prototype’s `0.65` adjacent ratio, `25%` history-mismatch threshold, `0.35` 20-minute/45-second ratio, and `2.2` 45-second/3-minute ratio are superseded by the rules above.

## **10\. Derived fields and CSV behavior**

The derived fields should include:

* `estimated_cp`  
* `estimated_w_prime`  
* `power_law_a`  
* `power_law_b`  
* `predicted_power_1min`  
* `predicted_power_2min`  
* `predicted_power_4min`  
* `predicted_power_8min`  
* `predicted_power_12min`  
* `predicted_power_15min`  
* `predicted_power_30min`  
* `predicted_power_60min`  
* `warning_flags`  
* `confidence_summary`  
* `retest_level`

Remove `riegel_S`, `riegel_E`, and `predicted_power_5min` from the schema.

CSV export should include the raw required fields, optional metadata, and derived fields. On import, the raw inputs and supported metadata are authoritative. Recompute all derived values rather than trusting calculations stored in the file.

Uploading a CSV replaces the current in-session history. Validate the entire file before replacing history; if required columns or unique profile keys are invalid, reject the upload and leave the existing history unchanged.

The earlier four-table schema and long-format internal storage proposal are superseded for Version 1\.

## **11\. Implementation priorities**

Update the prototype in this order:

1. Remove Riegel S/E calculations and displays.  
2. Align the prediction fields with the eight fixed durations.  
3. Remove the custom-duration page and use the five master-spec tabs.  
4. Define and relabel `test_date` as the testing block start date.  
5. Implement duplicate detection, explicit replacement, and loading a saved profile for editing.  
6. Implement the resolved hard-stop, compromised-save, and soft-warning behavior.  
7. Add structured context flags and make protocol confirmation unchecked by default.  
8. Align CSV import/export with the wide-format schema and recomputation rules.  
9. Test valid profiles, ordering failures, gross outliers, soft warnings, compromised sessions, duplicate records, and CSV round trips.

## **Governing principle**

The five observed power results remain the foundation. The power-law curve, CP/W′ estimates, predictions, and training targets are interpretations layered on top of those results. The app must preserve raw data, distinguish observation from estimation, and withhold model outputs when the data do not support a normal profile.

