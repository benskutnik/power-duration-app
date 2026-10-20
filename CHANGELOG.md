# Changelog

User-facing changes to the Power-Duration App.

## Unreleased

### Added

- A status bar on Current Test Entry shows how many of the 5 results are entered and what to do next.
- The Testing Protocol page explains how to record, save, and keep your results, and what the results do and don't measure.
- The Import / Export page lists the required CSV columns.
- Saving a test with the same athlete and start date as a saved test asks whether to replace it. Duplicates are no longer created.
- Saved tests can be loaded back into the entry form for editing from the Historical Comparison tab.
- Results with an ordering problem or a likely entry error, and sessions marked Compromised or Retest needed, show the entered values without a fitted curve, CP/W′, predictions, or targets. They can be saved as compromised records for review.
- A 20-minute value above 600 W is flagged as likely not watts, such as a calories reading.
- Historical Comparison overlays the current profile's curve on the previous comparable test's curve.
- A checklist on Current Test Entry records conditions that may have affected the testing block, such as illness or poor sleep. Each one raises the retest guidance, and an interrupted or failed effort withholds model outputs.
- Ergometer is a dropdown of common machines, plus Not specified and Other.
- Uploads accept dates written as 2026-03-02 or 3/2/2026.
- Uploaded rows with ordering problems or likely entry errors are imported as compromised records.

### Changed

- The app opens on Current Test Entry. Tabs are now: Current Test Entry, Historical Comparison, Training Targets, Testing Protocol, Import / Export.
- The curve beyond 20 minutes is dashed over a shaded band, since your longest test is 20 minutes. The 30- and 60-minute predictions are labeled as extrapolated.
- Training targets show each anchor duration as its own value, such as "4 min: 352 W," instead of a combined range.
- The predicted-power table moved to Training Targets. It shows whole watts and labels each value as modeled or extrapolated.
- History CSV files now include `power_law_a` and `power_law_b` instead of `riegel_S` and `riegel_E`. Files exported from version 1.0 can still be uploaded.
- The date field is now labeled "Testing block start date," meaning the day of your 45-second test. The Testing Protocol presents the weekdays as a suggested cadence and gives the spacing to keep if your schedule differs.
- Athlete names match regardless of capitalization or extra spaces, so "Sam Smith" and "sam  smith" are the same athlete.
- After any save, replacement, or deletion, the quality checks on every saved test are recalculated.
- Data-quality checks use new thresholds based on simulated profiles. Typical profiles pass without warnings, and each warning names the results involved.
- Retest guidance reflects the most serious issue found, instead of the number of warnings.
- History checks and comparisons use the most recent earlier test with a normal profile on the same equipment. A test on different equipment is noted instead of compared by wattage.
- The history table shows each test's validity flag and retest guidance.
- Estimated CP and W′ appear together or not at all.
- The "I followed the testing protocol closely" box starts unchecked, so confirming it is a deliberate choice.
- History CSV files include `context_flags` and `retest_level` columns.
- Uploads check every row before replacing history. A file with missing or invalid values, or a repeated athlete and start date, is rejected with a message naming the rows, and the current history stays as it was.
- Equipment names in uploaded files are matched to the app's list regardless of capitalization. Unrecognized names are saved as Other.
- Uploaded rows without a `protocol_confirmed` value are treated as unconfirmed.
- Downloaded history files leave empty cells blank instead of writing "NA."
- The example CSV includes every editable column.

### Removed

- "Profile Power Level (Riegel S)" and "Power-Duration Endurance (Riegel E)." These were model parameters presented as performance scores, which the app's methods don't support.
- The Predicted Power by Duration tab. The app now predicts only the 8 fixed durations shown on Training Targets.
- The How to Use This App tab. Its guidance now appears on the pages where it applies.

### Fixed

- Clearing the date field no longer breaks the entry page. The app now asks for the start date instead.

## 1.0.0 (2026-05-03)

Initial public release.