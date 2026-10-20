# Changelog

User-facing changes to the Power-Duration App.

## Unreleased

### Added

- A status bar on Current Test Entry shows how many of the 5 results are entered and what to do next.
- The Testing Protocol page explains how to record, save, and keep your results, and what the results do and don't measure.
- The Import / Export page lists the required CSV columns.

### Changed

- The app opens on Current Test Entry. Tabs are now: Current Test Entry, Historical Comparison, Training Targets, Testing Protocol, Import / Export.
- The curve beyond 20 minutes is dashed over a shaded band, since your longest test is 20 minutes. The 30- and 60-minute predictions are labeled as extrapolated.
- Training targets show each anchor duration as its own value, such as "4 min: 352 W," instead of a combined range.
- The predicted-power table moved to Training Targets. It shows whole watts and labels each value as modeled or extrapolated.
- History CSV files now include `power_law_a` and `power_law_b` instead of `riegel_S` and `riegel_E`. Files exported from version 1.0 can still be uploaded.
- The date field is now labeled "Testing block start date," meaning the day of your 45-second test. The Testing Protocol presents the weekdays as a suggested cadence and gives the spacing to keep if your schedule differs.

### Removed

- "Profile Power Level (Riegel S)" and "Power-Duration Endurance (Riegel E)." These were model parameters presented as performance scores, which the app's methods don't support.
- The Predicted Power by Duration tab. The app now predicts only the 8 fixed durations shown on Training Targets.
- The How to Use This App tab. Its guidance now appears on the pages where it applies.

### Fixed

- Clearing the date field no longer breaks the entry page. The app now asks for the start date instead.

## 1.0.0 (2026-05-03)

Initial public release.