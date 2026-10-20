# Power-Duration App

A Shiny app that estimates a practical power-duration profile from 5 maximal field tests on an ergometer.

**Use the app:** https://benskutnik-power-duration-app.share.connect.posit.cloud

## Who it's for

Home-gym athletes and coaches who can read average watts from an ergometer and want a repeatable way to track performance across short and long efforts.

## The tests

5 maximal average-power efforts, shortest to longest, over about 11 days:

| Suggested day | Test |
| ----- | ----- |
| Week 1, Monday | 45 seconds |
| Week 1, Wednesday | 3 minutes |
| Week 1, Friday | 6 minutes |
| Week 2, Monday | 10 minutes |
| Week 2, Thursday | 20 minutes |

The weekdays are a suggested cadence. The spacing allows recovery between efforts, and the 20-minute test comes last because it's the most sensitive to pacing and fatigue. The app's Testing Protocol tab covers warm-up, pacing, and when to delay or repeat a test.

## What the app shows

- Your 5 tested results and a fitted power-duration curve. Tested points and modeled values are shown separately, and the curve beyond 20 minutes is marked as extrapolated.
- Estimated critical power (CP) and W′ as secondary, model-derived metrics.
- Predicted power at 8 fixed durations, from 1 to 60 minutes.
- Practical training targets for 5 workout categories, each tied to specific durations.
- Comparisons with your earlier testing blocks.
- Data-quality checks that flag entry errors and unusual patterns, with guidance on whether to retest.

## What it doesn't measure

The app estimates performance from field tests. It does not measure lactate threshold, ventilatory thresholds, VO2max, or metabolic zones, and it doesn't replace laboratory testing. Training targets are starting references to adjust based on how your workouts go.

## Your data

The app stores nothing between sessions. Download your history CSV from the Import / Export tab before closing, then upload it next time to pick up where you left off.

## Run it locally

1. Install R and these packages: `shiny`, `ggplot2`, `dplyr`, `readr`, `scales`, `tidyr`, `DT`.
2. Open this folder in Positron or RStudio.
3. In the console, run `shiny::runApp()`.

## Project documents

- `CHANGELOG.md`: changes users will notice, by version.
- `docs/`: the planning documents and the scientific bibliography.
- `docs/DECISIONS.md`: design decisions and the reasoning behind them.
- `docs/RELEASING.md`: how changes get from a branch to the live app.
- `tests/test_app.R`: automated checks to run before each release.

## Status

Version 1.1 is live. See `CHANGELOG.md` for what changed.

## License

Copyright © 2026 Ben Skutnik. All rights reserved. No license is granted to copy, modify, or redistribute this code.