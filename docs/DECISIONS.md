# Decision Log

Each entry records what was decided, why, and what it replaced. Where entries conflict with the planning documents, these entries control.

**Source-of-truth order:** this log, then `resolved_v1_spec.md`, then `10_master_build_spec.md`, then documents 01 to 08 and the blueprint.

**Status values:** Decided (approved, not yet built), Implemented (built on branch `v1-resolved-spec`), Review scheduled (decided, with a planned recheck).

---

## D-001: Remove Riegel S/E

**Date:** 2026-10-19 | **Status:** Implemented (Step 1)

Removed `riegel_S` and `riegel_E` from the interface, comparisons, history table, and CSV schema. Replaced them in saved data with `power_law_a` (A = exp(alpha)) and `power_law_b` (B = beta) from the fit log(power) = alpha + beta × log(duration). These are stored as model parameters and not displayed.

**Why:** the app presented S and E as performance scores ("Profile Power Level," "Power-Duration Endurance"). The project's literature does not validate those interpretations for a power-versus-duration model. Riegel's 1981 model relates time to distance, so the label was also a loose fit. `power_law_a` equals the old S; `power_law_b` equals the old E minus 1.

## D-002: Mark extrapolation

**Date:** 2026-10-19 | **Status:** Implemented (Step 2)

The curve is solid from 45 s to 20 min and dashed beyond 20 min over a shaded band labeled "Extrapolated beyond 20 min." The prediction table labels 1 to 15 min as "Modeled estimate" and 30 and 60 min as "Extrapolated estimate." The curve grid includes the 5 test durations so the style changes at exactly 20 min. Predicted watts display as whole numbers.

**Why:** Resolved spec §1. The longest test is 20 min.

## D-003: Training targets per anchor

**Date:** 2026-10-19 | **Status:** Implemented (Step 2)

Each category shows its 2 anchor durations as separate values (for example, "4 min: 352 W") instead of a combined range. The 30 and 60 min anchors carry "(extrapolated)," and the steady-aerobic note calls them lower-confidence planning references.

**Why:** Resolved spec §3. A combined range hides that its endpoints refer to different durations.

## D-004: Five tabs

**Date:** 2026-10-19 | **Status:** Implemented (Step 3)

Tabs, in order: Current Test Entry, Historical Comparison, Training Targets, Testing Protocol, Import / Export. Removed the custom-duration page and the "How to Use This App" page. Moved the prediction table to Training Targets. Added a completion status bar and next action to Current Test Entry. Moved the useful "How to Use" content to the entry sidebar, the Testing Protocol page, and Import / Export.

**Why:** Resolved spec §2 and §4.

## D-005: Gross-outlier check

**Date:** 2026-10-19 | **Status:** Implemented (Step 6)

Implemented as an adjacent-pair ratio check (shorter-duration power ÷ longer-duration power). A single pair cannot show which value is the typo, so the message names both.

| Pair | Suspected entry error above |
| ----- | ----- |
| 45 s ÷ 3 min | 3.0 |
| 3 ÷ 6 min, 6 ÷ 10 min, 10 ÷ 20 min | 2.0 |

A suspected entry error withholds model outputs. The user corrects the entry or saves the raw values as a compromised record.

**Why:** the resolved spec's 2.0 limit on the 45 s ÷ 3 min pair could block real sprint-leaning athletes, with no way to save their data. Simulation (see D-007) put typical athletes at 1.3 to 2.0 and extreme sprint profiles at 2.0 to 2.4. The mathematical ceiling is 4.0. Common typos give 4 or more, or break the descending order.

**Replaces:** the "compare against both neighbors" wording in resolved spec §7.

## D-006: Hard input errors versus pattern blocks

**Date:** 2026-10-19 | **Status:** Implemented (Step 6)

| Type | Triggers | Saving |
| ----- | ----- | ----- |
| Hard input error | Missing name or date; a watt value missing, non-numeric, zero, or negative; 20 min power above 600 W | Blocked until corrected |
| Pattern block | Descending-order violation; suspected entry error (D-005) | Only as a compromised record |

**600 W cap:** users don't select units, so the "units unclear" rule has to check values. Consoles also show calories per hour and pace. A Concept2 monitor shows about 1,330 cal/hr at 300 W, and an all-calories entry would pass every other check. A 20 min value above 600 W exceeds world-class 20 min power, so it most likely isn't watts. Input labels also say to use the watts reading.

## D-007: Soft-warning thresholds

**Date:** 2026-10-19 | **Status:** Implemented (Step 6); Review scheduled

| Check | Rule | Retest level |
| ----- | ----- | ----- |
| Near-flat decline | Any adjacent decline ≤ 1% | Keep, but interpret cautiously |
| Steep 45 s to 3 min | Ratio above 2.0, up to 3.0 | Keep, but interpret cautiously |
| Steep 3 to 6 min | Decline above 25% | Retest suggested |
| Steep 6 to 10 min, 10 to 20 min | Decline above 20% | Retest suggested |
| Curve shape | 3, 6, or 10 min value more than 4% from a quadratic fit (below) | Retest suggested |
| Historical mismatch | Any duration more than 8% from the baseline (D-010) | Retest suggested |

**Curve-shape check:** fit a quadratic of log(power) on log(duration) to the 5 values and check only the 3 interior points. This is a quality-control tool only. The displayed curve, predictions, and targets still come from the power-law fit.

**Why:** in simulation with 2% day-to-day variation (method below):

| Rule | Fires on honest profiles |
| ----- | ----- |
| Old 30% / 20% steep-decline rules | About 80% |
| New steep-decline rules | 1% to 5% |
| Old 10% power-law curve-shape rule | About 31% |
| New quadratic curve-shape rule | About 3% |
| Old 2% near-flat rule | 14% to 20% |
| New 1% near-flat rule | 6% to 9% |

The new curve-shape rule caught a 10% error at an interior point about 84% of the time. The old rule caught a 10% upward error about 21% of the time. A power-law fit is a straight line in log-log space, and real data across 45 s to 20 min bends, so the old rule flagged smooth profiles.

**Replaces:** resolved spec §8 thresholds (2% near-flat, 30% and 20% steep, 10% curve shape).

**Simulation method:** 174 profiles from a 3-parameter critical-power model (CP 180 to 340 W, W′ 10 to 35 kJ, Pmax 700 to 2,200 W), with 2% to 3% random error added per test. That error level is an assumption, not a measured value. All rates above are estimates until the review below replaces them with real data.

**Review plan:** the app collects no usage data, so the review uses testing blocks gathered deliberately.

- **Trigger:** at least 30 real testing blocks, from the developer, coached athletes, or beta testers who share exported CSVs.
- **Measure:** how often each warning fires on blocks the tester considers good.
- **Target:** no single soft warning fires on more than 10% of good blocks. Adjust any that do.
- **Variation check:** when an athlete repeats a test within a week, compare the 2 results to measure the day-to-day variation the simulation assumed.

## D-008: Context flags

**Date:** 2026-10-19 | **Status:** Implemented (Step 7)

`context_flags` is a multi-select stored as one text field, separated by "; ". The combined "interrupted efforts or warm-ups" flag is split in two.

| Flag | Retest level |
| ----- | ----- |
| Interrupted or failed effort | Retest recommended |
| Illness | Retest suggested |
| Hard training between tests | Retest suggested |
| Pacing problems | Retest suggested |
| Equipment or setup issues | Retest suggested |
| Poor sleep | Keep, but interpret cautiously |
| Unusual soreness | Keep, but interpret cautiously |
| Disrupted warm-up | Keep, but interpret cautiously |

An interrupted or failed effort withholds model outputs, the same as a session marked "Compromised." The message says: "If you repeated that effort and entered the new result, uncheck this flag."

**Why:** the levels follow how directly each condition affects a maximal effort. An interrupted effort doesn't represent a full maximal effort. Illness, hard training, pacing problems, and equipment issues can each shift a result by more than normal variation. Poor sleep, soreness, and a disrupted warm-up matter, but the effort was still complete.

## D-009: Retest level and confidence summary

**Date:** 2026-10-19 | **Status:** Implemented (Step 6)

`retest_level` is the highest level any rule triggers. The prototype's "2 or more warnings = Retest suggested" rule is removed.

Retest recommended always means model outputs are withheld. It applies to hard input errors, pattern blocks, an interrupted or failed effort, and sessions the user marks "Compromised" or "Retest needed." Raw values are kept in every case except hard input errors, which can't be saved.

`confidence_summary` is a sentence tied one-to-one to `retest_level`:

| Retest level | Sentence |
| ----- | ----- |
| Retest recommended | Model outputs are withheld for this testing block. Correct the entry or complete a replacement block for a normal profile. |
| Retest suggested | Usable, but one or more results look unusual. Repeating the flagged test would improve confidence. |
| Keep, but interpret cautiously | Usable. Minor issues were noted; interpret the profile with some caution. |
| No retest signal | Usable. No data-quality issues were detected from the entered values. |

## D-010: Historical baseline

**Date:** 2026-10-19 | **Status:** Implemented (Step 6)

A record's baseline is the most recent earlier record (by start date) for the same athlete that is not compromised and is equipment-comparable. Equipment is comparable when both records name the same equipment or both leave it unspecified. "Other" is never comparable, including to another "Other." When the preceding valid record used different equipment, the app discloses the difference instead of issuing a wattage-based warning.

## D-011: Recompute history after every change

**Date:** 2026-10-19 | **Status:** Implemented (Step 5)

After any save, replace, delete, or upload, every record's derived fields are recomputed in date order.

**Why:** editing an earlier record can change the baseline, and therefore the warnings, of later records.

## D-012: CSV import

**Date:** 2026-10-19 | **Status:** Implemented (Step 8)

The whole file is validated before history is replaced. Any failure leaves existing history unchanged.

- **Reject the file** for missing required columns, duplicate athlete/start-date keys, any row with a hard input error (D-006), or any invalid optional value (table below). The message names the rows.
- **Import as compromised** any row with an ordering violation or suspected entry error (D-005), with the reason kept in `warning_flags`.
- **Ignore and recompute** all derived columns, including legacy `riegel_S`, `riegel_E`, and `predicted_power_5min`.
- **Match equipment** to the dropdown list case-insensitively. Unmatched values become "Other."

**Optional columns:**

| Column | Accepted (case-insensitive) | Missing column or blank value | Anything else |
| ----- | ----- | ----- | ----- |
| `protocol_confirmed` | TRUE/FALSE, yes/no, 1/0 | FALSE | Reject the file |
| `test_validity_flag` | Complete, Compromised, Retest needed | Complete | Reject the file |
| `context_flags` | The 8 flag names in D-008, separated by "; " | None | Reject the file |
| `ergometer_type` | Any text, matched as above | Not specified | Becomes "Other" |
| `notes` | Any text | Empty | Kept as entered |

**Why FALSE for `protocol_confirmed`:** a row that never recorded confirmation shows the minor "protocol unconfirmed" warning, since the app doesn't know. History files exported from V1.0 include the column and keep their values. Unknown values are rejected instead of guessed, because an unknown flag is most likely a typo and dropping it would lose information.

**Example CSV:** the downloadable example includes `protocol_confirmed` and `context_flags`, so users see they exist.

**Dates:** `test_date` is accepted as 2026-03-02, or with slashes (3/2/2026 or 3/2/26), because spreadsheet apps often rewrite dates when saving a CSV. Added on 2026-10-20 during implementation.

**Slash date order** (updated 2026-10-31, 1.2.1): the app reads every slash date in a file the same way. A first number above 12 (25/03/2026) means day-first. A second number above 12 (03/25/2026) means month-first. A file showing both is rejected. When every date could be read either way, the app asks the user, using a date from the file as the example. A file where every date has the same day and month (3/3/2026) needs no question.

**Why:** the app previously read all slash dates month-first. A file saved by a spreadsheet set to UK or European dates turned March 2 into February 3 with no warning, and a wrong date can change which earlier test serves as the baseline (D-010).

**Replaces:** month-first reading of all slash dates.

## D-013: Athlete key

**Date:** 2026-10-19 | **Status:** Implemented (Step 5)

The profile key uses the athlete name trimmed, with internal spaces collapsed, compared case-insensitively. The name displays as entered.

## D-014: Equipment dropdown

**Date:** 2026-10-19 | **Status:** Implemented (Step 7)

Options: Not specified, Rogue Echo Bike v1, Rogue Echo Bike v2, Rogue Echo Bike v3, Rogue Echo Rower, AssaultBike Classic, AssaultBike Pro X, AssaultBike Elite, AssaultRower, Concept2 Rower, Concept2 SkiErg, Concept2 BikeErg, Wattbike Atom, Wattbike AtomX, Wattbike Proton, Wattbike AirPro, Wattbike Nucleus, Other. Each listed model counts as different equipment, including each Echo Bike version.

The Rogue Echo Rower and the 5 Wattbike models were added on 2026-10-19 during implementation.

## D-015: Testing block start date

**Date:** 2026-10-19 | **Status:** Implemented (Step 4)

Label: "Testing block start date," with helper text "The day you did the 45-second test." The app does not check whether it's a Monday. The weekday schedule is presented as a suggested cadence.

**Replaces:** the "(first Monday)" label in resolved spec §5.

## D-016: Profile-shape comparison

**Date:** 2026-10-19 | **Status:** Implemented (Step 6)

Historical Comparison overlays the current fitted curve and points on the baseline record's curve and points. It appears only when both produce a normal profile.

## D-017: Ordering failures

**Date:** 2026-10-19 | **Status:** Implemented (Step 6)

Simulation suggests about 6% of honest testing blocks may break the descending order through normal variation, mostly between 10 and 20 min. The message names the failing pair and suggests retesting that effort. The user loads the record, replaces the one value, and saves it under the same start date. This uses the existing replace workflow and does not add a partial-profile mode.

## D-018: Project records

**Date:** 2026-10-19 | **Status:** Implemented

Decisions and their reasons go in this file. User-facing changes go in `CHANGELOG.md`. Planning documents live in `docs/`. Commit messages record each implementation step.

## D-019: Brand colors

**Date:** 2026-10-30 | **Status:** Implemented (1.2.0)

The app uses the Ben Skutnik brand colors: black (#111111) on white, grays for secondary marks, one system font stack, and orange (#E4572E) as the only accent color. Orange marks estimated CP on Historical trends. The 5 test durations are grays, darkest for 45 s and lightest for 20 min, each labeled at its latest point.

**Why:** the app matches the brand's other apps. With orange as the only accent, it marks exactly one series.

**Review:** CP is a secondary metric (non-negotiables memo). If feedback shows the orange CP line pulls attention away from the tested results, switch it to a gray dashed line and use orange elsewhere.

**Replaces:** the blue-and-orange chart colors discussed for 1.2.0, which weren't built.

## D-020: Running mode

**Date:** 2026-10-31 (recorded 2026-11-21) | **Status:** Decided

The app adds running as a second sport. `docs/running_mode_spec.md` holds the full design. Scope:

- Runners do the same 5 fixed-time tests (45 s, 3, 6, 10, 20 min) on the same suggested 11-day cadence, and enter the distance covered.
- The athlete picks a sport for each testing block. Ergometer stays the default. Each block has one sport.
- Runners can test on a track, outdoors with a GPS watch, or on a manual treadmill.
- All model math uses speed in m/s, and the app displays pace. Running reports critical speed (CS) and D′ where ergometer tests report CP and W′.
- The app does not collect body mass or convert running results to watts.
- Running ships as 1.3.0, and existing history files keep working.

**Why:** users asked for running. Fixed-time tests keep the app's 5-duration structure and match the developer's training program. Speed comes straight from distance and time. Running power estimates depend on body mass and on each device's model, so converting to watts would add error, and the ratios between durations would stay the same.

**Motorized treadmills are not supported:** a maximal effort on one means changing belt speed mid-effort, so the result reflects the speed setting more than the runner.

**Widens:** `01_project_definition_memo.md`, which defines the app as an ergometer tool. The profile key (D-013) and baseline (D-010) will gain sport. Those changes are recorded when built.

## D-021: Running validation thresholds

**Date:** 2026-11-21 | **Status:** Decided; Review scheduled

Running checks compare average speed (distance ÷ time) and show pace in their messages.

| Check | Rule | Result |
| ----- | ----- | ----- |
| Unit check | 20 min distance below 1,000 m or above 8,000 m | Hard input error |
| Suspected entry error | Speed ratio (shorter ÷ longer) above 2.0 for 45 s ÷ 3 min, or above 1.5 for any other adjacent pair | Outputs withheld |
| Large reversal | A longer test more than 2% faster than the test before it | Outputs withheld |
| Small reversal | A longer test as fast as, or up to 2% faster than, the test before it | Keep, but interpret cautiously |
| Steep decline | Speed drops more than 35% (45 s to 3 min), 15% (3 to 6 min), or 12% (6 to 10 min and 10 to 20 min) | Retest suggested |
| Curve shape | 3, 6, or 10 min speed more than 4% from a quadratic fit (D-007 method) | Retest suggested |
| Historical mismatch | 3, 6, 10, or 20 min speed more than 8% from the baseline | Retest suggested |

Large reversals and suspected entry errors are pattern blocks (D-006): raw values save only as a compromised record, and uploaded rows import as compromised.

Running adds 3 context flags, each at Retest suggested: Wind, Heat or humidity, and Hills or uneven ground.

The near-flat check is not used for running. D′ displays rounded to the nearest 10 m.

**Simulation method:** 20,000 profiles from the 3-parameter critical-speed model (CS 3.0 to 5.5 m/s, D′ 100 to 350 m, maximum speed 1.4 to 2.3 times CS), in 3 scenarios: 1% day-to-day variation, 1% plus a 10 m GPS distance error, and 2% variation. The variation levels are assumptions. The simulation script is kept outside the repo.

**How often each rule fires on honest blocks:**

| Rule | Track, 1% | GPS, 1% + 10 m | 2% |
| ----- | ----- | ----- | ----- |
| Small or large reversal | 3.2% | 4.2% | 22.5% |
| Large reversal (withheld) | 0.1% | 0.2% | 5.9% |
| Steep decline | 0.0% | 0.0% | 0.8% |
| Curve shape | 0.0% | 0.1% | 3.6% |
| Historical mismatch | 0.0% | 0.0% | 2.2% |
| Suspected entry error | 0.0% | 0.0% | 0.0% |
| Near-flat, 1% (not used) | 9.0% | 9.9% | 15.8% |

**Why:**

- The reversal, curve-shape, historical, and unit limits from the spec hold up. Honest 20 min distances ran 3.7 to 6.9 km.
- **Entry-error limits:** the highest honest speed ratios were 1.51 (45 s ÷ 3 min, GPS) and 1.23 (other pairs). A dropped or extra digit gives a ratio of 10 or more, or a reversal.
- **Steep-decline checks:** curve shape checks only the 3 interior tests, so it caught a 20 min result 10% low only 11% to 29% of the time. The steep check caught it 66% to 76% of the time.
- **No near-flat check:** speed falls only about 3.4% from 10 to 20 min, so a 1% rule fires on about 1 in 10 honest blocks. The reversal checks cover the worst case.

**Known gaps:**

- A 45 s result 10% low is caught 6% to 30% of the time. It affects the curve and the short training targets, not CS or D′.
- A 5% error at 3, 6, or 10 min is caught 16% to 32% of the time.
- D′ estimates carry an error of about 43 to 59 m (1 standard deviation).
- A block entered entirely in the wrong unit, such as miles saved as km, passes every single-block check. Only the historical check catches it.

**Review plan:** same trigger and target as D-007: at least 30 real running blocks, and no soft warning firing on more than 10% of good blocks. Day-to-day variation is the main unknown. If it's closer to 2% than 1%, the small-reversal warning fires on about 1 in 5 good blocks and needs loosening.

**Replaces:** the validation summary in `docs/running_mode_spec.md` section 5, where they differ.

## D-022: Running data model

**Date:** 2026-11-21 | **Status:** Implemented (build step 6)

Each testing block is still one wide row. New columns:

| Column | Holds | Missing column or blank |
| ----- | ----- | ----- |
| `sport` | Ergometer or Running, case-insensitive | Ergometer |
| `distance_45s_m` through `distance_20min_m` | Running distances in meters | Required for running rows |
| `distance_unit` | m, km, or mi, for display only | m |
| `run_setting` | A running setting (spec section 6), matched case-insensitively | Not specified |

Derived columns, recomputed on upload: `estimated_cs_m_per_s`, `estimated_d_prime_m`, and `predicted_speed_1min_m_per_s` through `predicted_speed_60min_m_per_s`.

- **One sport per row.** Each record fills only its own sport's result and model columns. On upload, values in the other sport's columns are ignored.
- **Entry:** distances are entered in the block's unit and stored in meters, rounded to 0.01 m.
- **Profile key:** athlete name, start date, and sport. The same athlete can have an ergometer block and a running block with the same start date.
- **Baseline:** same athlete and sport, with comparable equipment or setting. "Other" and "Other manual treadmill" are never comparable.
- **Upload:** every file needs `athlete_name` and `test_date`. Each row needs the 5 result columns for its own sport, so a file of ergometer rows needs only the power columns. The file is rejected for an unrecognized sport, an unrecognized distance unit on a running row, or a running-only context flag on an ergometer row. An unrecognized running setting becomes "Other."
- **Example CSV:** one ergometer row and one running row, with blank cells left empty.

**Why:** separate columns per sport keep watts and meters from mixing in one field. Ignoring the other sport's columns lets a hand-edited file with leftover values still upload its valid data, since a file the app exports never has them. Sport in the key follows the spec's one-sport-per-block assumption.

**Changes:** D-010 (baseline adds sport and the running settings) and D-013 (key adds sport).