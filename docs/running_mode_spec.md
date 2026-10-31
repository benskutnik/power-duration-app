# Running mode spec

**Status:** Approved 2026-10-31. D-020 in `docs/DECISIONS.md` records the scope decisions below. Later DECISIONS entries record details as they're built, and those entries control.

---

## 1. Decisions made

Agreed on 2026-10-31:

1. **Running is in scope.** Users have asked for it. This widens `01_project_definition_memo.md`, which defines the app as an ergometer tool.
2. **Fixed-time tests.** Runners do the same timed efforts and record the distance covered. This matches the developer's training program and keeps the app's 5-duration structure.
3. **One app.** The athlete picks a sport for each testing block. Ergometer stays the default.
4. **Where runners can test:** a track, outdoors with a GPS watch, or a manual (self-powered, non-motorized) treadmill. Motorized treadmills are not supported: a maximal effort on one means changing belt speed mid-effort, so the result reflects the speed setting more than the runner.
5. **Chart axis:** plot speed, labeled in pace (for example 3:30, 4:00, 4:30 /km), so faster sits at the top and the curve keeps the same shape as the ergometer chart. Open to change after testing.
6. **App name:** stays "Power-duration app" for now.
7. **Manual treadmill models:** AssaultRunner Pro, AssaultRunner Elite, TrueForm Runner, and Woodway Curve, plus Other manual treadmill.

## 2. Assumptions to confirm

- Running uses the same 5 durations (45 s, 3, 6, 10, 20 min) and the same suggested 11-day cadence as ergometer testing.
- One sport per testing block. An athlete who does both keeps separate blocks.

## 3. Units

- **Entry unit:** chosen once per testing block: meters, kilometers, or miles.
- **Storage:** always meters. 1 km = 1,000 m. 1 mi = 1,609.344 m.
- **Speed:** meters ÷ seconds, used internally for all model math and checks.
- **Display:** pace in min:sec per km when the block uses meters or kilometers, and per mile when it uses miles. Pace rounds to the nearest second. Distances display in the block's unit.
- **D′:** shown in meters for every unit choice.

## 4. Model

- **Curve:** log(speed) = alpha + beta × log(duration), the same fit used for power. Stored in the existing `power_law_a` and `power_law_b` columns.
- **CS and D′:** fit distance = CS × time + D′ to the 3, 6, 10, and 20 min tests, the same tests used for CP. If either value is non-finite or not positive, both are reported as unavailable (same rule as CP and W′).
- **Predictions:** the same 8 durations. 30 and 60 min are labeled extrapolated.
- **Bibliography:** add critical-speed sources, checked the same way as the existing entries. Candidates to verify: Hughson, Orok and Staudt (1984) on a treadmill test for critical velocity, and Smyth and Muniz-Pumares (2020) on critical speed in recreational marathon runners. Kennelly, Riegel, and Péronnet and Thibault already cover running performance.

## 5. Validation

### Why ergometer thresholds don't transfer

Speed falls off much less with duration than power does. An example runner with CS of 4.0 m/s and D′ of 200 m:

| Test | Distance | Speed | Drop from previous |
| ----- | ----- | ----- | ----- |
| 45 s | 300 m | 6.67 m/s | |
| 3 min | 920 m | 5.11 m/s | 23% |
| 6 min | 1,640 m | 4.56 m/s | 11% |
| 10 min | 2,600 m | 4.33 m/s | 5% |
| 20 min | 5,000 m | 4.17 m/s | 4% |

The ergometer steep-decline rules (20% and 25%) would almost never fire on this profile. The 1% near-flat rule sits close to normal 10-to-20-min drops.

### Hard input errors (block saving)

- Name or date missing.
- A distance missing, non-numeric, zero, or negative.
- 20 min distance above 8,000 m. The 10,000 m world record works out to about 7,600 m in 20 minutes, so a larger value most likely comes from a unit mistake. Message: check the unit.
- 20 min distance below 1,000 m, which is 3 km/h, slower than walking. This catches km or mi values typed with meters selected. Message: check the unit.

### Pattern blocks (save only as compromised)

- **Ordering:** each shorter test must have a higher speed than the next longer one.
- **Suspected entry error:** adjacent speed ratios above running-specific limits, set by the running-threshold simulation (build order step 3).

### Soft warnings

Same 6 checks as D-007, applied to speed: near-flat, steep 45 s to 3 min, steep declines, curve shape, and historical mismatch. Running thresholds come from a new simulation (build order step 3) using a 3-parameter speed model with 2% to 3% day-to-day error plus measurement error.

**Measurement error matters here:** a 10 to 20 m GPS error on a 300 m effort is 3% to 7%, larger than the ergometer curve-shape limit of 4%. Manual treadmill consoles also differ in calibration between brands. The simulation should include both.

### Context flags

The existing 8 flags apply to both sports. Running adds 3, each at **Retest suggested**:

- Wind
- Heat or humidity
- Hills or uneven ground

These only appear when the block's sport is running.

## 6. Running setting

Replaces the ergometer dropdown for running blocks.

Options: Not specified, Track, Outdoor (GPS watch), AssaultRunner Pro, AssaultRunner Elite, TrueForm Runner, Woodway Curve, Other manual treadmill, Other.

- **Outdoor (GPS watch)** covers roads, paths, and trails. Terrain differences are recorded with the Hills or uneven ground flag.
- **Track** stays separate, since a measured track gives more accurate distances than GPS.
- **Manual treadmills** are listed by model because each brand calculates distance its own way.

Comparability follows D-010: the same setting is comparable, Not specified matches only Not specified, and the 2 Other options are never comparable. Track and Outdoor are not comparable with each other, and no treadmill model is comparable with another.

## 7. Data and CSV

New columns:

| Column | Values | Missing column or blank |
| ----- | ----- | ----- |
| `sport` | Ergometer, Running | Ergometer |
| `distance_unit` | m, km, mi (display only) | m |
| `run_setting` | The options in section 6 | Not specified |
| `distance_45s_m` through `distance_20min_m` | Meters | Required for running rows |

New derived columns, ignored and recomputed on upload like the existing ones: `estimated_cs_m_per_s`, `estimated_d_prime_m`, and `predicted_speed_1min_m_per_s` through `predicted_speed_60min_m_per_s`.

Rules:

- Ergometer rows leave running columns blank, and running rows leave `power_*`, `ergometer_type`, and the CP columns blank.
- Each row must have the 5 result columns for its own sport. A file with only ergometer columns still uploads, so all existing history files keep working.
- **Profile key:** athlete name + start date + sport. Changes D-013.
- **Baseline:** same athlete, same sport, comparable setting or equipment. Changes D-010.
- All other D-012 upload rules apply unchanged, including the slash-date order rules added in 1.2.1.

## 8. Interface

- **Sport choice** at the top of the entry sidebar: Ergometer (default) or Running. Choosing Running swaps the 5 input labels (for example, "45 s distance (m)"), shows the unit picker and setting dropdown, and hides the ergometer dropdown. Inputs accept decimals for km and mi.
- **Profile chart:** speed on the y-axis, labeled in pace at round values. Pace gridlines are not evenly spaced, since pace is the inverse of speed.
- **Summary:** estimated CS shown as pace, D′ in meters.
- **Training targets:** same 5 categories and anchors, shown as pace, for example "4 min: 4:02 /km."
- **Historical comparison:** change shown in seconds per km or mile and as a percentage.
- **Historical trends:** one athlete and one sport at a time. The Athlete dropdown from 1.2.1 gains a sport filter.
- **Testing protocol:** separate running sections (section 9).
- **Import / export:** lists the running columns. The example CSV includes a running row.

## 9. Running protocol content

- **Warm-up:** easy running, drills, and strides, kept the same before every test.
- **Pacing:** same guidance as ergometer tests, adapted for running.
- **Measuring distance on a track:** start on a marked line, count laps, and have a partner mark where you stop when time runs out. Record the total to the nearest 10 m.
- **Measuring distance outdoors with a GPS watch:** use the same watch every time, turn auto-pause off, and record the watch's distance for the effort. Use the same route or a similar flat one for every test. GPS is least accurate on the 45 s and 3 min tests, so a track is better for those when one is available.
- **Measuring distance on a manual treadmill:** use the console's distance for the effort, and use the same treadmill for every test. Don't compare results across treadmill brands.
- **Not supported:** motorized treadmills.
- **Safety:** the 45 s test is close to a sprint. Build up with strides during the warm-up.
- **Conditions:** note wind, heat, or hills with the context flags.

## 10. Out of scope

- Fixed-distance tests (for example 400 m, 1,600 m, 5 km)
- Motorized treadmills
- Race time predictions
- Elevation or treadmill grade adjustments
- Heart rate, cadence, body mass

## 11. Build order

1. Finish and release 1.2.1. Done 2026-10-31.
2. Add D-020 to `docs/DECISIONS.md`. Done 2026-10-31.
3. Run the running-threshold simulation and record the results as D-021.
4. Refactor the code so each sport's labels, units, and thresholds come from one settings list. Ergometer behavior stays the same, and all existing checks still pass.
5. Add the data model and CSV changes, with tests.
6. Build the entry interface and unit conversion.
7. Update model outputs, charts, and training targets to show pace.
8. Write the running protocol and Import / Export text.
9. Add running tests, do a full walkthrough, and release.

**Version:** running adds a feature and keeps old files working, so it ships as the next minor version, 1.3.0.

## 12. Open questions

None as of 2026-10-31.
