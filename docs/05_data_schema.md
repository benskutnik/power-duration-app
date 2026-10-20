# Power-Duration App --- Version 1 Data Schema

## 1. Core Data Model

Use **four main record types** in version 1:

### A. `athlete_profile`

One row per user.

**Purpose:** - Stores stable user-level information\
- Supports individualized display, scaling, and history tracking

### B. `test_session`

One row per completed 11-day testing block.

**Purpose:** - Groups the five tests into one interpretable profile\
- Stores the date range, completion status, and session-level notes\
- Acts as the main anchor for comparisons over time

### C. `test_result`

One row per duration within a session.

**Purpose:** - Stores the actual entered result for each of the five
fixed tests\
- This is the most important raw data table in the app

### D. `derived_profile`

One row per completed session.

**Purpose:** - Stores calculated outputs derived from the five test
results\
- Keeps calculations separate from raw input data\
- Simplifies plotting, reporting, and comparison

## 2. Required Input Fields

### `athlete_profile`

-   athlete_id (character): Unique user identifier\
-   body_mass_kg (numeric): Enables W/kg calculations

### `test_session`

-   session_id (character): Unique session identifier\
-   athlete_id (character): Links session to user\
-   session_start_date (date): Start of 11-day window\
-   session_label (character): Human-readable session name

### `test_result`

-   result_id (character): Unique test identifier\
-   session_id (character): Links to session\
-   duration_sec (integer): One of 45, 180, 360, 600, 1200\
-   test_date (date): Date performed\
-   avg_power_w (numeric): Core performance metric

## 3. Optional Input Fields

### `athlete_profile`

-   athlete_name (character): Display label

### `test_session`

-   session_notes (character): Context for session\
-   completed_flag (logical): Marks full completion

### `test_result`

-   rpe (numeric): Effort quality\
-   device_name (character): Ergometer tracking\
-   protocol_notes (character): Notes on execution\
-   cadence_rpm (numeric): Optional context\
-   heart_rate_avg (numeric): Optional context\
-   test_completed_flag (logical): Valid vs invalid effort

## 4. Derived Fields

### Test-Level

-   avg_power_wkg (numeric): Relative output\
-   duration_label (character): Friendly label\
-   test_order (integer): Sequence\
-   days_from_session_start (integer): Schedule validation

### Session-Level

-   cp_w, w_prime_kj, cp_wkg\
-   pmax_45s_w, p180_w, p360_w, p600_w, p1200_w\
-   percent_drop metrics\
-   profile_shape_tag\
-   training_target\_\*\
-   fit_r2\
-   model_fit_flag\
-   profile_summary_text\
-   derived_at

## 5. History Storage Schema

Files: - athlete_profile.csv\
- test_sessions.csv\
- test_results.csv\
- derived_profiles.csv

## 6. CSV Import/Export

### Internal (Long)

Required: - session_id, duration_sec, test_date, avg_power_w

### User (Wide)

Required: - athlete_id, session_label, session_start_date, body_mass_kg\
- power_45s_w, power_3min_w, power_6min_w, power_10min_w, power_20min_w

## 7. Validation Fields

-   duration_sec\
-   test_date\
-   session_start_date\
-   avg_power_w\
-   body_mass_kg\
-   test_completed_flag\
-   rpe

## 8. Excluded Fields

-   physiology lab metrics\
-   demographics beyond body mass\
-   detailed training logs\
-   environmental data\
-   raw time series

## 9. Future-Proofing

-   Separate raw vs derived\
-   Use IDs\
-   Standardize durations\
-   Add fields, don't redesign\
-   Long format internally

## 10. Final Recommendation

Use four tables: - athlete_profile\
- test_sessions\
- test_results\
- derived_profiles

**Principle: Raw test data is the foundation. Everything else is layered
and replaceable.**
