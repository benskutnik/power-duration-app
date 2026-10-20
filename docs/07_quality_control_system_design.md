# Quality-Control Logic Memo (Version 1)

## 1. Purpose of the Quality-Control System

The quality-control system should protect the app from turning weak, distorted, or obviously wrong test data into confident-looking outputs.

Its job is to reduce four main risks:

- data-entry error: wrong units, misplaced digits, swapped test results, missing values
- bad test execution: pacing mistakes, incomplete efforts, interrupted tests, non-maximal attempts
- fatigue contamination: a later test being meaningfully suppressed by poor recovery, illness, or unusual training load
- false confidence: the app presenting smooth curves, targets, or estimated metrics as trustworthy when the input data are questionable

In version 1, the system should not try to prove whether a test was valid. It should do something simpler and more useful: block clearly unusable data, flag suspicious patterns, and tell the user when the profile is likely less reliable than it looks.

---

## 2. Hard-Stop Validation Rules

These conditions should block calculation or saving entirely.

### Required data completeness
- All five required durations must have a value before a full profile can be calculated:
  - 45 seconds
  - 3 minutes
  - 6 minutes
  - 10 minutes
  - 20 minutes

### Numeric validity
- Every entered test value must be numeric.
- No blank strings, text, symbols, or malformed entries.
- No negative values.
- No zero values.

### Unit consistency
- The app must require the user to enter values in one clearly defined unit system for the session.
- If the app is built around average power in watts, then non-watt entries should not be accepted.
- Do not allow mixed units inside one profile.

### Duplicate-duration overwrite protection
- Do not allow two different values to be saved for the same duration in the same test session without explicit overwrite confirmation.

### Impossible ordering check
For maximal average power across these durations, power should decline as duration gets longer. A strict descending pattern should be required for saving a clean profile:

- 45 s power > 3 min power
- 3 min power > 6 min power
- 6 min power > 10 min power
- 10 min power > 20 min power

If any shorter test is equal to or below a longer test, block full-profile calculation and saving until the user confirms correction or marks the session as incomplete/problematic.

### Obvious digit-error check
Block saving if one result is grossly out of scale relative to the others in a way that strongly suggests a typo, such as:
- one value more than about 2x the adjacent-duration values
- one value less than about 50% of the adjacent-duration values

---

## 3. Soft-Warning Rules

These should trigger warnings but still allow the user to continue.

### Near-flat drop between adjacent durations
Warn if the decline between adjacent tests is unusually small, for example:
- less than about 1-2% drop from 3 min to 6 min
- less than about 1-2% drop from 6 min to 10 min
- less than about 1-2% drop from 10 min to 20 min

### Very steep drop between adjacent durations
Warn if the decline between adjacent tests is unusually large, for example:
- more than about 15-20% drop between neighboring durations

### Break in curve smoothness
Warn if one result creates a visible kink that does not match the rest of the profile, even if the rank order is still correct.

### Large mismatch with historical data
Warn when a new result differs substantially from the athlete’s recent history for the same duration, such as:
- more than about 5-8% change from the most recent comparable test

### Session quality self-report problem
Warn if the user indicates:
- poor sleep
- illness
- unusual soreness
- interrupted warm-up
- ergometer malfunction
- obvious pacing mistake

### Partial-profile mode
Warn that:
- the profile is incomplete
- targets and model estimates are provisional

---

## 4. Pattern-Based Suspicion Rules

### Possible pacing error
- 20-minute result unusually close to 10-minute result
- 3-minute result low relative to 45 s and 6 min
- 20-minute result much lower than expected vs 6 and 10 min

### Possible fatigue distortion
- later tests uniformly depressed
- 10 and 20 min both weak vs short durations

### Possible non-maximal effort
- one test much lower than adjacent durations
- user reports effort not all-out

### Possible data-entry mistake
- descending order violated
- one value implausibly large or small

---

## 5. Retest Logic

### Strong retest recommendation
- hard-stop triggered
- rank order broken
- blatant outlier
- interrupted or failed effort

### Moderate retest recommendation
- one suspicious test
- large mismatch with history
- fatigue likely affected result

### Mild retest suggestion
- minor anomalies
- imperfect conditions but usable data

### Retest target
- recommend repeating the suspicious duration only

### Retest phrasing hierarchy
- Retest recommended
- Retest suggested
- Keep, but interpret cautiously

---

## 6. User-Facing Warning Language

### Hard-stop examples
- This result pattern does not look valid enough to calculate a full profile. Please review your entries.
- A shorter test cannot be lower than a longer test in this profile. Check for entry mistakes or repeat the affected test.
- One or more values appear invalid or incomplete. Enter all five test results to continue.

### Soft-warning examples
- Your profile can be calculated, but one result looks unusual. Interpret this profile with caution.
- Your 6-minute result is lower than expected relative to your other tests.
- Your 20-minute result may have been affected by pacing or recovery.
- This result differs noticeably from your previous test history.
- You marked this session as affected by poor readiness or disrupted conditions.

### Retest examples
- Retest recommended: this result is likely too distorted for confident use.
- Retest suggested: one test may not reflect your true current ability.
- This profile is usable, but repeating the flagged test would improve confidence.

---

## 7. Coach/Admin Review Logic

### Data quality flags
- missing test(s)
- corrected entry
- overwritten result
- hard-stop triggered
- soft warning triggered
- retest level assigned

### Pattern flags
- rank-order violation
- drop too small or too large
- curve kink
- historical mismatch
- pacing issue (suspected)
- fatigue issue (suspected)
- entry issue (suspected)

### Context notes
- poor sleep
- illness
- hard training between tests
- erg issue
- interrupted effort
- pacing mistake

### Confidence summary
- High confidence
- Moderate confidence
- Low confidence

---

## 8. What the App Should Not Pretend to Know

The system must not claim certainty about:
- fatigue
- pacing correctness
- effort maximality
- physiological systems
- causes of performance changes

It should only state that:
- the pattern is unusual
- confidence is reduced

---

## 9. Version 1 Restraint Rules

- No 0-100 scoring systems
- No physiology diagnosis
- No auto-correction of data
- No advanced anomaly modeling
- No certainty language for soft warnings
- No complex retest scheduling
- Keep athlete UI simple

---

## 10. Final Recommendation

Use a three-layer system:

### Layer 1: hard stops
Block when:
- missing data
- invalid entries
- unit issues
- rank order broken
- obvious outliers

### Layer 2: soft warnings
Warn when:
- unusual drops
- curve kink
- mismatch with history
- poor conditions
- incomplete profile

### Layer 3: retest guidance
- Retest recommended
- Retest suggested
- Keep, but interpret cautiously

Core principle:

Block what is unusable. Flag what is suspicious. Save what is useful. Do not overclaim.
