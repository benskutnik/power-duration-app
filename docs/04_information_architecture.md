## 1. Recommended App Structure

Use **5 top-level pages**:

1. **Overview**
2. **Enter Test Results**
3. **Profile & Targets**
4. **History**
5. **Testing Protocol**

This is the right v1 structure because it matches the user’s actual jobs:

- understand where they are
- enter data
- see what the data means
- compare past results
- learn how to do the tests correctly

Do **not** build the app around physiology concepts or model outputs. Build it around user tasks.

## 2. Purpose of Each Page

### 1. Overview
This is the landing page and control center. It should tell the user what profile they are looking at, whether the current test set is complete, and what to do next.

Why it exists:
- users need orientation before interpretation
- users need to know whether their dataset is usable
- users need one obvious next step

### 2. Enter Test Results
This is the data entry page for the five fixed tests. Its job is to make input fast, clear, and error-resistant.

Why it exists:
- entering values is the core action that powers the rest of the app
- separating entry from interpretation reduces confusion
- fixed-duration testing is simple enough to deserve a dedicated page

### 3. Profile & Targets
This is the main interpretation page. It should show the power-duration curve, the five test points, simple profile summaries, and practical training targets.

Why it exists:
- this is the primary payoff page
- users want to see their profile visually and get usable outputs
- targets should sit next to the profile, not buried elsewhere

### 4. History
This page lets users compare saved profiles across dates.

Why it exists:
- tracking change over time is one of the app’s main reasons to exist
- comparison needs its own space to avoid cluttering the main interpretation page

### 5. Testing Protocol
This page explains exactly how to perform the 11-day testing sequence and what the results do and do not mean.

Why it exists:
- test quality determines data quality
- user-facing scientific honesty belongs in a dedicated page
- protocol guidance should be available without interrupting the analysis workflow

## 3. What Each Page Must Contain

### 1. Overview
Must contain:
- app title and one-sentence purpose
- current profile status: complete / incomplete
- simple completion tracker for the five tests
- date of the active test set
- clear call-to-action button:
  - if incomplete: **Enter Test Results**
  - if complete: **View Profile**
- brief plain-English summary of what the app does:
  - estimates a descriptive power-duration profile
  - can estimate practical targets
  - does not directly measure exact physiological thresholds

### 2. Enter Test Results
Must contain:
- five fixed input fields, one for each duration:
  - 45 s
  - 3 min
  - 6 min
  - 10 min
  - 20 min
- units clearly labeled
- test date field for the test set
- save button
- update/edit existing results behavior
- immediate validation:
  - missing values
  - impossible values
  - obvious entry errors
- optional small helper text near each duration, not large blocks
- simple display of test schedule:
  - Mon / Wed / Fri / Mon / Thu over 11 days

Strong recommendation:
- use one structured card or row per duration
- keep entry on one page, no wizard

### 3. Profile & Targets
Must contain:
- main power-duration chart as the dominant element
- plotted user test points
- fitted descriptive curve
- simple profile summary cards, such as:
  - best short-duration power
  - best mid-duration power
  - best long-duration power
  - estimated critical power if included
  - estimated W′ if included
- practical target section with plain labels
- short explanation of what targets are:
  - practical estimates from the current profile
  - not direct measures of exact thresholds
- clear indication of which saved test set is being viewed
- option to save or name the profile snapshot if needed

### 4. History
Must contain:
- list of saved test dates or named profiles
- choose one or two profiles to compare
- comparison chart with overlaid curves and points
- simple change summary between selected profiles
- table showing the five raw test values by date
- obvious way to set one profile as the active profile for deeper viewing

Strong recommendation:
- limit comparison to two profiles in v1

### 5. Testing Protocol
Must contain:
- the full 11-day schedule in plain language
- one short section for each test duration
- pacing guidance for each test
- warm-up guidance
- recovery expectations between tests
- instructions on using the same equipment/settings each time
- explanation of why the tests are ordered shortest to longest
- clear “what this app does not claim” section
- guidance on when not to test:
  - illness
  - acute fatigue
  - inconsistent setup
  - incomplete recovery

## 4. What Each Page Should Avoid

### 1. Overview
Avoid:
- detailed charts
- too many stats
- raw model parameters
- long educational text
- history comparison tools

### 2. Enter Test Results
Avoid:
- interpretation
- advanced model settings
- multiple save pathways
- too much explanatory text
- freeform duration entry

### 3. Profile & Targets
Avoid:
- dense physiology language
- too many derived metrics
- multiple competing charts
- zone systems with false precision
- academic model diagnostics unless absolutely necessary

### 4. History
Avoid:
- comparing more than two profiles
- cluttered tables with too many calculated columns
- session-level workout logs
- detailed protocol education

### 5. Testing Protocol
Avoid:
- interactive analytics
- dense sport-science discussion
- references to concepts the app does not actually estimate well
- too much warning language that overwhelms the user

## 5. Recommended User Flow

### First-time user flow
1. **Overview**
   - user understands what the app is for
   - sees that no complete profile exists yet

2. **Testing Protocol**
   - user reads the schedule and test instructions before entering anything
   - this matters because bad testing ruins the outputs

3. **Enter Test Results**
   - user enters the five values after completing the testing block
   - validation catches obvious mistakes

4. **Profile & Targets**
   - user sees the curve, key summaries, and practical targets
   - this is the main reward moment

5. **History**
   - only after at least two saved profiles exist
   - user compares progress over time

### Returning user flow
1. Overview
2. Profile & Targets or Enter Test Results
3. History when needed
4. Testing Protocol only as reference

Important design decision:
- first-time users should be nudged toward **Testing Protocol**
- returning users should be nudged toward **Profile & Targets** or **Enter Test Results**

## 6. Visual Priority Rules

Across the app, the visual hierarchy should be:

### Primary
- the user’s current status
- the next action
- the main chart on the interpretation page
- the five test inputs on the entry page

### Secondary
- summary metric cards
- training target outputs
- comparison tables
- protocol helper notes

### Tertiary
- estimated critical power and W′
- scientific caveats
- background explanations
- any export or naming controls

Specific rules:
- one dominant visual per page
- one dominant action per page
- supporting text should be short and below the main action, not above it
- the user should never have to scan past technical content to do the primary task

## 7. Simplicity Rules

1. **One job per page.**  
Each page should have a primary task. Do not mix entry, education, comparison, and interpretation on the same screen.

2. **Use fixed durations only.**  
Do not let users create custom test durations in v1.

3. **Use plain labels first, technical labels second.**  
Example: say “Estimated sustainable power” before “Critical Power,” not the other way around.

4. **Show only a few outputs that matter.**  
If a metric does not change a practical decision, it should not be prominent.

5. **Use constrained inputs.**  
Numeric fields, clear units, required durations, and validation.

6. **Keep the chart readable before making it sophisticated.**  
A clean curve with five points is enough for v1.

7. **Avoid false precision.**  
Round outputs sensibly. Do not present exact-looking numbers that imply certainty the model does not have.

8. **Repeat scientific honesty in the right places only.**  
Overview, Profile & Targets, and Protocol should each contain brief guardrails. Do not flood every screen with warnings.

## 8. Version 1 Restraint Rules

Deliberately leave out:

- custom test batteries
- free-form duration modeling
- advanced parameter tuning
- multiple model selection options
- detailed residual/error plots
- workout library integration
- daily readiness or fatigue scoring
- automated protocol adaptation
- athlete types or training personas
- zone systems with many bands
- session recommendations generated from weak evidence
- export dashboards
- coach/admin multi-user features
- mobile-optimized complexity beyond basic responsiveness

Also leave out:
- any UI that suggests the app directly measures lactate threshold, ventilatory thresholds, or exact metabolic domains
- any dense explanation of CP/W′ math beyond a brief optional note

v1 should feel narrow, clean, and trustworthy.

## 9. Open UI Decisions to Lock Next

### 1. Navigation style
Decide whether the app uses:
- top navigation tabs
- left sidebar navigation

Recommendation: **top tabs** for v1, because there are only five pages and the app is task-based, not dashboard-heavy.

### 2. Data entry layout
Decide whether the five tests appear:
- as one vertical form
- as five cards in a grid
- as a table-style entry form

Recommendation: **one vertical form with one card per duration**. It is easiest to read, validate, and build in Shiny.

### 3. Main chart layout on Profile & Targets
Decide whether the page is:
- chart first, metrics below
- chart left, metrics right

Recommendation: **chart first, metrics below**. The chart is the core object and should dominate the page.

### 4. History comparison layout
Decide whether comparison uses:
- side-by-side metric cards with one overlay chart
- one big comparison table plus chart

Recommendation: **one overlay chart plus a small comparison table**. The visual trend matters more than a dense table.

### 5. Protocol presentation format
Decide whether the protocol page is:
- a long single page
- accordion sections
- schedule summary plus expandable sections

Recommendation: **schedule summary plus expandable sections**. This keeps the page readable while still holding enough detail.

The core principle is simple: build v1 around **complete profile entry, clear interpretation, and trustworthy guidance**. Everything else is secondary.
