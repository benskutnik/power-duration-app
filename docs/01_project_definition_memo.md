## 1. Product Definition

This app is a user-facing Shiny tool that helps home-gym athletes build, view, and track a practical power-duration profile from five maximal average-power tests completed across an 11-day protocol.

It does five things:

1. collects the user’s best average power for 45 seconds, 3 minutes, 6 minutes, 10 minutes, and 20 minutes
2. visualizes those results as a power-duration curve and related comparison views
3. stores repeated testing blocks so users can compare changes over time
4. turns the profile into practical training guidance anchored to tested performance
5. explains the testing protocol in plain language so users can execute it consistently

The problem it solves is simple: most athletes either rely on a single anchor metric, guess at training targets, or use overly technical models that are hard to interpret and easy to misuse. This app gives them a broader, more useful picture of performance across durations without pretending to be a lab test.

This is not a physiology diagnostic tool. It is a practical field-testing and decision-support app built around repeated maximal efforts on an ergometer.

## 2. Target User

**Primary user:** home-gym athletes using ergometers who want a simple, repeatable way to assess performance across short, medium, and longer efforts.  
What they need:
- a test process they can actually complete
- clear data entry
- understandable charts
- practical training targets
- progress tracking without jargon

**Secondary user:** coaches working with athletes who use ergometers and want a quick field-based profiling tool.  
What they need:
- a standardized testing protocol
- clean comparison views across test dates
- outputs that are actionable in programming
- language that is scientifically defensible when shared with athletes

**Not the target user for V1:** exercise physiologists, data scientists, or users expecting lab-grade metabolic assessment. The app should not be optimized around advanced modeling at the expense of usability.

## 3. Core User Jobs

The app should help users do these jobs:

1. **Complete a structured testing block correctly.**  
   The protocol must be easy to understand and hard to misinterpret.

2. **Enter or update five test results quickly.**  
   Data entry must be frictionless and obvious.

3. **See how power changes across duration.**  
   Users need an immediate visual summary of their performance profile.

4. **Identify strengths and weaknesses across the curve.**  
   The app should make it easy to see whether the user is relatively better at short, medium, or longer efforts.

5. **Generate practical training targets from tested performance.**  
   The app should convert results into usable guidance without pretending to define exact physiology.

6. **Compare one testing block to another.**  
   Users need to see whether they improved, regressed, or changed shape across durations.

7. **Use the results for training decisions.**  
   The outputs should help answer: What should I train more? What should I target? Where am I improving?

## 4. Version 1 Scope

Version 1 must include:

**A. Protocol page**
- clear explanation of the 11-day testing structure
- purpose of each test
- pacing and execution guidance
- warm-up guidance
- recovery/rest guidance between tests
- instructions for when to retest
- basic standardization notes such as ergometer consistency, freshness, and testing conditions

**B. Test entry**
- inputs for the five fixed durations: 45s, 3m, 6m, 10m, 20m
- one complete testing block tied to a test date or testing cycle label
- basic input validation
- ability to edit saved results

**C. Power-duration visualization**
- a clean plot showing the five entered points
- a fitted curve or smoothed profile used as a practical visual estimate
- duration on the x-axis and power on the y-axis
- clear labeling and readable interpretation

**D. Historical comparison**
- save multiple testing blocks
- compare current vs prior profile
- show absolute and percentage changes by duration
- show whether the curve shifted up, down, or changed shape

**E. Practical training guidance**
- simple recommendations anchored to tested durations and profile shape
- estimated training targets framed as practical ranges or anchors
- short explanatory text on how to use them in training

**F. Secondary estimated metrics**
- optional display of estimated critical power and W′
- clearly labeled as model-derived estimates
- not required for the main app flow

**G. Readable Shiny implementation**
- modular enough to extend later
- understandable R code
- structured for learning and maintenance

## 5. Explicit Exclusions

Version 1 should not include:

- direct claims to measure lactate threshold, ventilatory thresholds, VO2max, or exact metabolic zones
- adaptive training plans
- automated workout prescription engines
- wearable integrations or live ergometer integrations
- advanced athlete management features
- multi-user coach dashboards
- machine learning predictions
- highly parameterized physiological modeling
- duration-custom test protocols
- mobile app packaging
- social or gamified features
- nutrition, recovery, or readiness scoring systems
- any output that implies medical, diagnostic, or lab-grade validity

These are distractions for V1. The app wins by being clear, useful, and trustworthy.

## 6. Scientific Guardrails

### What the app can reasonably claim
- it estimates a user’s power-duration relationship from five field tests
- it provides a practical performance profile across fixed durations
- it helps track changes in field-tested performance over time
- it can generate training guidance anchored to tested outputs
- it may estimate model-based values such as critical power and W′ as secondary metrics

### What the app cannot claim
- that it directly measures lactate threshold
- that it directly measures ventilatory thresholds
- that it identifies exact metabolic zones
- that it diagnoses physiology
- that it predicts race performance with precision
- that its fitted curve perfectly represents true maximal capability at all durations
- that estimated critical power or W′ are ground truth

### Wording that must be handled carefully
Use:
- “estimate”
- “profile”
- “field-tested”
- “practical training anchor”
- “model-derived”
- “based on your entered maximal efforts”
- “useful for guiding training”

Avoid:
- “proves”
- “measures your threshold”
- “reveals your exact energy systems”
- “pinpoints your zones”
- “scientifically determines”
- “this is your true critical power”
- “this tells you exactly how to train”

### Practical scientific stance
The app should present results as informed estimates from a standardized field protocol. The core promise is practical usefulness, not physiological certainty.

## 7. Product Principles

1. **Use field-test language, not lab-test language.**  
   The app must never imply it measures physiology directly when it only estimates performance from maximal efforts.

2. **Make the five tested durations the backbone of the product.**  
   The app’s authority comes from these actual efforts, not from model outputs.

3. **Prioritize clarity over sophistication.**  
   A simpler output that users understand is better than a more advanced one they misuse.

4. **Every result must be interpretable by a non-expert.**  
   Charts, labels, and guidance should make sense without sport-science training.

5. **Show estimates as estimates.**  
   Fitted curves and derived metrics must always be visually and verbally distinguished from measured test points.

6. **Practical training value beats theoretical completeness.**  
   The app should help users make better training decisions, not impress them with complexity.

7. **Historical comparison is a core feature, not an extra.**  
   The value of the app grows through repeated use and trend visibility.

8. **Protocol adherence matters as much as analytics.**  
   Poor testing execution weakens everything downstream, so the protocol page must be treated as product-critical.

9. **Do not let critical power dominate the experience.**  
   CP and W′ can appear, but the app’s main logic should remain broader than a two-parameter model.

10. **Keep the code extensible and teachable.**  
   Version 1 should be built so future improvements are easy without turning the codebase into a black box.

11. **Use conservative language wherever scientific precision is limited.**  
   When confidence is lower, wording should become more careful, not more promotional.

12. **Design for repeatability.**  
   Users should be able to run the protocol again under similar conditions and make valid practical comparisons.

## 8. Priority Decisions

The next five decisions that should be locked are:

1. **What exact calculation logic will power the main profile view?**  
   You need to decide the primary visual and analytical method: raw points only, fitted curve, and which curve logic is acceptable for V1.

2. **How will training targets be derived and displayed?**  
   This is central to usefulness. You need explicit rules for turning test results into practical guidance.

3. **What data structure will store testing blocks and history?**  
   Decide how sessions are saved, labeled, edited, and compared in Shiny.

4. **What exactly will the user see on the main results screen?**  
   Lock the information hierarchy: chart, summary, strengths/weaknesses, targets, comparison, and optional model metrics.

5. **How will the app communicate uncertainty and limitations?**  
   Decide the standard wording for estimates, derived metrics, and guidance so this stays consistent across the app.

## 9. Recommended Build Order

1. **Lock the scientific and product logic.**  
   Define exactly how the profile is built, how comparisons work, and how training guidance is generated.

2. **Design the app structure on paper first.**  
   Finalize the pages, tabs, modules, and user flow before writing much code.

3. **Build the protocol page.**  
   This sets the testing standard and prevents garbage-in, garbage-out.

4. **Build the data-entry workflow.**  
   Create the inputs, validation, save/edit logic, and test-session structure.

5. **Build the main power-duration chart.**  
   Show the five points first, then add the fitted profile layer if included.

6. **Build the summary interpretation layer.**  
   Add simple outputs that explain the profile shape and highlight stronger vs weaker durations.

7. **Build historical comparison.**  
   Add session storage, prior-vs-current comparisons, and change summaries by duration.

8. **Build practical training guidance outputs.**  
   Add the training-anchor logic and keep the wording disciplined.

9. **Add optional secondary metrics.**  
   Include estimated CP and W′ only after the main user value is already working.

10. **Refactor for readability and stability.**  
   Clean modules, naming, comments, and reactive flow so the app remains learnable and maintainable.

11. **Test with realistic user behavior.**  
   Check incomplete entries, bad inputs, repeated sessions, odd edge cases, and interpretation clarity.

## 10. Risks and Failure Modes

### Scientific risks
**Overclaiming the outputs.**  
If the app implies threshold detection or exact zone mapping, it loses credibility immediately.

**Treating model outputs as more real than the test data.**  
If fitted values overshadow the measured efforts, the app becomes scientifically weaker.

**Using unstable logic for training targets.**  
If the guidance appears precise but rests on weak assumptions, users will overtrust it.

### Practical risks
**The protocol feels too burdensome.**  
If the testing process feels confusing or unrealistic, users will not complete it.

**The outputs are too technical.**  
If non-expert users cannot understand what the chart means or what to do next, the product fails.

**Historical comparison is clumsy.**  
If users cannot save and compare testing blocks easily, retention drops.

**The app does not help with decisions.**  
A pretty chart without actionable interpretation is not enough.

### Technical risks
**Reactive logic becomes messy.**  
Poor Shiny structure can make the app brittle, hard to debug, and hard to extend.

**Data persistence is weak or confusing.**  
If saved sessions are unreliable or hard to manage, trust collapses.

**Too much complexity enters too early.**  
Adding advanced modeling, too many views, or too many options in V1 will slow the build and dilute the product.

### Strategic failure mode
The biggest risk is building a tool that looks scientific but is not actually useful, or looks useful but quietly overstates what it knows. The app must sit in the narrow band where it is both honest and practically valuable. That is the core product standard.
