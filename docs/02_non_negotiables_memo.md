## 1. Non-Negotiable Design Principles

**1. The app must estimate, not diagnose.**  
Every output must be framed as a field-based performance estimate from completed tests, not a direct measurement of physiology.

**2. The five tests are the foundation of the app.**  
The app’s core value comes from the 45 s, 3 min, 6 min, 10 min, and 20 min efforts. Features that weaken, bypass, or obscure that structure should not be added.

**3. Practical usefulness beats theoretical sophistication.**  
When forced to choose, prefer outputs that help an athlete train better this week over outputs that are more complex but harder to use or explain.

**4. Scientific honesty beats marketing appeal.**  
The app must never use stronger language than the evidence supports, even if simpler or bolder claims would sound more impressive.

**5. Simplicity must not come at the cost of distortion.**  
The interface should be easy for non-experts, but simplification cannot hide uncertainty, mislabel estimates as facts, or encourage bad testing practice.

**6. The power-duration profile is the primary product.**  
The main output is the shape and practical meaning of the user’s power-duration relationship over relevant durations. Secondary metrics must remain secondary.

**7. Historical comparison must be built around context, not just numbers.**  
The app should compare tests across time, but it must preserve dates, protocol conditions, and enough context to keep comparisons honest.

**8. Training guidance must stay anchored to observed performance.**  
Recommendations should be derived from the tested profile and clearly described as practical targets or ranges, not hidden physiological truths.

**9. Bad input must not produce false confidence.**  
If the test set is incomplete, inconsistent, obviously flawed, or entered incorrectly, the app should warn, limit outputs, or refuse certain interpretations.

**10. Every major screen must answer a real user question.**  
No page, chart, metric, or paragraph should exist unless it helps users do one of these things: test correctly, understand their profile, compare results, or train from it.

**11. Learning-friendly code is a product requirement.**  
The Shiny build should be readable, modular, and teachable. Clever but opaque code is a failure, even if it works.

**12. Trust is more important than feature count.**  
A smaller app that users believe and can use correctly is better than a larger app full of fragile models, speculative claims, and confusing outputs.

## 2. Scientific Non-Negotiables

**Do not claim direct measurement of internal physiology.**  
The app must not claim to measure lactate threshold, ventilatory thresholds, VO2 kinetics, muscle fiber type, aerobic capacity, anaerobic capacity, or metabolic zones directly from these tests.

**Do not present model outputs as biological facts.**  
Critical power, W′, fitted curves, inferred breakpoints, and derived targets must be labeled as estimates or model-based interpretations.

**Do not imply precision beyond the data quality.**  
The app should avoid false exactness. Small decimal-heavy outputs, narrow prescriptions, or overly specific thresholds should not be shown unless clearly justified.

**Do not treat one modeling framework as the truth.**  
Critical power and W′ can be included, but the app must not be built around the assumption that those metrics fully explain performance across all tested durations.

**Do not ignore pacing and execution error.**  
A maximal average-power test is only useful if it was performed well. The app must acknowledge that poor pacing, fatigue, motivation, and setup can distort results.

**Do not overgeneralize from five tests.**  
Five field efforts can support a useful profile, but not a complete physiological map. The app must stay within what those data can reasonably support.

**Do not prescribe training as if uncertainty does not exist.**  
Targets should be framed as working estimates, practical anchors, or starting points for training, not exact individualized truths.

**Do not hide the difference between observed and inferred values.**  
Actual tested values and modeled estimates must always be visually and verbally distinguishable.

## 3. UX Non-Negotiables

**Use plain English before technical language.**  
Users should be able to understand every key screen without exercise science training. Technical terms can appear, but only with clear explanation.

**Show the user what is real, what is estimated, and what is suggested.**  
The app must consistently separate entered test results, modeled profile values, and training guidance.

**Do not overload the main interface.**  
The primary workflow should be obvious: enter tests, view profile, compare history, see training guidance, access protocol. Anything else is secondary.

**Warnings must be actionable.**  
If the app flags missing tests, suspicious values, or low-confidence outputs, it must tell the user what to do next.

**The protocol page must be operational, not decorative.**  
It should tell users exactly how to perform the tests, in what order, under what conditions, and what mistakes to avoid.

**Charts must help decisions, not just look impressive.**  
Every graph should answer a useful question. Avoid dense, multi-layered plots that require expert interpretation.

**Users must not need to understand the math to use the app well.**  
The interface should expose enough explanation for trust without requiring the user to learn the modeling framework.

**Comparisons must be easy to interpret.**  
When historical tests are shown, users should be able to see whether they improved, worsened, or changed shape across durations without hunting through numbers.

**Do not punish new users with complexity.**  
The default experience should work for a first-time user with minimal setup, while still allowing deeper inspection for advanced users.

## 4. Data and Testing Non-Negotiables

**Require clear entry of all five target durations.**  
The app should be structured around the fixed protocol, not arbitrary duration entry as the default mode.

**Preserve raw test values.**  
Original entered results must be stored as entered. Derived values should never overwrite raw data.

**Store date and session identity for every test set.**  
Historical comparison is only credible if each profile is tied to a clear testing window or session label.

**Validate inputs aggressively.**  
The app must check for missing values, impossible values, unit errors, and obvious entry mistakes before generating profile outputs.

**Flag incomplete or low-quality test sets.**  
If one or more tests are missing, or if the pattern strongly suggests poor execution, the app should reduce confidence and limit interpretation.

**Keep observed and modeled data separate in storage and display.**  
This protects both scientific honesty and future debugging.

**Do not silently smooth away contradictions.**  
If the data are messy, the app should show that the data are messy. Modeling should not create a false impression of perfect coherence.

**Support repeat testing without destroying comparability.**  
Users need a stable way to log multiple test rounds over time using the same protocol structure.

**Protocol consistency matters.**  
The app should explicitly encourage similar equipment setup, environmental conditions, and pre-test preparation across testing windows.

## 5. Engineering Non-Negotiables

**Build the app around clear modules.**  
Separate logic for data entry, validation, modeling, visualization, history, protocol, and guidance. Do not mix everything into one server block.

**Keep business logic out of the UI where possible.**  
Calculations, validation rules, and guidance generation should live in reusable functions, not inline reactive clutter.

**Name things for readability, not brevity.**  
Object names, function names, and file names should make sense to a learner reading the code later.

**Every derived output must be traceable.**  
A future reviewer should be able to identify where each metric, chart, and training target comes from in the code.

**Fail safely.**  
If calculations break or data are invalid, the app should show clear fallback messages rather than erroring out or returning misleading numbers.

**Do not hard-code hidden assumptions.**  
Durations, labels, validation rules, and explanatory text should be explicit and easy to inspect or revise.

**Use reactive logic sparingly and intentionally.**  
Only make something reactive when it truly needs to respond to user input. Uncontrolled reactivity makes Shiny apps fragile and hard to learn from.

**Prefer base R or widely understood packages unless a dependency clearly adds value.**  
Do not build the app on obscure packages that increase maintenance burden without strong benefit.

**Document decisions in the code.**  
Comments should explain why a rule exists, not restate obvious syntax.

**The first working version should be stable before it is clever.**  
A robust version-one app with limited scope is preferable to an ambitious but brittle build.

## 6. Priority Conflicts

**Simplicity vs scientific accuracy**  
Scientific accuracy wins.  
The interface should be simplified, but never by making unsupported claims or hiding uncertainty.

**Practical training guidance vs physiological restraint**  
Physiological restraint wins on wording; practical guidance wins on format.  
The app should still give useful targets, but those targets must be described as estimates grounded in performance, not as direct threshold measurements.

**More metrics vs user comprehension**  
User comprehension wins.  
If a metric adds cognitive load without changing action, remove it or bury it in an advanced section.

**Smooth model outputs vs messy real data**  
Messy real data wins.  
The app can model the profile, but it must not conceal poor data quality or contradictions in the observed tests.

**Feature richness vs trustworthiness**  
Trustworthiness wins.  
Do not add features that users cannot understand, validate, or use correctly.

**Code cleverness vs maintainability**  
Maintainability wins.  
Readable, teachable code is mandatory for this project.

**Automation vs user control**  
User control wins where interpretation is involved.  
The app can automate calculations and visuals, but users should still be able to see what data were used and what assumptions shaped the output.

**Secondary metrics vs primary product focus**  
Primary product focus wins.  
If critical power, W′, or any other estimate starts dominating the interface, it is a design error.

This memo should function as a rejection tool: if a future feature, sentence, chart, or code choice violates one of these rules, it should not ship.
