# Scientific Guardrails Memo

## 1. What the App Can Reasonably Claim

- The app can estimate a user’s power-duration profile from five maximal field tests.
- The app can show how power changes across short to longer efforts within this specific test set.
- The app can help users compare one testing block to another over time.
- The app can generate practical training targets anchored to observed performance, as long as those targets are framed as estimates.
- The app can identify broad strengths and weaknesses across the tested durations.
- The app can show whether a user improved, declined, or stayed similar at the tested durations compared with prior results.
- The app can fit a curve to the entered data and use that curve as a practical summary of the athlete’s current profile.
- The app can offer secondary modeled metrics, including critical power and W′, as estimated descriptors of the profile.
- The app can support coaching decisions by organizing field-test data into a usable format.
- The app can present the testing protocol and explain that results depend on pacing, effort quality, recovery, and test conditions.

## 2. What the App Must Not Claim

- It must not claim to directly measure lactate threshold.
- It must not claim to directly measure ventilatory threshold or gas-exchange thresholds.
- It must not claim to identify exact metabolic zones.
- It must not claim to diagnose aerobic or anaerobic capacity in a clinical or lab-grade sense.
- It must not claim that the modeled curve is a complete representation of the athlete’s physiology.
- It must not claim that five field tests fully capture performance across all durations.
- It must not claim that a training target is the user’s true optimal intensity.
- It must not claim that critical power is a hard biological breakpoint proven by this app.
- It must not claim that W′ is a direct measure of anaerobic battery, finite energy tank, or any literal physiological store.
- It must not claim that changes in the profile prove specific physiological adaptations without additional evidence.
- It must not claim precision that exceeds the quality of field testing.
- It must not imply that outputs are valid regardless of pacing errors, fatigue, illness, motivation, equipment differences, or protocol noncompliance.

## 3. Safe Terminology

- estimate
- estimated profile
- power-duration relationship
- field-test based estimate
- modeled curve
- observed test result
- best average power
- current testing block
- practical training target
- suggested training range
- reference point
- performance profile
- short-duration strength
- longer-duration strength
- comparison over time
- trend
- historical comparison
- secondary model output
- approximate
- useful guide
- coaching aid
- pacing-sensitive effort
- interpretation should consider context

## 4. Risky or Misleading Terminology

- exact threshold
- true threshold
- lactate threshold
- ventilatory threshold
- LT1
- LT2
- VT1
- VT2
- metabolic testing
- physiological diagnosis
- anaerobic battery
- finite energy tank
- exhausted W′
- maximal aerobic power, unless clearly defined and justified
- precise zone prescription
- personalized physiology engine
- validated metabolic zones
- definitive training zones
- optimal training intensity
- predicts race performance, unless heavily qualified
- proves aerobic improvement
- proves anaerobic limitation
- scientifically proven for you
- measures recovery status
- measures fatigue directly

## 5. User-Facing Uncertainty Language

- “This profile is an estimate based on your field-test results.”
- “These training targets are practical starting points, not exact physiological boundaries.”
- “Your results reflect performance on these test days under these conditions.”
- “Modeled values between tested durations are estimates, not directly measured outputs.”
- “Use these numbers as guides and adjust based on response, context, and coaching judgment.”
- “Critical power and W′ shown here are model-derived estimates.”
- “Small changes may reflect testing variability rather than meaningful fitness change.”
- “Pacing, fatigue, and equipment setup can affect the shape of your profile.”
- “This app does not directly measure lactate, breathing thresholds, or metabolic zones.”
- “Your historical comparison is most useful when tests are performed under similar conditions.”

## 6. Guardrails for Training Targets

Training targets should be framed as practical anchors, not physiological truths.

What that means in product terms:

- Targets should come from observed performance plus conservative modeling, not from grand claims about underlying biology.
- Targets should be shown as ranges when possible, not single magic numbers.
- The app should explain that targets are starting points to test in training, not commands to obey.
- Targets should be tied to workout purpose in plain language. Example: “hard repeat range,” “steady sustained work,” or “short severe efforts.” Do not pretend these map cleanly onto exact metabolic systems.
- The app should state that session quality, repeatability, and athlete response matter more than mathematical neatness.
- The app should avoid ultra-fine distinctions. A user does not need fake precision like 247 W versus 249 W.
- The app should make room for coach override and user judgment.
- The app should warn that targets are less trustworthy when recent testing was incomplete, poorly paced, or done under inconsistent conditions.
- The app should not auto-generate a full training plan from weak evidence. It can suggest anchor ranges, not claim to solve programming.

A good framing standard:
- “Here are useful training reference ranges derived from your recent tests.”

Not:
- “Here are your exact physiological zones.”

## 7. Guardrails for Critical Power and W′

CP and W′ can be included, but they must be demoted from “the truth” to “helpful secondary model outputs.”

Rules for safe inclusion:

- Present them as estimates derived from the fitted relationship, not directly measured quantities.
- Do not make them the main story of the app.
- Do not build the entire app logic around CP and W′.
- Do not present CP as the single defining marker of endurance ability.
- Do not present W′ as a literal tank, battery, or reservoir.
- Do not imply that CP cleanly separates all exercise domains for all users in all contexts.
- Do not calculate them and then act as if the rest of the profile is secondary. Your app’s primary asset is the broad power-duration profile.
- If shown, pair them with a short explanation such as: “Critical power and W′ are model-based summary metrics estimated from your test data. They can be useful reference points, but they are not direct measurements of specific physiological systems.”
- If the underlying fit is weak, unstable, or sensitive to one test result, the app should suppress or down-rank these outputs rather than displaying false authority.

## 8. Guardrails for Visualizations and Labels

Charts and labels can overclaim even when the body text is careful. Most product teams miss this.

What to avoid:

- Titles that imply diagnosis, such as “Your Physiology,” “Metabolic Profile,” or “Threshold Map.”
- Zone bands labeled with clinical or lab-style terms unless the app truly measures them, which it does not.
- Hard breakpoints on graphs that imply known biological transitions.
- Smooth curves displayed as if every point is measured truth.
- Precision-heavy annotations that suggest more certainty than the data support.
- Red-green “good/bad” labels that moralize normal profile differences.
- Visual metaphors for W′ that imply literal depletion of a measurable fuel store.
- Labels that convert estimates into facts, such as “true threshold,” “exact sustainable power,” or “max aerobic boundary.”

What is safer:

- “Power-duration profile”
- “Modeled estimate from recent tests”
- “Observed test points” versus “estimated curve”
- Dotted or visually distinct extrapolated regions if any values extend beyond the tested durations
- Clear marking of actual test points
- Notes that historical comparisons depend on similar testing conditions
- Tooltips that explain what is directly observed and what is modeled

## 9. Common Overclaim Failure Modes

- Treating modeled values between test durations as if they were measured.
- Translating a useful training estimate into a fake physiological label.
- Using CP or W′ as if they are ground truth because the math looks clean.
- Letting chart titles, legends, or tooltips say stronger things than the main copy.
- Confusing internal developer language with user-facing language. Teams often write “threshold” in the code and it leaks into the UI.
- Reporting tiny changes as meaningful improvement without accounting for test noise.
- Ignoring pacing error, especially in the 10-minute and 20-minute tests.
- Treating one bad or incomplete test as if it represents the athlete.
- Using fixed percent rules that imply universal validity across all users.
- Presenting suggested targets without context, caveats, or instructions for adjustment.
- Building “insight” text that infers physiology from pattern shape without enough support.
- Using authoritative UX patterns, badges, rankings, or confidence-free numbers that imply certainty the app does not have.

## 10. Final Non-Negotiable Rules

1. Every primary output must be framed as field-test based and estimated unless it is a directly entered test result.
2. The app must never claim to directly measure lactate threshold, ventilatory thresholds, metabolic zones, or any other lab-grade physiological marker.
3. The app must clearly distinguish observed test points from modeled values.
4. Training targets must be presented as practical reference ranges or starting points, not exact prescriptions or physiological truths.
5. Critical power and W′ must remain secondary summary metrics, not the central logic or main marketing claim of the app.
6. No copy, chart title, tooltip, annotation, or label may imply diagnostic certainty that the testing approach cannot support.
7. The app must not infer specific physiological mechanisms from profile changes unless that inference is explicitly softened and context-bound.
8. Historical comparisons must be qualified by test consistency, pacing quality, and testing conditions.
9. When the data quality is weak, incomplete, or obviously unstable, the app must reduce confidence, add caution, or suppress stronger interpretations.
10. When choosing between clarity and scientific ambition, choose clarity. When choosing between usefulness and overclaiming, choose usefulness with restraint.
