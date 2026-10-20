# Power-Duration App: Scientific Bibliography

## Scope and provenance

This bibliography covers the scientific model families used in the app: power-law descriptions of endurance performance and the critical-power model. It was compiled after the original build to document the scientific context for the app's methods. It does not record which papers were consulted during development.

Titles, journals, years, volume and issue numbers, page ranges or article numbers, and DOIs were checked against publisher records, PubMed, or journal archives where available. Each entry includes a link for direct checking.

## Power-law and endurance-performance models

Kennelly, A. E. (1906). An approximate law of fatigue in the speeds of
racing animals. *Proceedings of the American Academy of Arts and
Sciences, 42*(15), 275--331. https://doi.org/10.2307/20022230

Riegel, P. S. (1981). Athletic records and human endurance. *American
Scientist, 69*(3), 285--290. https://pubmed.ncbi.nlm.nih.gov/7235349/

Péronnet, F., & Thibault, G. (1989). Mathematical analysis of running
performance and world running records. *Journal of Applied Physiology,
67*(1), 453--465. https://doi.org/10.1152/jappl.1989.67.1.453

Vandewalle, H. (2018). Modelling of running performances: Comparisons of
power-law, hyperbolic, logarithmic, and exponential models in elite
endurance runners. *BioMed Research International, 2018*, Article
8203062. https://doi.org/10.1155/2018/8203062

Drake, J. P., Finke, A., & Ferguson, R. A. (2024). Modelling human
endurance: Power laws vs critical power. *European Journal of Applied
Physiology, 124*(2), 507--526.
https://doi.org/10.1007/s00421-023-05274-5

## Critical power and W′ background

Monod, H., & Scherrer, J. (1965). The work capacity of a synergic
muscular group. *Ergonomics, 8*(3), 329--338.
https://doi.org/10.1080/00140136508930810

Moritani, T., Nagata, A., deVries, H. A., & Muro, M. (1981). Critical
power as a measure of physical work capacity and anaerobic threshold.
*Ergonomics, 24*(5), 339--350. https://doi.org/10.1080/00140138108924856

Hill, D. W. (1993). The critical power concept: A review. *Sports
Medicine, 16*(4), 237--254.
https://doi.org/10.2165/00007256-199316040-00003

Vanhatalo, A., Doust, J. H., & Burnley, M. (2007). Determination of
critical power using a 3-min all-out cycling test. *Medicine & Science
in Sports & Exercise, 39*(3), 548--555.
https://doi.org/10.1249/mss.0b013e31802dd3e6

Jones, A. M., Vanhatalo, A., Burnley, M., Morton, R. H., & Poole, D. C.
(2010). Critical power: Implications for determination of V̇O₂max and
exercise tolerance. *Medicine & Science in Sports & Exercise, 42*(10),
1876--1890. https://doi.org/10.1249/MSS.0b013e3181d9cf7f

Jones, A. M., & Vanhatalo, A. (2017). The "critical power" concept:
Applications to sports performance with a focus on intermittent
high-intensity exercise. *Sports Medicine, 47*(Suppl. 1), 65--78.
https://doi.org/10.1007/s40279-017-0688-0

Poole, D. C., Burnley, M., Vanhatalo, A., Rossiter, H. B., & Jones, A.
M. (2016). Critical power: An important fatigue threshold in exercise
physiology. *Medicine & Science in Sports & Exercise, 48*(11),
2320--2334. https://doi.org/10.1249/MSS.0000000000000939

## How these sources relate to the app

The app's main curve is a descriptive power-law fit to the five entered
power values. In the R code, it fits a straight line to log-transformed
power and log-transformed duration, then transforms predictions back to
watts. Kennelly and Riegel are foundational sources for power-law
descriptions of endurance performance. Vandewalle compares several
performance models, while Drake and colleagues directly compare
power-law and critical-power approaches to modeling endurance.

Péronnet and Thibault is relevant to the broader history of mathematical
performance modeling, but it presents a distinct model rather than the
exact log-log regression implemented in this app.

The app estimates critical power and W′ as secondary metrics using a
work-versus-time linear fit on the 3-, 6-, 10-, and 20-minute test
points. The Monod and Scherrer, Moritani and colleagues, Hill, Vanhatalo
and colleagues, Jones and colleagues, and Poole and colleagues papers
provide background on the critical-power concept and its interpretation.
The app's multi-test field protocol is not the same as the validated
3-min all-out protocol studied by Vanhatalo and colleagues.

## Limits of what this bibliography establishes

These references support the history and scientific context of the model
families. They do not, by themselves, validate this app's exact
five-test schedule, the 11-day testing window, its data-quality warning
cutoffs, its training-target ranges, or the individual predictions it
produces. Those app-specific decisions should be described as design
choices unless separate validation studies are conducted.

The app should continue to describe its outputs as field-based
estimates. Its fitted curve is descriptive, and its critical-power and
W′ values are estimates from the entered tests rather than direct
laboratory measurements.
