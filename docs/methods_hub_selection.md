# Choosing OHBM hubs for the whole neuroimaging community: methods

This document explains, step by step, how scripts `R/11_openalex_community.R`, `R/12_participation_model.R` and `R/13_equitable_hub_selection.R` choose conference hub locations. It also says why each choice was made and how to explain it. Every section has a short **In plain words** summary, then the formal version, then what we found, then references.

## Overview

The earlier scripts (`05_`–`10_`) ask: *where should hubs be to minimise the travel of the people who came to OHBM 2023–2025?* That question has three blind spots.

1. **Who came depends on where the meeting was.** Attendance from the host region surges (Australia/New Zealand sent 386 people to Brisbane in 2025, against 65 to Montreal in 2023). Optimising on past attendees bakes in past host choices.
2. **Moving a meeting changes who comes.** A hub closer to a community brings more of its members. That is good for inclusion, but each extra traveller adds emissions.
3. **Footprint is not the only goal.** A plan that minimises CO2 by keeping a meeting where few people can reach it is not acceptable.

So the pipeline does three things:

| Script | Question | Method |
|---|---|---|
| `11_` | Where is the neuroimaging community? | Count articles in core neuroimaging journals per institution (OpenAlex) |
| `12_` | How does attendance fall with distance? | Fit a distance-decay model of participation on the 2023–2025 meetings |
| `13_` | Which hub sets give the best footprint/access trade-off? | Search hub sets, trace the Pareto front of footprint vs. coverage |

---

## M0. Notation

- **Institutions** $i = 1,\dots,N$ ($N = 4{,}764$). Community weight $C_i$ = number of articles with an author at $i$.
- **Candidate host cities** $j = 1,\dots,M$: the 313 cities with ≥ 1 million inhabitants in `maps::world.cities`.
- $d_{ij}$: great-circle (haversine) distance in km between institution $i$ and city $j$.
- **Hub set** $H$: a set of $k$ cities. Everyone goes to the nearest hub: $d_i(H) = \min_{j \in H} d_{ij}$. This is the same rule as `calculate_multicentric_footprint()` in `R/utils.R`.
- $\phi(d)$: round-trip emissions (kg CO2e) for a trip of $d$ km, from `calculate_footprint()` in `R/utils.R`. It is zero below 100 km, then 0.517, 0.375 or 0.304 kg/km for short (< 1,000 km), medium (< 3,500 km) and long haul.

---

## M1. Measuring the community (`11_`)

**In plain words.** The community is measured by where neuroimaging papers are written. We count, for every research institution in the world, how many articles in the main neuroimaging journals it took part in over five years.

**Formal version.**
- **Journals**, identified by ISSN-L: NeuroImage, Human Brain Mapping, NeuroImage: Clinical, Imaging Neuroscience, Brain Connectivity, Network Neuroscience, Cerebral Cortex, Brain Topography, Neuroinformatics.
- **Scope:** research articles only (OpenAlex `type:article`, which drops editorials and errata) published 2020–2024. That is 11,168 articles.
- **Full counting:** a paper with authors at three institutions counts once for each. This measures activity at each institution, not a share of the world's papers.
- **Institution data:** coordinates and country come from OpenAlex. For 140 institutions with coordinates but no country (e.g. Charité, Sunnybrook), the country is taken from the map position.
- **Regions:** institutions and attendees are grouped into UN M49 regions and subregions with `countrycode`.
- **Check:** Harvard's count (620) equals an independent direct query.

**What we found** (`output/reports/community_vs_attendees_by_region.csv`, `output/figures/community_vs_attendees_by_region.png`). Compared with its share of articles, each subregion's share of 2023–25 OHBM attendees was:

| Subregion | Share of articles | Share of attendees | Ratio |
|---|---|---|---|
| Northern America | 29.8% | 39.1% | 1.31 |
| Western Europe | 26.4% | 14.8% | 0.56 |
| Eastern Asia | 18.2% | 25.4% | 1.39 |
| Northern Europe | 11.8% | 7.3% | 0.62 |
| Southern Europe | 6.6% | 2.2% | 0.33 |
| Australia & NZ | 3.0% | 6.9% | 2.30 |

The three host regions (North America, East Asia, Australia) are over-represented and Europe is under-represented. That is the host-location effect we want to model, not to copy.

**Limits.**
- Publication output is itself unequal. Places with less funding, less English-language publishing or poorer indexing are under-counted.
- Two countries with OHBM attendees have no institution in these journals: Rwanda (8 attendees) and Nepal (1). They are left out of the model; that gap is an equity finding in itself.
- "Coverage" below therefore means coverage of the *publishing* community. It is a lower bound on true inequity.
- Umbrella organisations (CNRS, Inserm, Max Planck Society, Chinese Academy of Sciences) are placed at their head office. That is a few hundred km off for some labs: small compared with intercontinental distances, but not zero.

**References.**
- Priem, J., Piwowar, H., & Orr, R. (2022). *OpenAlex: A fully-open index of scholarly works, authors, venues, institutions, and concepts.* arXiv:2205.01833.
- OpenAlex API documentation: https://docs.openalex.org

---

## M2. How attendance falls with distance (`12_`)

**In plain words.** Every institution sends attendees in proportion to its research output, but fewer the farther away the meeting is. We estimate *how much* fewer from the three past meetings. For each country we know how many people came to Montreal, to Seoul and to Brisbane, and how far its institutions were from each.

### The model

For country $c$ and year $y$ (host Montreal, Seoul or Brisbane), let $A_{cy}$ be the number of attendees. We model it as a count:
$$A_{cy} \sim \text{Poisson}(\mu_{cy}), \qquad \mu_{cy} = e^{\alpha_y} \sum_{i \in c} C_i \, e^{\beta\, g(d_{iy})}$$

- $e^{\beta g(d)}$ is the **relative participation** of an institution at distance $d$ (1 at the venue). It is the same for every institution.
- $\beta < 0$ is the **decay parameter**, the quantity of interest.
- $e^{\alpha_y}$ is the **year level**: attendees per article for that meeting overall (venue appeal, post-COVID recovery, etc.).
- **Why sum over institutions** rather than use one distance per country: in the Montreal year, Boston is 400 km away and Los Angeles 4,000 km. A country-average distance would hide exactly the variation we want to measure. The model predicts each institution's contribution and only compares country *totals* with the data.

**Why Poisson?**
- $A_{cy}$ is a count, often 0 or small, and its spread grows with its size. Poisson regression is the standard model for counts and rates.
- With the community size inside $\mu$ (an *offset*, see Agresti ch. 3), it models a **rate**: attendees per article.
- A linear regression on rates would give tiny countries the same weight as large ones, and would mishandle zeros.

**Two shapes of decay** ($g$ is `distance_decay_g()` in `R/utils.R`):
- **power:** $g(d) = \log(1 + d/1000)$, so participation $\propto (1 + d/1000)^\beta$;
- **exponential:** $g(d) = d/1000$, so participation $\propto e^{\beta d/1000}$.

These are the two classic forms of spatial-interaction ("gravity") models. The power form fits much better (Poisson AIC 4,642 vs. 5,208), so it is used throughout.

**How it is fitted.**
- The parameters are found by maximum likelihood, numerically (`optimize`/`optim` in R).
- Given $\beta$, each $\alpha_y$ has a closed form: $e^{\alpha_y} = \sum_c A_{cy} / \sum_c S_{cy}(\beta)$, where $S_{cy} = \sum_{i\in c} C_i e^{\beta g(d_{iy})}$. So only $\beta$ needs a numerical search.
- A by-product: each year's predicted total equals the observed total exactly.
- Standard errors come from the curvature of the log-likelihood (the Hessian).

### Overdispersion, and why not the negative binomial

Country counts vary much more than Poisson allows. The **dispersion** (Pearson χ² / degrees of freedom) is 31, where Poisson expects 1.

- **Standard fix, quasi-Poisson:** keep the same estimates but multiply standard errors by $\sqrt{31} \approx 5.6$. All CIs below are computed this way.
- **Negative binomial, tried and rejected for prediction:** it models the extra variance explicitly (variance $\mu + \mu^2/\theta$). With $\theta \approx 1$ it gives so much weight to small countries that its fitted totals are 48% above the observed ones. It is kept in the summary table for transparency.

### The key robustness check: country fixed effects

The model above compares countries with each other. That is risky. Europe is far from all three hosts, and it also sends fewer attendees per paper than North America for other reasons (cost, competing meetings, habits). A model that compares countries cannot tell these apart, and would attribute the gap to distance.

**The fix:** give each country its own level $\gamma_c$ instead of using its paper count:
$$\mu_{cy} = e^{\gamma_c + \alpha_y} \sum_{i \in c} C_i \, e^{\beta g(d_{iy})}$$

- Now $\beta$ is estimated **only from how each country's attendance changed as the meeting moved** between Montreal, Seoul and Brisbane. Any stable country trait cancels out.
- The $\gamma_c$ have a closed form given $(\alpha, \beta)$ and are "profiled out", so the fit is still cheap.
- Countries that never attended carry no information here, and drop out (50 countries remain).

### What we found

Power form, 95% CIs with quasi-Poisson standard errors (`output/reports/participation_model_summary.csv`):

| Model | β | 95% CI | Dispersion |
|---|---|---|---|
| Offset model (between and within countries) | −0.85 | −1.00 to −0.70 | 31 |
| **Country fixed effects (within countries only)** | **−0.57** | **−0.66 to −0.49** | **7.3** |
| Simple GLM on country-mean distance (cross-check) | −0.91 | −1.07 to −0.75 | — |

- **Decay is strong under every model.** Its strength is uncertain within a range.
- **The fixed-effects model fits much better** (dispersion 7 vs. 31), because country propensities explain most of the extra variation.
- **The two CIs do not overlap.** Part of the steeper offset estimate is the confounding described above.

What the two slopes mean, as relative participation per article:

| Distance to meeting | 1,000 km | 2,000 km | 5,000 km | 10,000 km | 15,000 km |
|---|---|---|---|---|---|
| β = −0.57 (primary) | 0.67 | 0.53 | 0.36 | 0.25 | 0.21 |
| β = −0.85 (sensitivity) | 0.56 | 0.39 | 0.22 | 0.13 | 0.09 |

So with the primary slope, a lab 10,000 km away sends about **one quarter** as many people per paper as a lab next to the venue.

### Out-of-sample validation

The model is fitted on two meetings and used to predict how the third meeting's attendees split across UN subregions. Error = **fraction of attendees put in the wrong subregion** (total variation distance, $\tfrac12\sum|p - q|$).

| Held-out meeting | Decay model, fixed-effects slope | Decay model, offset slope | Past attendance shares | Community shares |
|---|---|---|---|---|
| 2023 Montreal | 11% | 6% | 33% | 27% |
| 2024 Seoul | 14% | 18% | 20% | 21% |
| 2025 Brisbane | 21% | 16% | 25% | 30% |

- **Both decay models beat both baselines** that ignore the host location, every time.
- **Neither slope wins clearly.** Three meetings cannot discriminate between β = −0.57 and −0.85.

### Decision

- **Primary slope:** β = −0.57 (fixed effects). It has the cleanest identification, and it is conservative: it predicts less induced demand.
- **Sensitivity slope:** β = −0.85. Every result in `13_` is computed under both.
- **Level for hypothetical meetings:** $\kappa$ = average over the three years of $e^{\alpha_y}$, re-estimated with $\beta$ fixed. Prediction for any hub set:
$$E_i(H) = C_i\, \kappa\, (1 + d_i(H)/1000)^{\beta}$$

**Known misfits** (`12_` exploration table).
- **The host country surges more than the curve predicts.** Australia, Brisbane 2025: 386 observed vs. 183 fitted. "Same country" brings more than short distance alone (no visa, no international flight, national networks).
- **Europe attends less than its paper count predicts:** e.g. Southern Europe 71 vs. 146 (2023).
- Both mean the model describes **what attendance would be if it depended only on community size and distance**. That is the planning question, not a forecast of OHBM's actual registration numbers.
- A same-country term is a natural extension once more meetings are available.

**References.**
- Agresti, A. (2019). *An Introduction to Categorical Data Analysis*, 3rd ed., Wiley. Ch. 3 (Poisson loglinear models, rates and offsets, overdispersion).
- Rodríguez, G. *Generalized Linear Models*, lecture notes, Princeton. Ch. 4, "Poisson Models for Count Data", free at https://grodri.github.io/glms/notes/ (formerly data.princeton.edu/wws509).
- McCullagh, P., & Nelder, J. A. (1989). *Generalized Linear Models*, 2nd ed., Chapman & Hall. §6.2 (quasi-likelihood and overdispersion).
- Fotheringham, A. S., & O'Kelly, M. E. (1989). *Spatial Interaction Models: Formulations and Applications.* Kluwer. On power vs. exponential distance decay.
- Cameron, A. C., & Trivedi, P. K. (2013). *Regression Analysis of Count Data*, 2nd ed., Cambridge. Ch. 9 (panel count models with fixed effects).

---

## M3. What a hub set is judged on (`13_`)

**In plain words.** For any set of hub cities we compute two numbers:
- **footprint:** how much CO2 everyone's travel would emit, counting the extra people who come because a hub is close;
- **coverage:** for each world region, the share of its community with a hub within a medium-haul flight, averaged over regions so that **each region counts equally**, whatever its size.

**Formal version.**
- **Expected footprint** (t CO2e):
$$F(H) = \frac{1}{1000}\sum_i E_i(H)\, \phi\big(d_i(H)\big)$$
- **Regional coverage.** For each region $r$, the share of its community within $D = 3{,}500$ km of a hub:
$$Q_r(H) = \frac{\sum_{i \in r} C_i\, \mathbb{1}[d_i(H) \le D]}{\sum_{i \in r} C_i}$$
- **The equity objective** is the mean over the six regions $R$ = {Africa, Latin America & Caribbean, Northern America, Europe, Asia, Oceania} (UN M49, Americas split):
$$Q(H) = \frac{1}{|R|}\sum_{r \in R} Q_r(H)$$
  $D$ = 3,500 km is the medium/long-haul boundary already used in `calculate_footprint()` (roughly a ≤ 4–5 h flight).
- **Why average over regions rather than count papers?**
  - We first used the paper-weighted share $\sum_i C_i \mathbb{1}[d_i \le D] / \sum_i C_i$.
  - Africa and Latin America hold about 1.4% of the articles, so plans reached 98% coverage while leaving those regions at 0–12%: the measure rewarded covering the large communities.
  - Averaging over regions gives every region the same weight, so reaching Latin America is worth as much as reaching Europe. This is a deliberate normative choice, in the spirit of "equal access" rather than "maximal throughput".
  - The paper-weighted share is still reported (`community_coverage`, also at 1,000 and 2,000 km), but not optimised.
- **Also reported:**
  - expected attendance $\sum_i E_i(H)$;
  - footprint per attendee;
  - coverage of the **worst-served region** (UN M49 regions, with the Americas split into Northern and Latin America), a "max-min" (Rawlsian) equity view.

**Why coverage, and why not just minimise footprint?**
- Because attendance falls with distance, a meeting nobody can reach has few attendees and therefore a *small* footprint. Minimising $F$ alone could reward exclusion.
- Coverage is the *maximal covering* idea from facility location: judge a service by how many people have it within an acceptable distance.

**References.**
- Church, R., & ReVelle, C. (1974). The maximal covering location problem. *Papers of the Regional Science Association*, 32, 101–118.
- Marsh, M. T., & Schilling, D. A. (1994). Equity measurement in facility location analysis: A review and framework. *European Journal of Operational Research*, 74(1), 1–17.

---

## M4. Rules every hub set must satisfy (city size)

1. **Eligibility:** hubs are cities with ≥ 1 million inhabitants, as a proxy for venue capacity and air connections (313 cities).
2. **Minimum catchment:** each hub must expect ≥ 150 attendees to be worth running. **Exception:** hubs in under-represented regions (Africa, Latin America & Caribbean, Southern Asia, South-eastern Asia) need only ≥ 50. Below that, a hub cannot be justified even as a deliberate investment in inclusion. Formally, the floor $m_j$ depends on the hub's region, and each hub needs $\sum_{i \to j} E_i \ge m_j$.
3. **No shrinking:** total expected attendance ≥ today's mean (2,687 per year). This blocks "low footprint by exclusion" outright.

---

## M5. Searching for good hub sets: greedy + Teitz–Bart swaps (`13_`)

**In plain words.**
- **Start:** pick the best city, then the best second city given the first, and so on.
- **Improve:** try every way of replacing one hub with another city, and keep the best swap. Repeat until no single swap helps.
- **Check:** do the whole thing again from random starting sets, to see whether we always end up at the same answer.

**Why not try every combination?**
- Choosing $k$ of 313 cities to minimise a sum of nearest-hub costs is the **p-median problem** (Hakimi 1964), which is NP-hard: no known method solves every instance quickly.
- There are $\binom{313}{6} \approx 1.2 \times 10^{12}$ six-hub sets.

**The algorithm** (Teitz & Bart 1968, "vertex substitution"), the textbook heuristic for p-median problems:
1. **Greedy start:** $H = \emptyset$; repeat $k$ times: add the feasible city that most improves the score.
2. **Swap phase:** for every hub $r \in H$ and every city $j \notin H$, evaluate $H - \{r\} + \{j\}$. If the best of these improves the score, apply it and repeat.
3. **Stop** when no single swap improves the score. The result is a *local optimum*.

Script `10_` stopped after step 1, which is why its choices could not be revised (e.g. Bergen).

**Speed trick.**
- All swaps for a given $r$ are scored at once. Institution $i$ moves to candidate $j$ only if $d_{ij}$ is shorter than its current distance.
- So the new footprint for *every* candidate is the current one, plus the sum of changes over the institutions that move: one matrix operation per $r$.

**How we know the heuristic is good** (`output/reports/equitable_hub_search_checks.csv`).
- **Exact check:** for $k = 2$, all $\binom{313}{2} = 48{,}828$ pairs were scored. The heuristic found the exact optimum in every case tested (3 values of λ × 2 scenarios).
- **Random restarts:** 5 random feasible starts per search, 60 restarts per $k$ and scenario. In the final run, no restart found a better solution than the reported one. All reached it up to $k = 5$; for $k = 6$, 45 of 60 (primary) ended in worse local optima.
  - An earlier run, with the paper-weighted coverage, did find better solutions from random starts at $k = 4$ (14 of 60). Local optima are real from $k \ge 4$.
  - That is why the Pareto front is built from **all** searches, restarts included, and why the bootstrap keeps the best of three starts for $k \ge 4$.

**References.**
- Hakimi, S. L. (1964). Optimum locations of switching centers and the absolute centers and medians of a graph. *Operations Research*, 12(3), 450–459.
- Teitz, M. B., & Bart, P. (1968). Heuristic methods for estimating the generalized vertex median of a weighted graph. *Operations Research*, 16(5), 955–961.
- Daskin, M. S. (2013). *Network and Discrete Location: Models, Algorithms, and Applications*, 2nd ed., Wiley. Ch. 6 (median problems, greedy and exchange heuristics).
- Larson, R. C., & Odoni, A. R. (1981). *Urban Operations Research.* Prentice-Hall. Ch. 6 on facility location. Free online: https://web.mit.edu/urban_or_book/www/book/
- Church, R. L., & Murray, A. T. (2009). *Business Site Selection, Location Analysis, and GIS.* Wiley. The most accessible introduction.

---

## M6. The trade-off: Pareto front

**In plain words.** Lower footprint and better access pull in different directions. Instead of picking one compromise, we list all hub sets that are not beaten on both counts at once. On that list, getting more coverage always costs some footprint. The **knee** is where the curve bends: past it, more access becomes expensive.

**Formal version.**
- **Dominance:** $H_1$ *dominates* $H_2$ if $F(H_1) \le F(H_2)$ and $Q(H_1) \ge Q(H_2)$, with at least one strict.
- **The Pareto front** is the set of non-dominated hub sets: every point on it is a defensible choice, and every point off it is beaten by one on it.

How it is traced, for each $k = 1..6$:
1. **Weighted sums.** Run the search on $\min\; F(H)/F_{\text{ref}} - \lambda\, Q(H)$ for λ = 0, 0.05, 0.1, 0.2, 0.3, 0.5, 0.75, 1, 1.5, 2, 3, 5.
   - $F_{\text{ref}}$ is the footprint of the best single hub, so both terms are unit-free.
   - λ = 0 means "footprint only"; large λ means "coverage first".
2. **Archive.** A weighted sum can miss parts of the front that bulge inward (Das & Dennis 1997). So **every feasible hub set evaluated during all searches** is kept, and the front is taken from that archive.
3. **ε-constraint cross-check.** Also run $\min F$ subject to $Q \ge \varepsilon$, for ε = 0.30 to 0.95 (Haimes et al. 1971). This finds front points of any shape.
4. **ε-dominance.** Coverage differences under 0.5 percentage points are treated as ties: coverage is binned in steps of 0.005, only the lowest footprint is kept per bin, and the front is taken across bins (Laumanns et al. 2002). Without this, the front filled up with hub sets that bought a negligible coverage gain (e.g. +0.01 points) with a large footprint increase (up to +1 kt), which made the knee unstable.
5. **Knee.** After scaling $F$ and $Q$ to [0, 1] on each front, the knee is the point farthest from the straight line joining the two ends of the front. It is highlighted as a candidate, not as "the answer".

**References.**
- Ehrgott, M. (2005). *Multicriteria Optimization*, 2nd ed., Springer. Ch. 1–4.
- Laumanns, M., Thiele, L., Deb, K., & Zitzler, E. (2002). Combining convergence and diversity in evolutionary multiobjective optimization. *Evolutionary Computation*, 10(3), 263–282. On ε-dominance.
- Das, I., & Dennis, J. E. (1997). A closer look at drawbacks of minimizing weighted sums of objectives for Pareto set generation in multicriteria optimization problems. *Structural Optimization*, 14, 63–69.
- Haimes, Y. Y., Lasdon, L. S., & Wismer, D. A. (1971). On a bicriterion formulation of the problems of integrated system identification and system optimization. *IEEE Transactions on Systems, Man, and Cybernetics*, 1(3), 296–297.

---

## M7. How stable are the recommendations?

**In plain words.** We redo the whole selection 100 times, each time with a slightly different version of the data and of the decay curve, and count how often each city is picked. A city picked in more than 80% of runs is a robust recommendation; one picked in 30% is a coin toss between neighbouring cities.

**Formal version.** Each bootstrap replicate:
1. resamples the 4,764 institutions with replacement (non-parametric bootstrap of the community);
2. draws $\beta$ from $N(\hat\beta, \widehat{SE}^2)$ (parameter uncertainty), and re-calibrates $\kappa$ so the three real hosts reproduce today's mean attendance;
3. re-runs the search at each $k$ with the λ whose solution was nearest the knee. For $k \ge 4$, two random starts are added to the greedy start and the best result is kept, since local optima appear there (M5).

**References.** Efron, B., & Tibshirani, R. J. (1993). *An Introduction to the Bootstrap.* Chapman & Hall.

---

## Results

Primary decay slope (β = −0.57) unless stated. Files: `output/reports/equitable_hub_pareto.csv`, `equitable_hub_benchmarks.csv`, `equitable_hub_bootstrap.csv`, `equitable_hub_search_checks.csv`; figures `output/figures/equitable_hub_*.png`.

### Search quality
- **Exact check:** for $k = 2$, the heuristic matches the exhaustive optimum in all 6 checks.
- **Restarts:** out of 60 random restarts per $k$, none found a better solution than the reported one. All reached it for $k \le 5$, and 45 of 60 for $k = 6$; the rest ended in worse local optima.

### Benchmarks under the same model
Expected values per meeting. "Regional coverage" is the mean over 6 regions; "community coverage" is the paper-weighted share.

| Configuration | Footprint (t CO2e) | Attendance | Regional coverage | Community coverage | Smallest hub |
|---|---|---|---|---|---|
| Montreal alone (2023 host) | 4,615 | 3,475 | 15% | 26% | — |
| Seoul alone (2024 host) | 5,601 | 2,804 | 15% | 18% | — |
| Brisbane alone (2025 host) | 7,323 | 1,952 | 16% | 3% | — |
| Past four hosts as hubs | 2,537 | 5,531 | 64% | 93% | 174 |
| Six hubs from `07_` (adds São Paulo, Johannesburg) | 2,498 | 5,566 | 91% | 94% | **9** (Johannesburg; infeasible) |

### Knee solutions (best footprint/coverage compromise for each number of hubs)

| Hubs | Cities | Footprint (t) | Attendance | Regional coverage | Community coverage |
|---|---|---|---|---|---|
| 1 | Belgrade | 4,407 | 3,672 | 20% | 47% |
| 2 | Brussels, Chicago | 3,020 | 5,204 | 38% | 76% |
| 3 | Brussels, Houston, Nanchang | 2,501 | 5,571 | 54% | 95% |
| 4 | Brussels, Chicago, Melbourne, Nanchang | 2,156 | 5,975 | 70% | 98% |
| 5 | Beijing, Brussels, Melbourne, Santa Cruz (Bolivia), Toronto | 1,963 | 6,192 | 83% | 97% |
| 6 | Beijing, Brussels, Los Angeles, Melbourne, Santa Cruz, Toronto | 1,831 | 6,340 | 84% | 99% |

### Bootstrap stability (share of 100 replicates in which a city is chosen)

| Hubs | Robust (≥ 80%) | Likely (50–80%) | Region needed, city open |
|---|---|---|---|
| 1 | Brussels 99% | | |
| 2 | Brussels 99% | Chicago 63% | North America: Chicago / Toronto |
| 3 | Brussels 92% | Chicago 72% | East Asia: Nanchang 45%, Beijing 27%, Hangzhou… |
| 4 | Brussels 99% | Melbourne 69%, Chicago 59%, Beijing 54% | |
| 5 | Brussels 100% | Melbourne 69%, New York 59%, Beijing 58% | South America: Santa Cruz 37% |
| 6 | Brussels 92%, New York 89%, Los Angeles 88% | Melbourne 70%, Beijing 60% | South America: Santa Cruz 36% |

### What the results say
1. **Moving from one meeting to 3–4 hubs halves the expected footprint, while the expected attendance roughly doubles.** One city: 4.4–7.3 kt. Four hubs: 2.2 kt, with about 6,000 attendees instead of about 2,700. Footprint per attendee drops from about 1.3–3.8 t to about 0.36 t.
2. **Optimised hubs beat the past-hosts configuration** at equal hub count: 4 hubs give 15% less CO2 and 6 points more regional coverage than Glasgow + Montreal + Seoul + Brisbane.
3. **The pattern is stable, even where individual cities are not.**
   - A hub in Western Europe (Brussels) is chosen in nearly every replicate.
   - North America gets one hub (Chicago/Toronto), or two coastal hubs from $k = 6$ (New York, Los Angeles).
   - East Asia (Beijing or central-eastern China) comes in from $k = 3$–4, and South-East Australia (Melbourne) from $k = 4$.
   - Within a region, the exact city is often a near-tie and should be decided on practical grounds: venue, airport, local hosts.
4. **Including Latin America costs little.** A 5th hub there (Santa Cruz, Bolivia, which reaches Brazil, Argentina and Chile within 3,500 km) raises regional coverage from 70% to 83%, and the footprint still falls (2.16 → 1.96 kt). Its expected attendance, about 50, sits exactly at the lower floor, so it is a deliberate inclusion hub, not a self-sustaining one. Santa Cruz is the geometric optimum. In practice São Paulo or Buenos Aires, which have the region's largest communities, would serve a similar role with more local attendees; this can be tested directly with `evaluate_set()`.
5. **Africa is never covered beyond 12%.**
   - About 91 articles in these journals come from African institutions. A hub there (e.g. Johannesburg) would draw about 9 expected attendees from the *publishing* community, far below any viability floor.
   - Including Africa therefore cannot come out of this optimisation. It requires an explicit decision: a hub whose purpose is to grow participation rather than serve existing demand, or a community measure that goes beyond publications.
6. **The steeper decay slope (sensitivity) gives the same regions** (Brussels, Beijing, Melbourne, New York/Houston, Santa Cruz), with higher predicted attendance. The conclusions do not depend on which of the two slopes is right.

### Caveat on the knee
With few points on a front (3 for $k = 1$), the knee is a weak notion. For $k = 1$ the knee is Belgrade (better reach to Western Asia and North Africa), while the λ-solution used in the bootstrap is Brussels. The bootstrap therefore measures the stability of "the λ-solution nearest the knee". Present the knee as an example compromise; the whole front is the actual result.

## Limitations, in one place

1. **The community is measured by publications.** That under-counts regions with less funding or indexing. Coverage refers to the *publishing* community.
2. **The decay curve rests on three meetings,** and the two estimates (−0.57, −0.85) disagree. All results are shown under both.
3. **Hub meetings are assumed to attract like a full OHBM.** A smaller regional hub may draw less (or, being cheaper, more).
4. **The host-country surge is under-predicted** (Australia 2025). The model is conservative about induced demand near hubs.
5. **Flight emissions** use distance-band factors (`calculate_footprint()`), not actual routes or connections.
6. **The search is heuristic.** It is verified exactly for $k = 2$ and by restarts for larger $k$, but it is not proven optimal for $k \ge 3$.
7. **City size is a proxy** for venue and airport capacity, not a check of actual venues.

## Related work

- Klöwer, M., Hopkins, D., Allen, M., & Higham, J. (2020). An analysis of ways to decarbonize conference travel after COVID-19. *Nature*, 583, 356–359. Optimal host locations and multi-hub formats for AGU/EGU; the closest precedent.
- Burtscher, L., et al. (2020). The carbon footprint of large astronomy meetings. *Nature Astronomy*, 4, 823–825.
