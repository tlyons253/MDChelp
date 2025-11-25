# Capture-Recapture: Closed Capture Abundance - Marked Individuals

## Closed population abundance estimation methods- marked individuals

As mentioned in the overview, capture-recapture models for closed
populations are strictly used for estimating the abundance of critters.
Though many abundance estimation methods have, at their core, some kind
of closed population capture-recapture framework, these methods below
are generally applied to strictly estimate abundance \\\hat{N}\\. There
have been limited applications of these methods beyond obtaining a point
estimate for a location, at a single point in time, and none that
robustly address abundance-habitat relationships, though expanding the
model development to address these questions using Bayesian methods (or
spatially explicit capture-recapture) are possibilities.

Below are several common capture-recapture abundance estimation methods
suitable for **closed populations**, often shortened as “closed capture
methods”. Demographic closure means that, while animals are being
observed, there are no births, deaths, immigration, or emigration within
the study area. The methods below are appropriate for “marked”
individuals. That is, these assume you are physically capturing and
marking individuals. Another document covers abundance estimation in
closed populations for individuals you can uniquely identify, but are
not physically marking them.

Fundamentally, we’re trying to solve this equation: \\ N=\frac{n}{p} \\
Where \\N\\ is the unknown abundance,\\n\\ is the number of unique
individuals we know about, and \\p\\ is the capture probability.

Probably the most important thing to recognize about these models is,
aside from needing to meet the closure assumption, all methods are
**very, very, sensitive to violations of assumptions about capture
probability and closure**. If capture/recapture probability is not
addressed correctly, your estimates of abundance can be very, very
wrong.

Typically, the use of the methods below will require you have some form
of trap grid or transects (think small mammal traps) or can at least
define some spatial extent you are trapping (e.g., sampling defined
stream reaches, capturing waterfowl within a spatially defined breeding
population). Again, having demographic closure is of key importance
here; if animals are moving in and out of the study area or the size of
the study area keeps changing (due to sampling effort) then your
estimate of \\N\\ becomes somewhat useless.

- Lincoln-Petersen Method
- Closed-capture likelihood models and variations
- Summary

  
The original Lincoln-Petersen (LP) estimator (Petersen 1896[¹](#fn1),
Lincoln 1930[²](#fn2)) is a simple 2-sample technique to estimate
abundance. By 2-sample technique, I mean you only need 2 capture
occasions: the first capture occasion when you mark and release animals,
and an additional occasion when you recapture them. That’s it. The LP
estimator calculates abundance as: \\N=\frac{n_1\times \\ n_2}{k}\\

- \\n_1\\ the number of animals encountered on the first occasion. All
  encountered animals are marked and released.

- \\n_2\\ the **total** number of animals encountered on the second
  occasion.

- \\k\\ is the number of marked individuals from \\n_1\\, captured again
  and included in \\n_2\\.

This is a very simple equation, but it’s not the one actually used in
practice. If you do end up using a LP, you will most likely use the
Chapman version/ estimator. It’s what’s implemented in
[`MDChelp::chapman()`](https://tlyons253.github.io/MDChelp/reference/chapman.md).

Multiple variations and extensions of the LP method exist. If you have
multiple encounters where you mark, release, and recapture individuals,
an alternative known as the Schnabel method/estimator is appropriate
(after Zoe Emily Schnabel). There are also methods that combine the
Schnabel method with simultaneous removals, but are not discussed here
and aren’t generally used anymore. The extensions beyond the simple LP
framework have largely been supplanted by methods that use uniquely
marked animals, primarily because of the development of better marking
techniques (e.g. RFID tags). Still, it’s important to highlight the
conceptual contributions of these individuals. It’s also a helpful
reminder of some less common methods that are sill viable when you can’t
meet the data collection needs of more contemporary alternatives.

#### Important Assumptions/ Limitations/ Tidbits

- The population is closed between the first and second capture
  events!!!!

- Tags or markings do not need to be unique among individuals, but there
  are no tag losses between the following the release of marked
  individuals.

- All individuals have the same ***recapture*** probability.

  - The capture probability at the first event can be different than the
    probability at the second event, but the capture probability between
    unmarked and marked individuals is the same.

    - This means that the methods used to capture individuals during the
      first event for marking can be different than those used to
      capture individuals during the second event.

      - The most extreme example of this is when the LP method is used
        to estimate abundance when the initial capture period is
        (obviously) a live capture, marking and release, but the
        recapture is a dead recovery (e.g. waterfowl band recovery,
        tagged fish).

- LP methods perform poorly when fewer than 50 individuals are marked (
  ideally, mark \> 100) or \\p\\ is small. This is important if there is
  group-level heterogeneity in capture probability. If capture
  probability differs between male/ female or adult/juvenile, you “bin”
  individuals in each group separately which can lead to small \\n_1\\
  for any group

#### Why still use it ?

While “better” methods do exist that deal with some very common
violations of the assumptions that LP relies on (capture heterogeneity),
they typically require you find more populations to sample, have more
capture events, etc. **The fact that the alternatives might be difficult
to implement is not an excuse to just use LP if you cannot adequately
meet the assumptions!!!** It is worth remembering though, that it can be
advantageous to alter your study design to meet the assumptions of a
simpler approach than to address logistical or animal welfare
constraints of methods that promise more flexible or relaxed
assumptions.

There are times when the LP or other simpler approaches won’t cut it.
Usually it’s because the assumption of a homogeneous encounter
probability for marked and unmarked animals isn’t met. If you cannot
meet that assumption, then it’s time to switch to a likelihood approach
that can!

Doing so requires some changes. Mathematically, we now start using
likelihood based methods. This just means we start using probability
theory and distributions to estimate abundance rather than algebra like
above. The more practical, real-world implication is, we now need to
have individuals marked with unique markings and we need to follow them.
You are likely going to also have \> 2 encounter events, or
opportunities to capture and recapture marked animals. Information about
whether an individual(s) was captured at each encounter event is called
an encounter history. We now have several parameters that get used to
describe these encounter or capture histories.

- \\\hat{N}\\ the estimated abundance
- \\p\\ the capture and/or recapture probability.
- \\c\\ the recapture probability, if different from \\p\\

These are not the only parameters you may come across if you end up
getting further into closed-capture models, but they’re the foundation
on what all other variations are built and what can be used to help
discriminate among the different variations.

#### Variations on closed-capture models

A big focus of the historical development around closed-capture models
is to account for capture heterogeneity. This has led to a host of
variations in the literature. Below are some more common variations in
closed capture models that might help clarify things if you get more
into these methods, but also represent some important practical issues
you should consider with your own data. Unless noted, these variations
exist for both full-likelihood and Huggins methods.

- **Behavioral response to trapping:** Use \\p\\ and \\c\\ to account
  for trap happiness or trap shyness

- **Temporal variation in capture probability:** \\p\\ and/or \\c\\
  varies among encounter events

- **Heterogeneity via “mixtures”:** There are groups of individuals with
  different \\p\\ or \\c\\, but you can’t identify them in the field. We
  can use a “mixture model” that introduces another parameter \\\pi\\
  that represents the probability an individual belongs to one
  unobservable group or the other. Groups have different \\p/c\\
  parameters, but individuals within groups are assumed homogeneous.

- **Heterogeneity as individual random effects:** Is only feasible with
  the Huggins model. Rather than grouping individuals, or assuming
  individuals within groups are homogeneous, we can treat all critters
  as being different from one another via an individual random effect.
  Like random effects in linear models, there is some “average” \\p/c\\
  but individuals deviate from this average. Those deviations follow a
  normal distribution.

These variations are an important extension. As mentioned before,
estimates of abundance are sensitive to assumptions about \\p/c\\,
whether they are the same among individuals, vary among individuals,
vary across capture events, etc. Appropriately accounting for these
possibilities is critical to obtaining accurate and unbiased estimates
of abundance.

#### Criticisims/ Complications

There are a few common criticisms or complications that arise with the
use of likelihood-based closed-capture methods

- \\N\\ is only an estimate of the population size vulnerable to
  capture. If you sample more area (intentionally or unintentionally),
  your estimate of \\N\\ increases. Other methods focus on estimating
  density try to address this problem (e.g. spatially-explicit
  capture-recapture; SECR).

- Most applications do not try to address or estimate what causes
  variation in \\N\\-it’s possible, but likely logistically challenging
  (trapping effort in multiple areas) and still suffers from the above
  issue.

- Because of this, it’s typically suggested to use a SECR approach if
  possible. It results in an exactly defined spatial extent, making
  \\N\\ more reliable and can more easily incorporate/ test for
  environmental variables affecting abundance.

- Other methods have you sample smaller, but still defined areas to
  estimate density in a model-based approach. It’s much simpler to
  incorporate information about

- If you can exactly define the extent of the space being sampled, \\N\\
  represents a **realized** estimate of abundance, that is the estimate
  of the number of critters actually running around at that time. Other
  methods may provide model-based estimates of \\N\\ or both.
  Model-based estimates provide the expected abundance, given
  information about things like habitat or other environmental
  covariates. These are not the same quantity and which one you should
  use will depend on your question.

The above covers more of the technical details of closed-capture
methods. It’s important to understand them and they cannot be ignored.
But if you are in the processing of identifying appropriate methods for
your particular question, below are some general, practical points about
closed-capture methods that may be helpful.

- The closed capture methods described above are best suited when
  getting an estimate of \\N\\ is of interest, not understanding various
  factors responsible for variation in \\N\\.

- Because of many of the strict assumptions, it’s probably best not to
  think of \\N\\ as a “true” estimate of abundance, unless you can
  guarantee all individuals are vulnerable to capture. If you can at
  least ensure a representative sample, it provides a very good index.

- In practice, the best uses of these methods independently (e.g. not
  part of a robust design or other integrated method) will probably be
  for simple monitoring and status and trend assessment.

- Some specifics like distinction between \\p/c\\ are specific to
  physically capturing animals, but conceptually, but closed-capture
  methods are a very general concept that most other capture-recapture
  methods are based on.

------------------------------------------------------------------------

1.  Petersen, C. G. J. 1896. The yearly immigration of young plaice into
    the Limfjord from the German Sea. In “Report of the Danish
    Biological Station”, vol. 6, pp. 1–48.

2.  Lincoln, F. C. 1930. Calculating waterfowl abundance on the basis of
    banding returns. Circular 118, pp. 1–4. United States Department of
    Agriculture,
