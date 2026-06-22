# Detection Probability

## 

### Concept

Counting or trying to observe the presence/absence of unmarked animals
during surveys can often seem like it’s a unique study design but it’s
maybe more helpful and correct to think about it as an extension of
capture-recapture models. Historically, counts or the presence/absence
of animals has been treated as if animals were perfectly observed. This
is incorrect because you do not detect all individuals, just like you do
not capture every individual in a capture-recapture study. When making
the jump from physically capturing an animal to observing or counting
them, (re)capture probability is no longer a relevant term or parameter.
Instead, the new parameter is called **detection probability** and it
attempts to account for imperfect detection. It does function in a
manner similar to capture probability but it starts to encompass more
“things” beyond what capture probability does. Detection probability is
really a composite of several processes. They are classified into one or
two (or three) groups that are recognized as combining to affect
detection probability: availability and sightability. You may then see
notation in some articles that present detection probability, p\_., as a
product of the two processes: p\_.=p\_{\\ availability} \times p\_{\\
sightability}

#### Availability

##### Temporary Emigration

Availability is not unique to detection probability or unmarked
sampling. For capture-recapture of marked or unmarked individuals, it
can refer to two separate processes. The first way is being physically
present. Just like capture probability only applies to or describes the
probability of capture for animals that are available to be captured,
detection probability only applies to individuals that are available to
be detected. An exaggerated example in capture-recapture could be if you
are trying to capture bears using culvert traps, during the winter. Most
bears are hibernating and would be unavailable to be captured. When
using something like a trap grid, this typically means that only animals
with home ranges inside your trapping grid are considered available.

An animal that is not present, because they’re not inside the study
area, hibernating in a den, etc., has a detection/capture probability of
0. More commonly, whether or not an animal is available varies. If an
animal is moving in and out of the study area during surveys or
captures, their capture/detection probability changes between p and 0.
They are sometimes available and sometimes not. This process is often
called **temporary emigration**. This is most often associated with
physical presence in a study area, but need not always be. The best way
to differentiate this form of availability from the next is recognizing
that availability has two discrete states: \left\\ \begin{aligned}
unavailable: p=0 \\ available: p\>0 \end{aligned}\right\\ This form of
availability applies both to traditional or physical capture-recapture
methods as well as surveys of unmarked individuals.

##### Behavior

The second form of availability (and possible third group) is related to
animal behavior, not their physical presence; is the animal behaving in
a way that makes them observable? Unlike temporary emigration, these
differences don’t always result in a bifurcation of detection probabilty
(i.e., 0 or p \> 0). For example, calling by frogs is sensitive to
precipitation and temperature. Trying to perform counts of frogs during
drought or cold weather will likely have fewer individuals available to
be observed than during warmer and wetter weather. Availability in this
second form is more of a continuous scale, and not a discrete yes/no
like with temporary emigration, but an animal must still be behaving in
a way that makes them available in order for an observer to detect them
(Farnsworth 2002)[^1].

#### Sightability/ perceptability

The last component of detection probability is sightability or
perceptability (not all detections are visual). Sightability is often
affected by attributes and characteristics of the observer or factors
that affect the ability of an observer to detect an animal, given the
animal is present and behaving in a way that makes the animal observable
(singing, strutting, not hibernating, etc.) Differences in hearing,
eyesight, or awareness among observers, distance between an observer and
an animal, or ambient road noise during a bird survey or shadows during
an aerial survey are all part of sightability process, but not
availability. Generally, this is unique to studies where animals are not
physically being captured, as the “observer” in traditional capture
recapture is usually a net or trap, not a person.

### Study Design

Given the above issues, how do you deal availability and sightability
when accounting for imperfect detection in your surveys? You do so by
carefully selecting and implementing a specific study design that allows
you to at least try to account for the various sources of imperfect
detection. In general, this means recording specific information at the
visit, or increasing the number observers, sampling visits to the same
site, or both. It’s rare and very difficult to model all the above
components separately. Most focus on only one or maybe two sources, and
may not uniquely model each process separately. Most often, behavioral
availability and sightability are estimated jointly as…..detection
probability.

##### Temporary immigration

If temporary immigration occurs in either traditional capture-recapture
or unmarked methods, the only certain method of dealing with it is via
the robust design (Pollock 1982)[^2]. Under this framework, you perform
a simple capture-recapture framework with \>1 capture opportunity where
closure is maintained (secondary occasions) multiple times (primary
occasions). In typical robust design methods,you might repeat 3
consecutive nights of surveys (secondary) on three consecutive weeks
(primary), assuming no births or deaths across weeks. Alternatively, you
can treat each survey visit to a site is a primary occasion and apply
double observer methods in place of secondary occasions, or break the
survey into smaller time intervals, treating repeated counts as
independent samples (Royle 2004)[^3] or removal samples (Chandler et
al. 2011)[^4].

The upshot is, you need to have a design that has primary and secondary
sampling in some manner to be able to deal with temporary immigration.

##### Behavior

[Removal
sampling](https://tlyons253.github.io/MDChelp/articles/CCAUnmarked.html)
is considered the primary way to account for availability in surveys. It
assumes that the only reason an individual was not detected during a
preceding time interval was they were not behaving in a way that made
them detectable. If you assume that is the only reason why an individual
wasn’t detected (i.e. ignore sightability), then removal sampling does
account for availability.

Behavior-related availability can also be addressed by making repeat
visits to a site while closure is maintained. In doing so, and
documenting the conditions that might influence availability, you can
model detection as a function of these covariates. In frog surveys, this
might mean recording the amount of rainfall in the previous 24 hours,
the ambient humidity, or temperature.

Alternatively, you might restrict your surveys to meet conditions where
availability will be high and consistent. For example, state biologists
will try to conduct roadside counts of pheasants only on mornings with
heavy dew, because pheasants are believed to try to “dry out” along
roads, making them more available for detection. You still should use
some method to account for imperfect detection, but this is one way to
reduce variation due to animal behavior.

##### Sightability

Sightability itself is comprised of individual observer differences as
well as environmental factors. An animal must be physically present, and
behaving in a way that makes them available to be sighted by an
observer. Depending on the study design, some components of sightability
can be modeled explicitly (i.e. distance sampling) while others may be
included as covariates on a model for detection probability, similar to
behavioral availability. From a design perspective, to deal with
sightability, you need multiple observers and/ or visits, recorded
covariates that might affect an observers ability to detect an animal
(road noise, visual obstructions, etc.), and/or collecting distance
data.

Double-observer methods, by virtue of the sampling design, implicitly
account for differences in sightability between two observers.
Sightability can be modeled explicitly as well. Distance sampling
explicitly accounts for a decrease in sightability as the distance
between the observer and animal increases. Some hierarchical approaches
explicitly model behavioral availability and sightability separately.
Some studies may restrict the distance at which they will count animals
(e.g., 50m radius point count instead of unlimited distance) to minimize
or eliminate the need to explicitly model the effect of distance.

##### Putting it all together

While these processes are, conceptually at least, unique, when trying to
account for them they are not always treated as such. There are
certainly approaches where only one component of detection is addressed.
When dealing with behavior-based availability and sightability, it’s
most common to model detection probability as a composite process,
rather than separate processes. In these cases, a linear model is
constructed for detection that includes covariates related to
sightability along with availability. In the frog example, you might
include covariates for rain as well as the observer and road noise: p. =
\mu+\beta_1X\_{rain}+\beta_2X\_{road\\ noise}+e\_{observer} In this
approach, you do not model availability and sightability separately.
It’s not “correct” despite frequently being done this way, but it’s
still a step in the right direction when dealing with imperfect
detection and well accepted. More complex methods that tackle temporary
immigration, or explicitly model sightability and availability
separately do exist. For example, using a robust design with removal,
double observer, or repeated counts, one can account for temporary
immigration and availability (Chandler et al. 2011)[^5]. Using distance
sampling methods and removal methods, you can separately (and “more
correctly”) account for availability and sightability (Amundson et
al. 2014)[^6]. Given the advancement of methods for fitting hierarchical
models in the past 20 years or so, the sky is the conceptual limit to
how many processes you can stack when trying to account for all the
different components of detection probability. There are real limits
though, as you will generally require more and more data to fit these
complex models. And while conceptually, they are plausible, in practice,
you might not be able to actually treat these processes separately.

### Last thoughts

Accounting for imperfect detection is an important part of any survey or
study. It’s also important not to obsess over it, but to make a good
faith effort to account for it. It is unlikely that you will be able to
adequately account for all aspects that contribute to imperfect
detection. Some components you may be able to address by adjusting when
or how you conduct surveys (optimal weather conditions, like with frogs;
50m vs unlimited distance point counts) or by implementing specific
methodologies (removal or double-observer sampling). The important thing
is to have a sound understanding the the ecology of your organism so you
can identify what sources of imperfect detection are the most important,
and address those.

[^1]: Farnsworth, G. et al. 2002. A removal model for estimating
    detection probabilities from point-count surveys. *Auk*.
    119:414-425.

[^2]: Pollock, K. H. 1982. A capture-recapture design robust to unequal
    probability of capture. *Journal of Wildlife Management*. 46:
    757-760.

[^3]: Royle, A. J. 2004. N-mixture models for estimating population size
    from spatially replicated counts. *Biometrics*. 60:108-115.

[^4]: Chandler et al. Inference about density and temporary emigration
    in unmarked populations. *Ecology*. 92:1429-1435.

[^5]: Chandler et al. Inference about density and temporary emigration
    in unmarked populations. *Ecology*. 92:1429-1435.

[^6]: Amundson et al. 2014. A hierarchical model combining distance
    sampling and time removal point counts. *Auk*. 131:476-494.
