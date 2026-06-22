# Hypothesis Testing: Equivalence Testing

## 

The dominant paradigm or way of approaching statistics is based on
**Null-hypothesis significance testing (NHST)**. All the ANOVAs,
t-tests, and regressions you are probably familiar with operate this
way. As a recap, under this approach, you assume the the effect of a
covariate or the difference between treatments, groups, etc., is 0. That
is the null hypothesis. The alternative is the difference or effect is
not zero. In a grossly simplified example, you collect data and, with a
specified false-positive error rate (i.e. a p-value (P);
\\\alpha\\=0.05) determine you either have evidence to reject the null
hypothesis, or you fail to reject the null. **FAILURE TO REJECT THE NULL
IS NOT THE SAME AS CONCLUDING THE NULL IS TRUE**.

\\ H_0: \theta_1-\theta_2=0\\ H_A: \theta_1-\theta_2 \neq 0\\

*Equations representing the typical null and alternative hypotheses that
the difference in two parameters is 0 (null) or not equal to 0.*

Whether or not using such a cutoff is advisable (it’s not, even Fisher
didn’t agree with it), this framework might not be optimal or ideal for
several reasons when it comes to ecological research or monitoring for
more fundamental reasons. Doug Johnson provides a more in-depth
discussion of statistical testing more generally (Johnson 1999)[^1] but
raises two important points we will try to summarize here.

- NHST is a straw man
- Sample size determines significance

The premise of NHST doesn’t jive with the inherent randomness or
stochasticty most biologists and ecologists assume is present in natural
systems. For example, we probably don’t expect the population size of
deer, nuthatches, or paddlefish to remain the same from one year to the
next. So if we were curious if the population had changed across years,
why would we perform a statistical test that posits the change is
exactly 0 (Dixon)? The “statistical hypothesis” that absolutely no
difference exists among the parameters or quantities we have measured is
nonsensical when we are conducting observational studies. It may have
it’s place in truly experimental designs, where individuals are randomly
assigned to a treatment, but Doug points out, such hypotheses
(difference = 0) are generally uninteresting or already known to be
false in most observational ecological studies.

This concept of testing a null hypothesis is especially problematic when
considering that in the above framework, P values are a product of
sample size. This means that, under small samples, it becomes nearly
impossible to detect anything but a very large difference, meaning you
will likely fail to reject \\H_0\\. Conversely, if you have a large
sample size, you can detect very small differences and will frequently
reject \\H_0\\. This may lead to odd or inconvenient results for
biologists who quickly learn to tap dance, describing their results as
“statistically significant” but not “biologically significant.”

| N   | D     | N   | D    | N   | D    |
|-----|-------|-----|------|-----|------|
| 2   | 30.42 | 15  | 5.50 | 100 | 1.98 |
| 4   | 13.88 | 20  | 4.66 | 200 | 1.39 |
| 6   | 9.99  | 40  | 3.20 | 300 | 1.14 |
| 8   | 8.15  | 60  | 2.58 | 400 | 0.98 |
| 10  | 7.05  | 80  | 2.22 | 500 | 0.88 |

The minimum difference (D) needed to reach tcriticial for a one sample,
two-tail T test for a given sample size (N); α = 0.05; σ = 10 {.table
.cl-30da55b4 quarto-disable-processing="true"}

## Equivalence/ Superiority/ Non-Inferiority Testing: A more better way to do biology/ecology

If we accept that there is always some small difference between
parameters or quantities we measure, then all NHST is really testing is
whether our sample size is large enough to detect that difference. So
why are we doing it?

What we generally are interested in is whether the difference we observe
is greater than some trivial amount. What defines trivial is a
biological question. Many will say “but I don’t know what is a trivial
difference.” That probably isn’t true. You probably know enough about
your system to try. You can probably come up with some justifiable
threshold that is different from “no difference.”[^2]. This might come
from adaptive harvest management plans (USFWS 2025)[^3], the costs of
some management intervention, the status of a species, or it’s
threatened or endangered status. The point is you can come up with some
justifiable threshold. It doesn’t need to be perfect; remember, the
alternative is the silly notion that the quantities are exactly the
same.

When approaching the problem from this perspective, there are three
alternative ways to frame the traditional statistical hypothesis:

- is option A better than the status quo? (superiority testing).
- is option A not any worse than the status quo? (non-inferiority
  testing)
- is option A effectively the same as the status quo? (equivalance
  testing)

These are different questions than NHST but arguably closer to what we
actually care about. We can go through some examples to further
highlight how and why.

### Example: Pheasant nest success and habitat management

Pheasants are a popular game species and considerable resources are
dedicated to identifying management practices that can benefit the
species. Because of the nature of gamebird population dynamics, managers
frequently try to identify an enact management practices that will
increase nest success. On average, nest success is ~35% in “typical”
grasslands. But perhaps we wish to consider if taking some kind of
additional management action improves nest success (food plots, predator
control, burning, forb interseeding, etc.) For simplicity sake, we’ll
pretend we know exactly when hens start nesting and nests are checked
daily.

Next, we will demonstrate what is really being tested under the 4
possible statistical hypotheses and try to help clarify which approaches
are suitable when. In each example, nest survival in a typical field
will be \\\theta\_{typical}\\ and nest survival in a managed field is
\\\theta\_{managed}\\. Let’s assume too, that nest survival in a managed
field is ~50%. We will assume we observe 60 nests in each type of field.

- Data Simulation
- NHST
- Non-inferiority
- Equivalence
- Superiority

We can simulate a data set for analysis that we will use throughout.

``` r

library(tidyverse)
# write a function

nest.sim<-function(n1=60,n2=60,p1=0.35, p2=0.5){
  
  typical<-rbinom(n1,1,p1)
  
  managed<-rbinom(n2,1,p2)
  
  data.frame(surv=c(typical,managed),
             trt=c(rep('ctrl',n1),
                   rep('managed',n2)))->tmp
  
  return(tmp)
  
}

 
set.seed(123)

sim.dat<-nest.sim()
```

First up is NHST. NHST is probably the most common framework people will
go to to in this example. It’s straightforward.

NHST is appropriate if your question is: “Is nest survival different
between typical grasslands and managed grasslands?” and is represented
by the statistical hypotheses: \\ H_0:
\theta\_{typical}-\theta\_{managed}=0\\ H_A:
\theta\_{typical}-\theta\_{managed} \neq 0\\ Because this is logistic
regression, the above is more accurately written as: \\ H_0:
\frac{Odds\_{managed}}{Odds\_{typical}}=1\\ H_A:
\frac{Odds\_{managed}}{Odds\_{typical}} \neq 1\\ The fraction in each
hypothesis is an odds ratio (\\OR\\). We don’t have time to get into the
weeds about what odds are, but just know that, because we are working
with logistic regression, one of the quantities we will be interested in
will be the odds ratio and that this representation is the same as the
previous one, just adapted for logistic regression.

We test it by running a simple glm

``` r

glm(surv~trt,family=binomial,data=sim.dat)->mod.nhst

broom::tidy(mod.nhst)->tab.nhst
tab.nhst
#> # A tibble: 2 × 5
#>   term        estimate std.error statistic p.value
#>   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
#> 1 (Intercept)   -0.547     0.268     -2.04  0.0413
#> 2 trtmanaged     0.480     0.372      1.29  0.197


exp(confint(mod.nhst))
#>                 2.5 %    97.5 %
#> (Intercept) 0.3372030 0.9697479
#> trtmanaged  0.7818544 3.3776999
```

The estimate listed for `trtmanaged` is the log odds ratio of the effect
of management. It effectively represents the difference between survival
in typical vs managed fields, on a transformed scale. Therefore, if this
quantity is different from 0, then \\H_A\\ is true. To get the \\OR\\,
all we have to do is exponentiate the estimate (\\e^{estimate}\\). Now
we want to see if the confidence interval around the \\OR\\ includes 1.

``` r

exp(confint(mod.nhst))
#>                 2.5 %    97.5 %
#> (Intercept) 0.3372030 0.9697479
#> trtmanaged  0.7818544 3.3776999
```

We can see the 95% confidence interval of the \\OR\\ includes 1. So
under NHST, we would fail to reject the null hypothesis. You may have
noticed the the `p.value` for `trtmanaged` was \>0.05. That’s a quick
way to arrive at the same conclusion. We go through the steps of talking
about the \\OR\\ though, because it will become more relevant later on.
But under NHST, looking at that `p.value`, in this example, works too.

We simulated data with a 15% difference in survival, but failed to
detect a difference. This is a type II error. But it’s just one possible
data set. We should simulate 1000 data sets to check to see how often we
might actually reject the null hypothesis, and detect the very real
difference in survival.

``` r


#a function to simulate data and test if the odds ratio overlaps 1
res.sim<-function(threshold=1,alpha=0.1){
  nest.sim()->tmp.dat
  
  glm(surv~trt,family=binomial,data=tmp.dat)->mod.tmp

  exp(suppressMessages(confint(mod.tmp,level=(1-alpha))))->tmp.ci
  
  as.numeric(between(threshold,tmp.ci[2,1],tmp.ci[2,2]))->out
  
  return(out)
  
}

map_dbl(1:1000,~res.sim(alpha=0.05))%>%
  sum()/1000
#> [1] 0.617
```

It should be that ~60% of the time, the interval overlaps 1 meaning we
would fail to reject the null hypothesis. What some folks will try to
say then, is that there is no difference or no effect of management.
That is false. You did not test that. Failing to reject the null
hypothesis does not mean the null hypothesis is supported or true. You
cannot prove a null hypothesis, statistical or otherwise. It’s a logical
fallacy to claim so.

So where do we go from here? We know that we didn’t have a large enough
sample size to detect a difference (because we simulated a pretty large
one, 15%, and things are NEVER the same) are never the same. But maybe
there is a way to reframe the question. Management rarely happens in a
vacuum other factors come into play and by considering those, we can use
a different testing framework.

Non-inferiority testing doesn’t test whether a difference exists, it
tests whether the alternative is worse than a standard or another
treatment. It’s common in drug trials and industry. It’s most
appropriate when you need to show that what you are proposing isn’t any
worse.

In our pheasant example, maybe we are replacing food plots with
high-diversity forb plantings. We don’t necessarily think that nest
success will improve, but many hunters believe that food plots are
critical to helping hens nest and produce large clutches in the spring.
In this case testing whether a difference exists isn’t what were
after.We just need to demonstrate that survival under forbs isn’t any
worse than “typical”.

Now, the hypotheses change:

\\ H_0:\theta\_{typical}-\theta\_{manged} \geq M\\ H_A:
\theta\_{typical}-\theta\_{manged} \< M \\

and we need to define \\M\\, or the margin we care about. This is where
people get squeamish. In practice, \\M\\ typically isn’t 0 (or 1 for an
\\OR\\). There is typically uncertainty in \\\theta\_{typical}\\ and we
never actually expect a difference to be 0. M is usually some difference
that we think is negligible. So how much worse, does the alternative
really need to be before we care? Defining that makes people squeamish,
but it’s likely folks can identify some threshold. In cases where you
really want to sit in the fence and not use your ecological/biological
knowledge to define the threshold, \\M\\ could be set relative to the
upper confidence limit of \\\theta\_{typical}\\. This suggests the
difference between the two parameters needs to exceed the uncertainty in
the control or reference group.

In the pheasant example, maybe we are ok as long as
\\\theta\_{managed}\\ isn’t more than 10% less than
\\\theta\_{typical}\\. This corresponds to an threshold \\OR\sim0.8\\. A
fun fact about non-inferiority tests, they are one-tailed while
tradition NHST is two-tailed. If we were to keep our type I error rate
the same as NHST, that means we now can use a 90% confidence interval
and test whether the lower bound of our CI includes 0.8.

``` r

exp(confint(mod.nhst, level=0.90))
#>                   5 %      95 %
#> (Intercept) 0.3687230 0.8933842
#> trtmanaged  0.8784433 2.9961480
```

Huzzah! The lower bound doesn’t include 0.8 so we can conclude that the
alternative is non inferior. But we should also simulate additional data
sets and compare it to NHST. Remember, under NHST we failed to reject
the null ~ 60% of the time.

``` r

map_dbl(1:1000,~res.sim(threshold=0.8,alpha=0.9))%>%
  sum()/1000
#> [1] 0.001
```

So now we only fail to reject the null 40% of the time. In practical
terms, under NHST, we select the true hypothesis (a difference exists),
only ~ 40% of the time, but under non-inferiority, we select the true
hypothesis ~ 60% of the time. We have more power (although neither is
still great).

Is this cheating? Maybe, depending on who you talk to. We’ve done two
things different from NHST. First, we lowered the threshold from 0 to
-10% (or in \\OR\\ terms, from 1 to 0.8). Second, we are using the same
\\\alpha=0.05\\ but only performing a one-tail test. Some people see
this as lowering the bar and would recommend using \\\alpha=0.025\\.
Instead, think of it as a reward for having a more appropriately defined
statistical hypothesis for the problem at hand!

But wait, there is more!

Another alternative to NHST is equivalence testing. This is really
appropriate when you want to prove that an alternative or effect isn’t
very different or near 0. In the wildlife world, it might be tempting to
reframe your management hypothesis as “I will take management action as
long as I know these two things aren’t equivalent”, but generally, you
use this when you **want** to demonstrate equivalence. You still can’t
conclude difference (can’t prove a null hypothesis). Our statistical
hypotheses are now written as:

\\ \begin{equation} H_0: \begin{cases}
\theta\_{typical}-\theta\_{manged} \leq -M \\ \quad \quad\quad and\\
\theta\_{typical}-\theta\_{manged} \geq\\ \\ M \end{cases}
\end{equation}\\ H_A: -M \leq\\ \theta\_{typical}-\theta_2 \leq\\ M\\

Like inferiority testing, you will need to define a margin of
equivalence, but now it will be two-sided. This may be trickier because
you may have noticed, that if you set the margin wide enough, anything
could be declared equivalent. This is where being a biologist comes in
and you need to be able to justify that interval. Likewise, if the
uncertainty surrounding a parameter is large, than it will be nearly
impossible to determine equivalence (or inferiority for that matter).

In the above pheasant example, we will need to change our scenario a
bit. Maybe now,“typical” represents some gold-standard for habitat
management, and the “managed” is a new program. Rather than testing for
a difference, we want to establish that the cheaper “managed” is
as-good-as the “typical.” It’s not a great example, but lets pretend
equivalence (and not non-inferiority) is what we’re after. Maybe we want
to make sure there aren’t too many pheasants with successful nests?

Lets consider a 10% difference in survival as our equivalence region.
It’s common to make this a symmetrical difference but need not be. So in
our case, if nest survival of managed fields is within a range of
0.25-0.45 (\\0.1\\ \pm\\ \theta\_{typical}\\), we would consider them
the same. This corresponds to an \\OR\\ ~ 0.62 - 1.52. If we calculate
the confidence interval for our \\OR\\ and it fits entirely within that
interval, we would declare that managed is the same as typical. However,
if we want to test at \\\alpha=0.05\\ we actually do so by computing the
90% confidence interval. Don’t worry about the derivation right now,
just know that equivalence testing has a type I error rate of \\\alpha\\
by constructing a \\1-2\alpha\\ interval.

``` r

exp(confint(mod.nhst,level=0.9))
#>                   5 %      95 %
#> (Intercept) 0.3687230 0.8933842
#> trtmanaged  0.8784433 2.9961480
```

In this case, the upper limit of the 90% CI exceeds 1.52, so we cannot
conclude that the two treatments are equivalent. Based on how we
simulated data, we know this is true, so did we just unlock some new
cheat code?

No, because just like NHST, failure to reject the null doesn’t mean the
null is true. The uncertainty in our survival estimates may be too large
to be able to detect just a 10% difference. There might be some specific
cases where there is an exception[^4], but it’s very unlikely.

However, this does demonstrate that, if you have *a priori* reasons for
defining the equivalence range (and not basing it on the SE of
\\\theta\_{typical}\\ that you might improve your power increasing your
sample in the alternative treatment category. That can be a useful
option in study design.

Superiority testing is probably the most correct form of statistical
hypothesis testing for a lot of wildlife management. It’s most
appropriate when you want to know if something is better than an
alternative. Superiority testing is superior to NHST for some
philosophical and statistical reasons. First, it requires you to define
“how much” better something has to be. This avoids the tap dance of
biological vs. statistical difference if you are blessed with a large
sample size. Second, this is technically a one-tail test. So testing at
\\\alpha=0.05\\ in a one tail test has a little more power for the same
sample size. Because of this, some folks will recommend using an
\\\alpha=0.025\\, to make it more like two-tailed test, but you don’t
need to.

In superiority testing, our statistical hypotheses are: \\ H_0:
\theta\_{manged}-\theta\_{typical} \leq M \\ H_A:
\theta\_{manged}-\theta\_{typical} \> M\\ Where, like other tests, \\M\\
is some minimum difference we thing is trivial. In the pheasant example,
maybe we wish to demonstrate that forb interseeding
(\\\theta\_{managed}\\) improves nest survival compared to a typical
brome field (\\\theta\_{typical}\\). Given the cost of forb seed, maybe
we need to make sure it’s at least 5% higher before we are willing to
adopt it. We can again use a confidence interval to test this. Now, our
\\OR\\ needs to not include ~1.23. But because we are using a one-tail
test, we can use the 90% confidence interval and only look at whether
the lower bound is less than 1.23

``` r

exp(confint(mod.nhst,level=0.90))
#>                   5 %      95 %
#> (Intercept) 0.3687230 0.8933842
#> trtmanaged  0.8784433 2.9961480
```

Unfortunately, we can’t declare \\\theta\_{managed}\\ is superior to
\\\theta\_{typical}\\, despite that being how we simulated the data. But
how does power compare to NHST?

``` r

map_dbl(1:1000,~res.sim(threshold=1.23,alpha=0.9))%>%
  sum()/1000
#> [1] 0.046
```

Despite performing a one-tail test, it’s actually worse! what gives?
Well, we specified a difference larger than 0 now, so there may have
been instances where simulated data estimated an \\OR\\ \>0 but less
than 1.23. If we wanted the one-tailed equivalent from NHST, we could
just examine whether the lower bound of 1 fell within a 90% CI.

``` r

map_dbl(1:1000,~res.sim(threshold=1,alpha=0.9))%>%
  sum()/1000
#> [1] 0.019
```

Now it seems that only 2% of the time, do we find that an \\OR\\ of 1

## Summary

For better or worse, statistical hypothesis testing is part of the
statistical milleau. Typical NHST sets a pretty high bar for rejecting a
null hypothesis, but more importantily, is incongruent with our
understanding of biology/ecology and reduces our need to think
critically about our problem. Equivalence testing offers an alternative
framework that requires us to be specific about how much of a difference
we consider to be biologically relevant. It also is the only correct way
to provide evidence that two quantites are “functionally equivalent.”…

[^1]: Johnson, D. H. The insignificance of statistical significance
    testing. *Journal of Wildlife Management*. 63:763-772.

[^2]: Fisher picked the cutoff \\\alpha = 0.05\\ because it corresponds
    to two standard deviations. It was convenient, had some kind of
    justification, but was never meant to exclude other levels or become
    so dogmatic

[^3]: U.S. Fish and Wildlife Service \[USFWS\]. 2025. Adaptive harvest
    management: 2026 hunting season. U.S. Department of Interior,
    Washington, D.C. 85pp.

[^4]: if you could choose your sample size and equivalence level *a
    priori* before collecting data, and demonstrate that you do have
    sufficient power to determine equivalence within the range you
    specified, then you could argure that it’s more likely things aren’t
    equivalent. You would still never be able to state the null
    hypothesis is correct, but it might be compelling enough for making
    decisions
