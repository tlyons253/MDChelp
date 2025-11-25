# Non-parametric Bootstrapping

## Linear Model Basics

When you fit a model to data, you are making assumptions about those
data, typically (but not limited to) something about how those data are
distributed or how the terms of the model are related to the response
(e.g., linear). There can be some other assumptions some examples are:

- Simple linear model/regression (LM)
  - Typically used for continuous variables
  - Model terms and response are linearly related
  - The *residuals* are normally distributed with homogeneous variance.
- Log-linear (Poisson) model/regression
  - Typically used to model count data
  - Model terms and the **log** response are linear related
  - The mean of the response is equal to the variance (Poisson)

Those assumptions are sometimes not met which has important negative
consequences for your results. John Fieberg’s book [*Statistics for
Ecologists*](https://statistics4ecologists-v3.netlify.app/) goes into
more details, but it’s bad. The long and short of it is:

- If you violate the linearity assumption, your regression coefficients
  are biased. Think about if you left out a quadratic term of a linear
  model e.g.

\\Y=\beta_0+\beta_1\cdot x_1\\ \\ vs\\ \\ Y=\beta_0+\beta_1\cdot x_1 +
\beta_2\cdot (x_1)^2\\

Using just the linear term \\(\beta_1)\\ would not accurately describe
the relationship between \\Y\\ and \\x_1\\ so \\\beta_1\\ would be
biased.

- Another common area where this can be an issue is if you use a
  covariate that is an areal measurement or a count of something else
  that has a verrrry broad scale (values from 10’s to 1,000’s or more).
  The issue isn’t the nature of the variable per se, but the fact the
  scale is very, very broad. This also leads to the response and
  covariates not being linearly related.

- When other assumptions about the model/data aren’t met, such as the
  normal residuals in a LM or mean-equal-to-variance in a Poisson, the
  sampling distribution of \\\beta\\ is underestimated. This means the
  reported SE for any particular \\\beta\\ is too small and any
  resulting confidence interval or p-value will be incorrect.

What to do when you violate these assumptions?

If the linearity assumption is not supported, you change the type of
model (or the assumed underlying distribution) or perform some kind of
transformation and this ensures the linear relationship is met. In the
case of covariates that have very broad ranges, you may log transform
the covariate. Often this will clear up most issues, but other
assumptions about variances or residuals may not be met.

For example, in Poisson regression, the most common type of violation is
***greater*** variance than expected, or data being overdispersed. In
practice, overdispersion can arise because you omit some kind of
grouping variable (e.g. ignoring the potential for correlated survival
probability of gamebird chicks in the same brood), or because there are
more 0’s than expected. If you were to fit a model that ignored these
factors, you can no longer trust the SE’s , CI’s, or p-values of your
\\\beta's\\ output by typical routines (e.g. glm in R; [See this
section](https://statistics4ecologists-v3.netlify.app/10-maximumlikelihood#sec-profile)
of Dr. Fieberg’s book for a more in-depth discussion of issues with the
CI’s and p-values reported in in common software functions) and you
might find that a goodness-of-fit (GOF) test would indicate the model is
a poor fit. In a simple LM, you may find that no matter the
transformation you use, or what other terms you add, you still don’t
have normal residuals.

To fix that problem you could include a random effect of some grouping
variable (brood in the first case), or change the distribution, like
switching to a negative binomial distribution in the second case. But
even if you specify the model correctly, and try out different
distributions, you may find that your model does not pass a GOF test.
What do do? Bootstrap your data.

## How to bootstrap

Bootstrapping comes in 3 flavors (parametric, semi-parametric,
non-parametric) but only non-parametric bootstrapping is discussed here
and consequently everything described here may not hold under other
bootstrap alternatives. In the case of the non-parametric bootstrap, you
relax the distributional assumptions of your model (mean/ variance
relationship, residuals) and instead make different assumptions, namely
that your resampling scheme matches the original sampling scheme. You
still have to have deal with issues like linearity, independence, etc.
either through the model itself and/or via the resampling method. If
those assumptions can still be met, then the bootstrap allows you to
estimate the *distribution* of parameters, be they the regression
coefficients (\\\beta's\\) or other model-based estimates or
predictions.

The short version is, you can use the bootstrap to estimate the SE of a
parameter. How does this work? The brief version is:

1.  Resample your data *WITH* replacement
2.  Estimate model parameters, save those parameters
3.  Repeat a few thousand times

You should then have N estimates of each parameter. These N estimates
comprise an approximation of the *distribution* of the parameter. You
don’t use it to estimate a parameter, but the SE/ CI of that parameter
instead. DO NOT USE THE DISTRIBUTION TO ESTIMATE THE PARAMETER ITSELF,
ONLY THE SE/CI. The best estimate of the parameter mean comes from the
model using the original data.

### Determining the resampling scheme

The validity of the bootstrap is dependent on the resampling scheme
matching the manner in which data were collected, preserving the random
structures in the sampling process. This is easy if all the observations
were randomly sampled. You can then just resample the data, as is, at
random. Like this code below that creates 10 bootstrap data sets from
the original data,`tmp`.

``` r
 data.frame(X1=seq(1,10,1),
           X2=rnorm(10,0,1))%>%
  mutate(Y=0.2*X1+X2)%>%
  select(-X2)->tmp

#Create 10 bootstrapped data sets

purrr::map(1:10,~sample(1:nrow(tmp),size=nrow(tmp),replace=TRUE)%>%
              tmp[.,])->boot.dat

boot.dat
```

if you print `boot.dat`, you should see 10 different data frames. You
can see the row labels that identify which observations comprise the new
data. If an observation in the original data was sampled more than once,
it gets a decimal value. For example, a row labeled 5.3 would be the 3rd
appearance in the bootstrapped data of an observation that was the 5th
row in the original data. In this example, you only have 10 bootstrapped
data sets, but in reality, you probably want a minimum of 2,000. In
fact, if your resampling scheme is more complex, you will likely need
more than 2,000; possibly as many as 10,000.

The resampling gets more complicated when you start having data that
naturally falls in clusters. This could be something you account for in
the model, as a fixed or random effect. The important part is, it’s not
something that arises randomly in the observation (i.e. randomly
sampling people and observing eye color), It’s a part of the study
design (randomly sampling 5 people with blue eyes, 5 with brown, etc.)
When a cluster/variable is a fixed effect and is part of the study
design, you do not resample at the level of a fixed effect, but it does
need to be accounted for in the resampling scheme. If that same variable
is treated as a random effect, it does get resampled.

For example, think of a study where test scores are collected from
students, in 5 classrooms per school, in 4 schools:

1.  Schools aren’t randomly selected, classrooms within a school are
    randomly selected, but all 20 students within a classroom are
    measured,

- Original data consists of 400 rows, with 25 levels of classrooms and 5
  levels of school.
  - Resample 5 classrooms within each school, with replacement
  - Do not resample students within classrooms.
  - Do not resample schools.

``` r
library(tidyverse)
demo.dat<-expand.grid(school=c("Hogwarts",
                               "Tattoine P.S. 1",
                               "Hobbition Elementary", 
                               "Mordor Primary"),
                      room=seq(1:5),
                      student=seq(1:20)
                      )%>%
  mutate(score=sample(50:100,nrow(.),replace=TRUE),
         room=paste0(school,'_',room),
         student_id=row_number())

# create 1 bootstrap data sets by creating indices of the data selected. Run the code block, line by line (cumulatively), to see what is being done at each step.
demo.dat%>%
  distinct(school, room)%>%
  split(.$school)%>%
  map(.,~slice_sample(.x,n=nrow(.x),replace=TRUE))%>%
  bind_rows()%>%
  purrr::pmap(.,~filter(demo.dat,
                       school=={..1},
                       room=={..2}))%>%
  bind_rows()

#create 10 data sets


map(c(1:10),~demo.dat%>%
  distinct(school, room)%>%
  split(.$school)%>%
  map(.,~slice_sample(.x,n=nrow(.x),replace=TRUE))%>%
  bind_rows()%>%
  purrr::pmap(.,~filter(demo.dat,
                       school=={..1},
                       room=={..2}))%>%
  bind_rows())->boot.10

map(boot.10,~.x%>%
      group_by(school)%>%
      tally())  # number of students from each school

map(boot.10,~.x%>%
      group_by(school,room)%>%
      tally())  # number of students from each school
```

In this scenario, only the classrooms were randomly selected so that is
all that was randomly sampled. Every school is still represented with
100 students from each school, but the number of different classrooms
differs, leading to some classrooms accounting for more than 20
students.

``` r
gdata::keep(demo.dat,sure=TRUE)
```

What if 10 students were randomly sampled in each classroom instead of
all 20? Now you just make sure that those students are also resampled
within each classroom.

``` r
# make an "original" data set of 10 students from each classroom. We are pretending the other 10 students were never measured. You won't do this step ever with your data
demo.dat<-expand.grid(school=c("Hogwarts",
                               "Tattoine P.S. 1",
                               "Hobbition Elementary", 
                               "Mordor Primary"),
                      room=seq(1:5),
                      student=seq(1:10) #This is now 10 students
                      )%>%
  mutate(score=sample(50:100,nrow(.),replace=TRUE),
         room=paste0(school,'_',room),
         student_id=row_number())


# create one bootstrap data set ---------------------------------------------


# create indexes of randomly sampled classrooms (within schools) and students
  demo.dat%>%
  distinct(school, room)%>%
  split(.$school)%>%
  map(.,~slice_sample(.x,n=nrow(.x),replace=TRUE))%>%
    bind_rows()->school.room.idx # list of resampled classrooms

  #use the list of resampled classrooms to resample students within
  
  pmap(school.room.idx,~filter(demo.dat,
                               school=={..1},
                               room=={..2}))%>%
    map(.,~slice_sample(.x,n=nrow(.x),replace=TRUE))
         
# List is 20 elements long, with 5 elements per school. Room ID's may be
# repeated within a school and student_id can be repeated within a room. 
  
# make 10 data sets where classrooms in schools and students in classrooms are randomly resampled  
  
map(c(1:10),~ demo.dat %>%
    distinct(school, room) %>%
    split(.$school) %>%
    map(.,  ~ slice_sample(.x, n = nrow(.x), replace = TRUE)) %>%
    bind_rows() %>%
    pmap(.,  ~ filter(demo.dat,
                      school == {..1},
                      room == {..2})) %>%
    map(.,  ~ slice_sample(.x, n = nrow(.x), replace = TRUE)) %>%
    bind_rows())->boot.10
```

Go and inspect boot.10 again. Now, rather than each student appearing
once each time the classroom appears in bootstrap, each student may
appear \>1 time each time their classroom is selected. There are still 5
classrooms per school and 10 observations per school/classroom, it’s
just now some appear more than once. So previously, the MOST a student
could appear in a bootstrapped data set was 5 times (assuming their
classroom was randomly selected 5x). Now, in theory, that student could
appear as many as 50 times (their classroom was selected all 5 times,
and they were selected all 10 times, each of those 5x).

You can adjust the sampling scheme by allowing for random sampling at
different levels, or combining variables to construct the appropriate
“cluster” to perform resampling on. Just remember, if it was randomly
selected in study design, it needs to be randomly sampled in the
bootstrap.

### Deriving SE and CI’s

Once you have your bootstrap estimates, make a density plot or a
histogram and determine if the distribution for each parameter is normal
or close enough. Close enough counts here because you should have
thousands of estimates.

- If it looks normal, you can compute the SE of those bootstrap
  estimates (e.g. `sd(boostrap_estimates$Var1->SE`) and then compute the
  95% (or some other %) confidence interval as \\\mu\\ \pm\\
  1.96\times\\ SE\\ where \\\mu\\ again is the estimate from the model
  using the original data.

- If it shows skew (which it likely will, even if it’s just fat tails),
  you need to use some form of a quantile
  (e.g.`quantile(SE,c(0.025,0.975)`). You cannot, however, derive the CI
  from a simple call to `quantile`. The interval you get might not
  include the mean you obtained using the original data.

  - If you need to use quantile intervals, you need to use a
    bias-corrected (and/or accelerated, BCa) confidence interval.
    - The `bcaboot` package will perform it for you, but it handles all
      steps of the bootstrap process from resampling data to computing
      statistics. So unless the statistics you’re after are
      straightforward (or your resampling scheme is simple) you may not
      be able to utilize it.
    - Another version exists in the`coxed` package via `coxed::bca()`.
      Do not use this. It is not appropriate for a non-parametric
      bootstrap.
    - The `MDChelp` package has functions that will compute the BCa
      interval given a table of bootstrapped estimates and the original
      data (`MDChelp::bca_jacknife`). Right now, it’s only been tested
      for creating a table of regression parameter estimates and the
      CI’s \\(\beta's)\\ but you might be able to get it to do the same
      for model-based estimates (e.g. marginal means, contrasts, etc.)
      depending on how you identify a parameter of interest in the table
      and the function you provide.

### Deriving P-values

You typically don’t estimate a p-value from the bootstrap. To do that,
you would need a permutation test. If your inference relies on reporting
or interpreting p-values:

1.  Don’t.
2.  Use a permuation test instead. There are “approximations” you can
    use by calculating T (or presumably F) statistics, but again, this
    seems at odds with the whole purpose of using a non-parametric
    bootstrap.

## Example Workflow

Here is a real worked example that includes tips to save on
computational speed/ memory.

#### Computational tips

When you do a real bootstrap, you can take advantage of parallel
processing and avoid running into memory limits by writing the
bootstrapped data to a file on your hard drive. This makes it very easy
to use `furrr` and `purrr` to execute your analysis and organize your
results.

### Create bootstrapped data

``` r
#create a set of dummy data

 data.frame(X=rnorm(100,0,5),
            G=rep(c(1,0),each=50),
            noise=rnorm(100,0,1)
           )%>%
  mutate(Y=1+2*G+0.2*X+noise)%>%
  select(-noise)->tmp

folder.name<-"Yourpath/boot_data/"
#complete path to a folder to hold bootstrapped data. Create this folder ahead of time

#assume G is part of the sampling design, but is not random so observations must be randomly sampled within each level of G

# write the function to do the bootstrapping and write to file, given just the original data and a counter, i. Essentially write a function that resamples the data appropriately, once.


make.bootdat<-function(X,i){
  X%>%
    split(.$G)%>%
    map(.,~slice_sample(.x,n=nrow(.x), replace=TRUE))%>%
    bind_rows()->out
  
  saveRDS(out,file=paste0(folder.name,'bootdat_',i,'.rds')) 
  #always save RDS, use the counter i to name each replicate data set   
}

# now use purrr:: walk to execute the above function multiple times

purrr::walk(c(1:100),~make.bootdat(tmp,.x))
```

Now the data is written to a folder specified in folder path.

There may or may not be any memory savings when generating data by
executing in parallel, but there almost certainly will be when running
your analysis. Parallel processing is easy in the `furrr` package
because in our case, it will split the data objects just created among
however many cores/workers we specify. This means it might be helpful to
do some math ahead of time and figure out how many cores you want to
assign. Most mid-range computers now will have up to 8 ‘logical’ cores
(4 physical) than can be use (assuming a Windows OS). You need to
reserve at least 1 to handle other processes. Beyond that it’s up to
you. I generally try to use the number of cores that results in the same
number of data objects being assigned to each core. No clue if this
actually helps or hurts, but it’s what I do…

### Create model objects

``` r
#create a list of file paths for all the bootstrapped data

bootdat.list<-list.files(path='your path/boot_data/',
                         full.names=TRUE,
                         pattern='.rds')

#write a function that will take the file path, read in that data object, do the desired analysis. you can make it write to memory/ workspace, or to a folder again. I like to write the model object (if using a model) at this stage because then you have more flexibility later on to get regression coefficents or compute contrasts/ marginal means without needing to re-run the model


reg.fxn.1<-function(X){
  readRDS(file=X)->boot.dat
  
  lm(Y~X+G,data=boot.dat)->out
  
  return(out)
}

#same as above, but writes to file. Needs a counter, i

reg.fxn.2<-function(X,i){
  readRDS(file=X)->boot.dat
  
  lm(Y~X+G,data=boot.dat)->out
  
  saveRDS(out,file=paste0('Yourpath/boot_mods/model_',i,'.rds'))
}


#non-parallel, write results to memory/workspace

map(bootdat.list,~reg.fxn.1(.x))->coef.tab.list




# set up the parallel processing structure only use if you write the objects to a file, not to memory.

future::plan(strategy='multisession',
             workers=5)
# use walk2 because we are writing to a file, and create the counter vector as a second argument for walk2 
furrr::future_walk2(bootdat.list,
                    c(1:length(bootdat.list)),
                    ~reg.fxn.2(.x,.y))


future::plan(strategy='sequential') #turn off parallel processing
```

From here you can use
[`purrr::map`](https://purrr.tidyverse.org/reference/map.html) to
process the model object further, depending on what you want to do. Just
remember, if you want to do it in parallel,you need to be
reading/writing from file, not from memory. But once you have your
desired result (maybe just the model object), you can store it in
memory. For simplicity here, we will just assume we want to just get a
table of regression coefficients and the BCa intervals. I’ll use the
model objects stored in the workspace, but the only tweak needed to be
made to the function is to have it read in the data first, like we did
in `reg.fxn.2()`.

### Summarize estimates and compute BCa intervals

The `MDChelp::bca_jacknife()` function needs a table of named parameters
and the bootstrapped estimates. This is easy to obtain using
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html). It
wants the column that represents the parameter value to be called
“theta.boot”. You also need to pass a function that replicates the
analysis step previously-whatever you wanted to do to the original data
to make it match the format of the bootstrapped estimates (e.g. the
reg.fxn above). It may need some slight tweaks so that it doesn’t write
to a folder.

``` r
map(coef.tab.list,~broom::tidy(.x)%>%
      select(term,
             theta.boot=estimate))%>% #rename the value to theta.boot
  bind_rows()->boot.param.tab # bind rows to make it all one data frame

#function to analyze the original data, similar to reg.fxn above.
bca.fxn<-function(X){
  lm(Y~G+X,data=X)%>%
    broom::tidy(.)%>%
    select(term, estimate)->out
  
  return(out)
}



MDChelp::bca_jacknife(boot.param.tab,tmp,bca.fxn,alpha=0.05,multcomp=FALSE)
```

Alpha sets the error rate and multcomp determines whether that is the
family-wide error rate (testing whether all parameters are = 0) or on a
per-variable basis. This uses the Sidak method of correction. This is
only intended to be used when returning a table of regression
coefficients. If you are doing something other than returning a table of
regression coefficients (e.g. using emmeans, ggeffects), you should set
`multcomp=FALSE`.
