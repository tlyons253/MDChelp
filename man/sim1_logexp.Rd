% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logexp.R
\name{sim1_logexp}
\alias{sim1_logexp}
\title{A simple logistic exposure survival simulation.}
\usage{
sim1_logexp(
  S.int = 0.95,
  nind = 10,
  ntime = 10,
  p.obs = 0.5,
  obs.start = TRUE,
  r.censor = FALSE,
  p.censor = 0.1,
  t.censor = 0.8,
  cjs = FALSE,
  logexp = TRUE
)
}
\arguments{
\item{S.int}{the interval survival probability}

\item{nind}{the number of individuals}

\item{ntime}{the length of the encounter histories}

\item{p.obs}{a probability to create differences in exposure period}

\item{obs.start}{TRUE or FALSE, if true, all individuals are observed at
time =1, otherwise. all individuals may not be observed until
later in their encounter history, or not at all before failing}

\item{r.censor}{TRUE or FALSE, the probability an individual is right censored.
The function defaults to having all individuals observed at
the last time interval (as is typical in nest survival studies)}

\item{p.censor}{the probability an individual is right censored before time = ntime}

\item{t.censor}{controls when censoring occurs by treating censoring time as a binomial
process where the number of trials is the length of the encounter history
and t.censor is the probability, and the resulting number of successes
is the interval at which censoring occurs}

\item{cjs}{if TRUE, the encounter history is output in a matrix typical of CJS matrix, suitable
for use in a bayesian frameowrk}

\item{logexp}{of TRUE, returns a data frame of rows of individual observation
intervals for use in glm or similar}
}
\value{
with logexp = TRUE, a data frame of nest-visits with the interval between
visits in a separate column.
}
\description{
simulate logistic exposure survival data, returns a "long"
object with n.individuals X n observations rows or a 'wide' object for
a different analysis method. permits right censoring
}
\examples{
# Show a long-format survival data set using the above defaults
\dontrun{

sim1_logexp(r.censor=TRUE,p.censor=0.2)->sim.dat

}
}
