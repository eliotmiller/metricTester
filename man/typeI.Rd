\name{typeI}
\alias{typeI}
\title{Test for type I errors}
\usage{
typeI(significance.results, expectation, wrong)
}
\arguments{
  \item{significance.results}{Data frame of significance
  results from call to sigTest()}

  \item{expectation}{Expected value: 0=not significant,
  1=clustered, 2=overdispersed}

  \item{wrong}{Value of a typeI error rate, e.g. 2 if
  expecting 1.}
}
\value{
Matrix with rows corresponding to metrics, and columns for
type I errors, "NoSignal" (i.e. < 50% of communities from
community data matrix exhibiting expected pattern), and
"Good" (i.e. > 50% of communities exhibiting expected
pattern). Values in table are 0s and 1s, where 1
corresponds to a confirmation of the pattern in question.
}
\description{
Sloppy function that needs work. Intended to test for type
I and II errors of results of testing of various metrics
against a single spatial simulations.
}
\details{
Note that IAC is thought to detect clustering if observed
is greater than upper CIs, so we have to explicitly flip
our expectations in the function. See example below for how
to test for type I error rates if expecting random
community structure. Note that it is possible to have a
type I error irrespective of power of test, so a row can
have more than one 1 in it.
}
\examples{
library(plyr)
library(geiger)
library(picante)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

system.time(allMetricsNull(tree=tree, orig.matrix=cdm, null.method="richness",
no.randomizations=10, temp.file="output.csv"))

possibilities <- read.csv("output.csv")

#call the summaries function from within a ddply statement
expectations <- ddply(possibilities, .(richness), summaries)

#calculate the observed metrics
observed <- allMetrics(tree, cdm)

#important merge command, confirm it works
results <- merge(observed, expectations, sort=FALSE)

oneMetric <- sigTest(results, "PSV")

#example of how to loop it over a table of results
metric.names <- names(observed)[3:21]

sig.results <- list()

for(i in 1:length(metric.names))
{
	sig.results[[i]] <- sigTest(results, metric.names[i])
}

sig.results <- as.data.frame(sig.results)

names(sig.results) <- metric.names

error.summ <- typeI(sig.results, expectation=1, wrong=2)

#if you are expecting 0s (random structure), then use: expectation=0, wrong=1|2
}
\references{
Miller, Trisos and Farine.
}

