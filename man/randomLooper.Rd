\name{randomLooper}
\alias{randomLooper}
\title{Test metrics across multiple random arenas}
\usage{
  randomLooper(no.species, x.min, x.max, y.min, y.max,
    no.quadrats, quadrat_size, mean.log.individuals,
    null.method, concatBYrichness = TRUE,
    no.randomizations, expectation, wrong, no.metrics,
    iterations, temp.file, output.file)
}
\arguments{
  \item{no.species}{Number of species in each arena}

  \item{x.min}{Minimum X coordinate of arena, e.g. 0}

  \item{x.max}{Maximum X coordinate of arena}

  \item{y.min}{Minimum Y coordinate of arena, e.g. 0}

  \item{y.max}{Maximum Y coordinate of arena}

  \item{no.quadrats}{Number of quadrats to sample}

  \item{quadrat_size}{Size of an individual quadrat}

  \item{mean.log.individuals}{Mean log of abundance vector
  from which species abundances will be drawn}

  \item{null.method}{A picante-style null, e.g. "richness"
  or "frequency"}

  \item{concatBYrichness}{Whether to concatenate null
  results by the richness of the randomized quadrat (the
  default), or by the quadrat ID (traditional method)}

  \item{no.randomizations}{Number of iterations the
  function should run, i.e. the number of times the
  orig.matrix will be shuffled and the metrics calculated
  on it}

  \item{expectation}{Expected value: 0=not significant,
  1=clustered, 2=overdispersed}

  \item{wrong}{Value of a typeI error rate, e.g. 2 if
  expecting 1.}

  \item{no.metrics}{Need to specify how many metrics are
  being tested}

  \item{iterations}{Number of arenas to simulate and test}

  \item{temp.file}{File name of output file where null
  metric values are saved to. Re-written each iteration}

  \item{output.file}{File name of results file}
}
\value{
  Two csvs and a matrix of results summarizing the type I
  and type II errors across all metrics and spatial
  simulations. One csv is just a temporary file storing the
  null expectations, the other is a csv of the same thing
  as the output matrix.
}
\description{
  Large, somewhat sloppy function tying many previous
  functions together into a single simulator function that
  generates random spatial arenas, samples quadrats,
  generates null expectations, tests for significance of
  observed metrics, and summarizes results as a matrix of
  type I and type II error rates.
}
\details{
  Could easily modify this function to save more
  information than it currently does, though obviously
  beware the additional space such an operation might
  require. A single null.csv file for 19 metrics by 1000
  iterations is about ~50 megabytes. The results matrix is
  also written to csv in case the function crashes part-way
  through.
}
\examples{
library(ape)
library(geiger)
library(colorRamps)
library(plyr)
library(picante)

randomLooper(no.species=50, x.min=0, x.max=300, y.min=0, y.max=300, no.quadrats=15,
quadrat_size=50, mean.log.individuals=3.2, null.method="richness",
concatBYrichness=TRUE, no.randomizations=2, expectation=0, wrong=1|2, no.metrics=19,
iterations=2, temp.file="deleteme.csv", output.file="random.csv")
}
\references{
  Miller, Trisos and Farine.
}

