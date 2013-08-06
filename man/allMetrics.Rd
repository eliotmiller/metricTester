\name{allMetrics}
\alias{allMetrics}
\title{Calculate phylogenetic community structure metrics}
\usage{
  allMetrics(tree, picante_cdm)
}
\arguments{
  \item{tree}{Phylo object}

  \item{picante_cdm}{A picante-style community data matrix
  with sites as rows, and species as columns}
}
\value{
  A data frame with the calculated 19 metrics and the
  associated species richness of all input "communities".
}
\description{
  Given a phylo object, and a picante-style community data
  matrix (sites are rows, species are columns), calculate
  all phylogenetic community structure metrics of interest.
}
\details{
  Currently we are calculating 19 phylogenetic community
  structure metrics
}
\examples{
library(geiger)
library(picante)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

results <- allMetrics(tree, cdm)
}
\references{
  Miller, Trisos and Farine.
}

