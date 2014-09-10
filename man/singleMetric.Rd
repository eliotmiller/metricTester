\name{singleMetric}
\alias{singleMetric}
\title{Calculate specific phylogenetic community structure metric}
\usage{
singleMetric(tree, picante_cdm, metric)
}
\arguments{
  \item{tree}{Phylo object}

  \item{picante_cdm}{A picante-style community data matrix
  with sites as rows, and species as columns}

  \item{metric}{A phylogenetic community structure metric
  of interest. Options are: "mpd" (non-abundance weighted
  MPD), "interspecific" (interspecific abundance-weighted
  MPD), "intraspecific", "complete", "mntd" (non-abundance
  weighted MNTD), "aw.mntd", "psv", "psc", "pse", "pae",
  "iac", "haed", "eaed", "eed", "hed", "simpson", "pd",
  "pd.c" (Cadotte's re-defined PD), and "qe".}
}
\value{
A data frame with the calculated metric and the associated
species richness of all input "communities".
}
\description{
Given a phylo object, and a picante-style community data
matrix (sites are rows, species are columns), calculate a
phylogenetic community structure metric of interest.
}
\details{
Useful wrapper function to calculate a number of
phylogenetic community structure metrics from different
packages.
}
\examples{
library(geiger)
library(picante)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

results <- singleMetric(tree, cdm, "mpd")
}
\references{
Miller, Trisos and Farine.
}

