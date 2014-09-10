\name{psc.corr}
\alias{psc.corr}
\title{Calculate corrected PSC}
\usage{
psc.corr(samp, tree)
}
\arguments{
  \item{tree}{Phylo object}

  \item{samp}{A picante-style community data matrix with
  sites as rows, and species as columns}
}
\value{
A data frame of correctly calculated PSC values, with
associated species richness and name of all communities in
input cdm
}
\description{
Given a phylo object and a picante-style community data
matrix (sites are rows, species are columns), calculated
corrected phylogenetic species clustering
}
\details{
Returns the inverse of psc as defined in picante
}
\examples{
library(geiger)
library(picante)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

results <- psc.corr(samp=cdm, tree=tree)
}
\references{
Helmus et al 2007
}

