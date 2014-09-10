\name{lengthNonZeros}
\alias{lengthNonZeros}
\title{Calculate the species richness of a community}
\usage{
lengthNonZeros(input.vector)
}
\arguments{
  \item{input.vector}{A vector from a community data matrix
  of abundances.}
}
\value{
A named vector of species richness.
}
\description{
Given a vector of abundances or presence/absences from a
community data matrix, will calculate the species richness
of that community.
}
\details{
An internal function to calculate richness of a cdm.
}
\examples{
library(geiger)
library(picante)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

#note that with this example, each community in the cdm will be labeled by its richness
apply(cdm, 1, lengthNonZeros)
}
\references{
Miller, Trisos and Farine.
}

