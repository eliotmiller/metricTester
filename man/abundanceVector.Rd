\name{abundanceVector}
\alias{abundanceVector}
\title{Generate regional abundance vector}
\usage{
abundanceVector(cdm)
}
\arguments{
  \item{cdm}{Community data matrix in picante format}
}
\value{
A character vector in the form "s1, s1, s1, s2, s2, s3,
etc".
}
\description{
Given a community data matrix of sites by species, extract
the column-wise sums (the total number of individuals of
each species) and expand to create a regional abundance
vector.
}
\details{
Simple function to create a regional abundance vector given
a "regional" community data matrix.
}
\examples{
library(plyr)
library(geiger)
library(picante)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))

cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

abund <- abundanceVector(cdm)
}
\references{
Miller, Trisos and Farine.
}

