\name{phyloNtraits}
\alias{phyloNtraits}
\title{Generate phylogeny with trait data}
\usage{
  phyloNtraits(no.species)
}
\arguments{
  \item{no.species}{Desired number of species in resulting
  phylogeny}
}
\value{
  A list where the first object is a phylogeny with the
  desired number of species and the second object is a
  matrix of trait values for those species.
}
\description{
  Given a desired number of species, will generate a tree
  with that many species and associated trait data for two
  traits following a Brownian motion evolution model.
}
\details{
  Uses geiger's sim.bdtree function with b=0.1 and d=0.
  Evolves two traits up phylogeny with Brownian motion
  evolution process.
}
\examples{
library(geiger)

results <- phyloNtraits(50)
}
\references{
  Miller, Trisos and Farine.
}

