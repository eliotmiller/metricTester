\name{regionalNull}
\alias{regionalNull}
\title{Regional null model}
\usage{
regionalNull(cdm, tree, regional.abundance)
}
\arguments{
  \item{cdm}{Picante-style community data matrix with
  communities/quadrats/plots/etc as rows and species as
  columns}

  \item{tree}{Ape-style phylogeny}

  \item{regional.abundance}{Vector of species names, where
  each species' name is repeated the number of times
  necessary to accomodate its abundance in the regional
  species pool}
}
\value{
A matrix with all species in the input tree in phylogenetic
order, and the same number of randomized quadrats as used
in the input community data matrix
}
\description{
Entirely vectorized null model that maintains species
richness (approximately only during this phase of the
calculation, but we do so strictly later on), species
occurrence frequency, and species abundance distributions.
}
\details{
Although not as fast as, e.g. randomizeMatrix, this
functions does not contain any for loops and so still runs
decently fast. It works by drawing the total number of
individuals observed in the input plot from the regional
abundance vector. Thus while a randomized quadrat will not
necessarily have the same number of species as the observed
quadrat, over many iterations it will likely be sampled. We
can then concatenate the results by richness at the end
which will only compare observed values to random quadrats
of the same richness. As an example, an observed quadrat
might have two individuals of speciesA and two of speciesB.
If the regional abundance vector is
c("spA","spA","spA","spA","spB","spB","spB","spC"), and we
draw four individuals, it would be possible to draw 1, 2,
or 3 species, but in general, two species would be seen in
the randomized quadrats.
}
\examples{
library(ape)
library(geiger)
library(plyr)
library(picante)

tree <- sim.bdtree(stop="taxa", n=50)

arena <- randomArena(tree, 0, 300, 0, 300, 3.2)

bounds <- quadratPlacer(15, 300, 300, 30)

temp.cdm <- quadratContents(arena$arena, bounds)

cdm <- t(temp.cdm)

regionalNull(cdm, tree, arena$regional.abundance)
}
\references{
Miller, Trisos and Farine.
}

