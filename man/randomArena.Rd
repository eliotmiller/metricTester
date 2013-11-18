\name{randomArena}
\alias{randomArena}
\title{Generate a random spatial arena}
\usage{
  randomArena(tree, x.min, x.max, y.min, y.max,
    mean.log.individuals)
}
\arguments{
  \item{tree}{Phylo object}

  \item{x.min}{The x minimum of the output arena, e.g. 0}

  \item{x.max}{The x maximum of the output arena}

  \item{y.min}{The y minimum of the output arena, e.g. 0}

  \item{y.max}{The y maximum of the output arena}

  \item{mean.log.individuals}{Mean of the log-normal
  distribution}
}
\value{
  A list of 4 elements: the mean of the genetic distance
  matrix of the input phylogeny, the regional abundance
  vector (where each element is a species name, repeated as
  many times as is present in pool), the spatial arena, and
  the dimensions of that arena.
}
\description{
  Given a phylogenetic tree, the desired dimensions of the
  arena, and the mean log of the regional abundance pool,
  randomly generates spatial arena.
}
\details{
  This function generates a log-normal regional abundance
  distribution and assigns those abundances to random
  species. It then draws from this regional abundance
  distribution to settle individuals at random in the
  landscape.
}
\examples{
library(geiger)
library(colorRamps)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

#generate the random arena
arena <- randomArena(tree, x.min=0, x.max=300, y.min=0, y.max=300, mean.log.individuals=2)

#calculate genetic distances
gen.dists <- cophenetic(tree)

#define species' colors for plotting
cols <- blue2green2red(nrow(gen.dists))

#plot the arena
plot(arena$arena$X, arena$arena$Y, pch=20, cex=0.5, xlim=c(0,300), ylim=c(0,300), col=cols[arena$arena$individuals])
}
\references{
  Miller, Trisos and Farine.
}

