\name{settleSome}
\alias{settleSome}
\title{Randomly settle individuals in a spatial arena}
\usage{
  settleSome(killSomeOutput)
}
\arguments{
  \item{killSomeOutput}{Output from the killSome function}
}
\value{
  A list of 4 elements: the average relatedness in the
  geographic neighbordhood of consideration (passed
  directly from the killSome output, not re-calculated
  here), the regional abundance vector, the new spatial
  arena, and the dimensions of that arena.
}
\description{
  Given output from the killSome function, randomly settles
  individuals in the arena.
}
\details{
  This function uses the number killed element of the
  killSome output to randomly draw from the regional
  abundance vector, then settles the individuals at random
  in the arena.
}
\examples{
library(geiger)

#simulate tree with birth-death process
tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

#create a random arena
arena <- randomArena(tree, x.min=0, x.max=300, y.min=0, y.max=300, mean.log.individuals=2)

#remove some of the most closely related individuals
new.arena <- killSome(tree, arenaOutput=arena, max.distance=50, percent.killed=0.2)

dim(arena$arena)
dim(arena$new.arena)

#now settle some indiviudals

newer.arena <- settleSome(new.arena)

dim(new.arena$arena)
dim(newer.arena$arena)
}
\references{
  Miller, Trisos and Farine.
}

