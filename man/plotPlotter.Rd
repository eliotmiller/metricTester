% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotPlotter.R
\name{plotPlotter}
\alias{plotPlotter}
\title{Plot simulated plots in arena}
\usage{
plotPlotter(plot.bounds)
}
\arguments{
\item{plot.bounds}{Matrix of plot bounds}
}
\value{
Plotted plots
}
\description{
Given a matrix of plot bounds, plots the plots in an already plotted,
simulated arena.
}
\details{
Plots plots as defined by the supplied matrix, e.g. a call to
plotPlacer. An active plot with the simulated arena needs to already be open, 
see example.
}
\examples{

tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)

temp <- evolveTraits(tree)

phydistmatrix <- ape::cophenetic.phylo(temp[[1]])

#define a color for each species
cols <- plotrix::color.scale(x=1:nrow(phydistmatrix),
cs1=c(0.2,0.4,0.8), cs2=c(0,0.5,0.8), cs3=c(1,0.5,0))

#prep the data for the simulation
prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=2, 
length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
competition.iterations=3)

positions <- filteringArena(prepped)

#plot the arena. don't close the window
plot(positions$arena$X, positions$arena$Y, pch=20, cex=1, xlim=c(0,300), ylim=c(0,300), 
col=cols[positions$arena$individuals])

bounds <- plotPlacer(no.plots=10, arena.length=300,
plot.length=50)$plot.bounds

plotPlotter(bounds)
}
\references{
Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
structure metrics and null models: a review with new methods and software.
Ecography DOI: 10.1111/ecog.02070
}
