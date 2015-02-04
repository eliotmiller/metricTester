#' Plot simulated quadrats in arena
#'
#' Given a matrix of quadrat bounds, plots the quadrats in an already plotted,
#' simulated arena
#'
#' @param quadrat.bounds Matrix of quadrat bounds
#' 
#' @details Plots quadrats as defined by the supplied matrix, e.g. a call to quadratPlacer
#' An active plot with the simulated arena needs to already be open, see example.
#'
#' @return Plotted quadrats
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#' library(colorRamps)
#'
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' temp <- evolveTraits(tree)
#'
#' phydistmatrix <- cophenetic(temp[[1]])
#'
#' #define a color for each species
#' cols <- blue2green2red(nrow(phydistmatrix))
#'
#' #prep the data for the simulation
#' prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=2, 
#' length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
#' competition.iterations=25)
#'
#' positions <- filteringArena(prepped)
#'
#' #plot the arena. don't close the window
#' plot(positions$arena$X, positions$arena$Y, pch=20, cex=0.5, xlim=c(0,300), ylim=c(0,300), 
#' col=cols[positions$arena$individuals])
#'
#' bounds <- quadratPlacer(no.quadrats=10, arena.length=300, quadrat.length=50)
#'
#' quadratPlotter(bounds)

quadratPlotter <- function(quadrat.bounds)
{
	for(i in 1:dim(quadrat.bounds)[1])
	{
		polygon(c(quadrat.bounds[i,1],quadrat.bounds[i,2],quadrat.bounds[i,2],
		quadrat.bounds[i,1]),c(quadrat.bounds[i,3],quadrat.bounds[i,3],
		quadrat.bounds[i,4],quadrat.bounds[i,4]))
	}
}
