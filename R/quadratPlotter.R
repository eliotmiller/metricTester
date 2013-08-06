#' Plot simulated quadrats in arena
#'
#' Given a matrix of quadrat bounds, plots the quadrats in an already plotted,
#' simulated arena
#'
#' @param quadrat_bounds Matrix of quadrat bounds
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
#' temp <- phyloNtraits(50)
#'
#' scaled <- scaler(temp[[2]], min.arena=0, max.arena=300)
#'
#' phydistmatrix <- cophenetic(temp[[1]])
#'
#' #define a color for each species
#' cols <- blue2green2red(nrow(phydistmatrix))
#'
#' positions <- locationSampler(phyloNtraits.results=temp, scaled.results=scaled, mean.log.individuals=4, length.parameter=5000, sd.parameter=50)
#'
#' #plot the arena. don't close the window
#' plot(positions$X, positions$Y, pch=20, cex=0.5, xlim=c(0,300), ylim=c(0,300), col=cols[positions$individuals])
#'
#' bounds <- quadratPlacer(no.quadrats=15, x_max=300, y_max=300, quadrat_size=50)
#'
#' quadratPlotter(bounds)

quadratPlotter <- function(quadrat_bounds)
{
	for(i in 1:dim(quadrat_bounds)[1])
	{
		polygon(c(quadrat_bounds[i,1],quadrat_bounds[i,2],quadrat_bounds[i,2],quadrat_bounds[i,1]),c(quadrat_bounds[i,3],quadrat_bounds[i,3],quadrat_bounds[i,4],quadrat_bounds[i,4]))
	}
}
