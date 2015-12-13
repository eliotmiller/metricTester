#' Wrapper for creating a CDM from a spatial simulation result
#'
#' Given the results of a single spatial simulation, and a desired number of quadrats
#' and the length of one side of each quadrat, will place the quadrats down and output
#' a CDM. Importantly, also carries along the regional abundance vector from the
#' spatial simulation results if one was included.
#'
#' @param single.simulation The results of a single spatial simulation, e.g. a call to
#' randomArena
#' @param no.quadrats The desired number of quadrats in the final CDM
#' @param quadrat.length The length of one side of each quadrat
#'
#' @details Just a simple wrapper function to quickly turn spatial simulations into CDMs
#' for subsequent analysis.
#'
#' @return A list with the regional abundance from the single simulation result, if it
#' included such a result, or the results of a call to abundanceVector() if not. The list
#' also includes the CDM based on the parameters (number and size of quadrats) provided.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #prep the data for the simulation
#' prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=2, 
#' length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
#' competition.iterations=3)
#'
#' competition <- competitionArena(prepped)
#'
#' test <- makeCDM(competition, 15, 30)

makeCDM <- function(single.simulation, no.quadrats, quadrat.length)
{
	#use the quadratPlacer function to get the bounds of quadrats (notice we do not get
	#the dists here)
	temp1 <- quadratPlacer(no.quadrats, max(single.simulation$dims), quadrat.length)
	bounds <- temp1$quadrat.bounds
	temp2 <- quadratContents(arena=single.simulation$arena, quadratPlacer.results=temp1)
	if(is.null(single.simulation$regional.abundance))
	{
		regional.abundance <- abundanceVector(temp2$cdm)
	}
	else
	{
		regional.abundance <- single.simulation$regional.abundance
	}
	results <- list("regional.abundance"=regional.abundance, "dists"=temp2$dists,
		"cdm"=temp2$cdm)
	results
}
