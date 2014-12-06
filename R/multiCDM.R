#' Wrapper for deriving CDMs from the results of multiple spatial simulations
#'
#' Given the results of a call to runSimulations(), this function places quadrats down
#' randomly (though identically across simulations). 
#'
#' @param simulations.result List of data frames of three columns: 
#' "individuals", "X", and "Y"
#' @param no.quadrats Number of quadrats to place
#' @param quadrat.length Length of one side of desired quadrat
#' 
#' @details Both the size and number of quadrats
#' are determined by the user. A conservative check (perhaply overly so) is in place to
#' ensure the function doesn't get stuck looking for solutions for how to randomly place
#' non-overlapping quadrats down. Either decreasing the number of size of quadrats is
#' necessary if this throws and error.
#'
#' @return A list of data frames.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(dplyr)
#' library(geiger)
#' library(picante)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=4,
#' 	length.parameter=1000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
#'	competition.iterations=5)
#'
#' #run the spatial simulations
#' arenas <- runSimulations(prepped)
#'
#' #derive CDMs. quadrats are placed in the same places across all spatial simulations
#' cdms <- multiCDM(arenas, no.quadrats=10, quadrat.length=20)

multiCDM <- function(simulations.result, no.quadrats, quadrat.length)
{
	bounds <- quadratPlacer(no.quadrats, arena.length=max(simulations.result[[1]]$dims), 
		quadrat.length)
	results <- lapply(1:length(simulations.result), function(x)
		quadratContents(arena=simulations.result[[x]]$arena, quadrat.bounds=bounds))
	names(results) <- names(simulations.result)
	results
}
