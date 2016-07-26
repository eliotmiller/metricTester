#' Run defined spatial simulations
#'
#' Given a prepared simulations.input object, will run all specified spatial simulations,
#' and return a list of randomized CDMs.
#'
#' @param simulations.input Prepped simulations.input object
#' @param simulations Optional. If not provided, defines the simulations as all of those
#' in defineSimulations. If only a subset of those simulations is desired, then
#' simulations should take the form of a character vector corresponding to named functions
#' from defineSimulations. The available simulations can be determined by running
#' names(defineSimulations()). Otherwise, if the user would like to define a new
#' simulation on the fly, the argument simulations can take the form of a named list of
#' new functions (simulations). In this case, new_ must be set to TRUE.
#' @param new_ Whether or not new simulations are being defined on the fly. Default is
#' FALSE. Set to TRUE if a new metric is being used.
#' 
#' @details We ran three spatial simulations in our Ecography paper: neutral, habitat
#' filtering, and competitive exclusion community assembly.
#'
#' @return A list of lists of simulation results, where each of the first-order elements
#' in the list relates to a unique simulation as defined in defineSimulations.
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @export
#'
#' @examples
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=2, 
#' length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
#' competition.iterations=3)
#'
#' results <- runSimulations(prepped)

runSimulations <- function(simulations.input, simulations, new_=FALSE)
{
	if(!inherits(simulations.input, "simulations.input"))
	{
		stop("Input needs to be of class 'simulations.input'")
	}

	#if a list of named simulations is not passed in, assign simulations to be NULL, in
	#which case all simulations will be run	
	if(missing(simulations))
	{
		simulations <- NULL
	}	
	
	simulations <- checkSimulations(simulations, new_=new_)
		
	results <- lapply(simulations, function(x) x(simulations.input))
	
	results
}
