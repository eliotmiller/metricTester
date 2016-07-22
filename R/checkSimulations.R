#' Confirm that the spatial simulation functions are in suitable format
#'
#' Utility function. Creates a list of spatial simulations, either those defined in
#' defineSimulations or a named list of simulation functions.
#'
#' @param x Optional. If not provided, defines the simulations as those in
#' defineSimulations. Else
#' either a character vector or a named list of functions, depending on whether new_ is
#' set to TRUE or FALSE. See runSimulations.
#' @param new_ Whether or not new simulations are being defined on the fly. Default is
#' FALSE. Set to TRUE if a new simulation is being used.
#' 
#' @details A few quick checks to confirm the spatial simulations are in suitable format.
#'
#' @return A list of functions.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' checkSimulations(names(defineSimulations()))

checkSimulations <- function(x, new_=FALSE)
{
	#if nothing is passed to checkSimulations, then just calculate all simulations
	if (is.null(x))
	{
		simulations <- defineSimulations()
	}

	#if a character vector is passed to checkSimulations, and new_ remains FALSE, run the
	#unexported function simulationNameMatcher. this checks whether all the names in the
	#character vect match named functions from defineSimulations, then outputs named list
	else if(!is.null(x) & new_==FALSE)
	{
		if (!inherits(x, "character"))
		{
			stop("The simulations need to be input as a character vector of named functions")
		}
					
		simulations <- simulationNameMatcher(simulation.name.vector=x)
	}
	
	#if a new simulation is passed to checkSimulations, then new_ needs to be set to TRUE. 
	#in this case, it needs to be passed as a named list
	else if(!is.null(x) & new_==TRUE)
	{
		if (!inherits(x, "list"))
		{
			stop("New simulations need to be input as a list of named functions")
		}
		if (is.null(names(x)))
		{
			stop("New simulations need to be input as a list of named functions")
		}
		
		simulations <- x
	}					
	simulations
}

simulationNameMatcher <- function(simulation.name.vector)
{
	#define all simulations here
	allPossible	<- defineSimulations()
	
	#throw an error if the simulation.name.vector does not perfectly match named simulations
	if(length(setdiff(simulation.name.vector, names(allPossible))) > 0)
	{
		stop("Not all of your specified simulations match those named in defineSimulations()")
	}
	
	#now subset them to just those defined in simulation.name.vector
	output <- allPossible[simulation.name.vector]
	
	output
}
