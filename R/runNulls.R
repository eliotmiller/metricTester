#' Randomize input CDM according to defined null models
#'
#' Given a prepared nulls.input object, will randomize a community data matrix according
#' to specified null models, and return a list of randomized CDMs.
#'
#' @param nulls.input Prepped nulls.input object
#' @param nulls Optional. If not provided, defines the nulls as all of those in
#' defineNulls. If only a subset of those is desired, then nulls should take
#' the form of a character vector corresponding to named functions from defineNulls.
#' The available nulls can be determined by running names(defineNulls()). Otherwise,
#' if the user would like to define a new null on the fly, the argument nulls can take
#' the form of a named list of new functions (nulls). If the
#' latter, new_ must be set to TRUE. 
#' @param new_ Whether or not new nulls are being defined on the fly. Default is FALSE.
#' Set to TRUE if a new null is being used.
#' 
#' @details Determine which nulls will be calculated by running names(defineNulls()).
#' If only a subset of these is desired, supply metrics with a character vector of the
#' named, available metrics.
#'
#' @return A list of matrices. Each matrix is a product of a randomization of the input
#' CDM and one of the specified null models.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' prepped <- prepNulls(tree, cdm)
#'
#' results <- runNulls(prepped)

runNulls <- function(nulls.input, nulls, new_=FALSE)
{
	if(!inherits(nulls.input, "nulls.input"))
	{
		stop("Input needs to be of class 'nulls.input'")
	}
	
	#if a list of named nulls functions is not passed in, assign nulls to be NULL, in
	#which case all nulls will be run
	if(missing(nulls))
	{
		nulls <- NULL
	}	
	
	nulls <- checkNulls(nulls, new_=new_)

	#finally, if no distances.among input was provided, the dispersalNull cannot be run.
	#remove it here. if other nulls are defined later, either add them to this list, or
	#do something more elegant so they are not run and errors are not thrown. this used to
	#check whether distances.among == ignore, but that was throwing lots of warnings if it
	#did not equal ignore. now it checks whether it is a matrix. if its not it removes
	#the dispersal null. it does not have to say ignore specifically anymore.
	
	if(!is.matrix(nulls.input$distances.among))
	{
		nulls$dispersal <- NULL
	}

	lapply(nulls, function(x) x(nulls.input))
}
