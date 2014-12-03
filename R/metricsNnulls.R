#' Parallelized function that calculates metrics on randomized matrices
#'
#' This function sends out jobs to as many cores as are specified. Each randomizes the
#' input CDM according to all defined null models, then calculates each observed metric on
#' each randomized matrix.
#'
#' @param tree Phylo object
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param regional.abundance A character vector in the form "s1, s1, s1, s2, s2, s3, etc".
#' Optional, will be generated from the input CDM if not provided.
#' @param randomizations The number of times the input CDM should be randomized and the
#' metrics calculated across it.
#' @param cores This function can run in parallel. In order to do so, the user must
#' specify the desired number of cores to utilize.
#'
#' @details This function sends out jobs to as many cores as are specified. Each randomizes the
#' input CDM according to all defined null models, then calculates each observed metric on
#' each randomized matrix.
#'
#' @return A list of lists of vectors. The first level has as many elements as there
#' are randomizations. The second level has one list for each null model. Each element of
#' this second level is a named vector corresponding to the calculated metric at each
#' quadrat.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#' library(picante)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)
#'
#' rawResults <- metricsNnulls(tree, cdm, randomizations=3, cores=1)

metricsNnulls <- function(tree, picante.cdm, regional.abundance=NULL, randomizations=2, 
	cores=1)
{
	require(foreach)
	require(doMC)
	registerDoMC(cores)
	#prep the inputs for parallel randomizations
	nullsPrepped <- prepNulls(tree, picante.cdm, regional.abundance)
	#set up a list to save results into (might not be necessary)
	randomResults <- list()
	#call the parallel for loop. each iteration, save a new list of lists, where each
	#inner element are the metrics for a given null model
	foreach(i = 1:randomizations) %dopar%
	{
		#run the nulls across the prepped data. this randomizes the CDMs all at once
		randomMatrices <- runNulls(nullsPrepped)
		#prep the randomized CDMs to calculate the metrics across them
		randomPrepped <- lapply(randomMatrices, function(x) prepData(tree, x))
		#calculate the metrics
		randomResults[[i]] <- lapply(randomPrepped, calcMetrics)
	}
}
