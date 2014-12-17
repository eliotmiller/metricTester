#' Wrapper for prepping and calculating observed metrics
#'
#' Given a cdm and phylogeny, this function preps the data and calculates all metrics that
#' are defined in defineMetrics.
#'
#' @param tree Phylo object
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param metrics Optional list of named metric functions to use. These
#' must be defined in the defineMetrics function. If invoked, this option will likely
#' be used to run a subset of the defined metrics.

#'
#' @details A simple wrapper function to quickly prep data and calculate observed metrics.
#'
#' @return A data frame with the species richness and calculated phylogenetic community
#' structure metrics for all input quadrats from the CDM.
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
#' #prep the data for the simulation
#' prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=4, 
#' length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
#' competition.iterations=3)
#'
#' positions <- randomArena(prepped)
#'
#' tempCDM <- makeCDM(positions, 15, 30)
#'
#' results <- observedMetrics(tree=tree, picante.cdm=tempCDM$cdm)
#'
#' #example of how to pass specific metrics to be calculated (always use at least 
#' #richness)
#'
#' results <- observedMetrics(tree=tree, picante.cdm=tempCDM$cdm, 
#' metrics=list("richness"=my_richness,"PSV"=my_psv))

observedMetrics <- function(tree, picante.cdm, metrics)
{
	#if a list of named metric functions is not passed in, assign metrics to be NULL, in
	#which case all metrics will be calculated
	if(missing(metrics))
	{
		metrics <- NULL
	}
	
	#prep the observed data to run the metric calculations across it
	metricsPrepped <- prepData(tree, picante.cdm)
	#calculate observed scores and set aside for later
	observed <- as.data.frame(calcMetrics(metricsPrepped, metrics))
	observed
}
