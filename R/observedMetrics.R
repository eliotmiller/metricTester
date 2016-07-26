#' Wrapper for prepping and calculating observed metrics
#'
#' Given a cdm and phylogeny, this function preps the data and calculates metrics of the
#' user's choice
#'
#' @param tree Phylo object
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param metrics Optional. If not provided, defines the metrics as all of those in
#' defineMetrics. If only a subset of those is desired, then metrics should take
#' the form of a character vector corresponding to named functions from defineMetrics.
#' The available metrics can be determined by running names(defineMetrics()). Otherwise,
#' if the user would like to define a new metric on the fly, the argument can take
#' the form of a named list of new functions (metrics).
#'
#' @details A simple wrapper function to quickly prep data and calculate observed metrics.
#'
#' @return A data frame with the species richness and calculated phylogenetic community
#' structure metrics for all input plots from the CDM.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #prep the data for the simulation
#' prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=2, 
#' 	length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
#' 	competition.iterations=3)
#'
#' positions <- randomArena(prepped)
#'
#' tempCDM <- makeCDM(positions, 15, 30)
#'
#' results <- observedMetrics(tree=tree, picante.cdm=tempCDM$picante.cdm)
#'
#' #example of how to pass specific metrics to be calculated (always use at least 
#' #richness). not run
#'
#' #results <- observedMetrics(tree=tree, picante.cdm=tempCDM$picante.cdm, 
#' #metrics=c("richness","PSV"))

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
