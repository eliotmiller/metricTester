#' Wrapper for prepping and calculating observed metrics
#'
#' Given a cdm and phylogeny, this function preps the data and calculates all metrics that
#' are defined in defineMetrics.
#'
#' @param tree Phylo object
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
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
#' positions <- competitionArena(prepped)
#'
#' tempCDM <- makeCDM(positions, 15, 30)
#'
#' results <- observedMetrics(tree=tree, picante.cdm=tempCDM$cdm)

observedMetrics <- function(tree, picante.cdm)
{
	#prep the observed data to run the metric calculations across it
	metricsPrepped <- prepData(tree, picante.cdm)
	#calculate observed scores and set aside for later
	observed <- as.data.frame(calcMetrics(metricsPrepped))
	observed
}