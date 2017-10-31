#' Run multiple simulations and calculations to test beta metric + null performance
#'
#' This function runs multiple iterations of the linker function, saving results to file.
#'
#' @param no.taxa The desired number of species in the input phylogeny
#' @param arena.length A numeric, specifying the length of a single side of the arena
#' @param mean.log.individuals Mean log of abundance vector from which species abundances
#' will be drawn
#' @param length.parameter Length of vector from which species' locations are drawn. Large
#' values of this parameter dramatically decrease the speed of the function but result in
#' nicer looking communities
#' @param sd.parameter Standard deviation of vector from which species' locations are 
#' drawn
#' @param max.distance The geographic distance within which neighboring
#' individuals should be considered to influence the individual in question
#' @param proportion.killed The percent of individuals in the total arena that should be
#' considered (as a proportion, e.g. 0.5 = half)
#' @param competition.iterations Number of generations over which to run competition 
#' simulations
#' @param no.plots Number of plots to place
#' @param plot.length Length of one side of desired plot
#' @param randomizations The number of randomized CDMs, per null, to generate. These are
#' used to compare the significance of the observed metric scores.
#' @param cores The number of cores to be used for parallel processing.
#' @param iterations The number of complete tests to be run. For instance, 1 iteration
#' would be considered a complete cycle of running all spatial simulations, randomly
#' placing plots in the arenas, sampling the contents, creating a community data
#' matrix, calculating observed metric scores, then comparing these to the specified
#' number of randomizations of the original CDMs. 
#' @param prefix Optional character vector to affix to the output RData file names, e.g.
#' "test1". 
#' @param simulations Optional. If not provided, defines the simulations as all of those
#' in defineSimulations. If only a subset of those simulations is desired, then
#' simulations should take the form of a character vector corresponding to named functions
#' from defineSimulations. The available simulations can be determined by running
#' names(defineSimulations()). Otherwise, if the user would like to define a new
#' simulation on the fly, the argument simulations can take the form of a named list of
#' new functions (simulations).
#' @param nulls Optional. If not provided, defines the nulls as all of those in
#' defineNulls. If only a subset of those is desired, then nulls should take
#' the form of a character vector corresponding to named functions from defineNulls.
#' The available nulls can be determined by running names(defineNulls()). Otherwise,
#' if the user would like to define a new null on the fly, the argument nulls can take
#' the form of a named list of new functions (nulls). 
#' @param metrics Optional. If not provided, defines the metrics as all of those in
#' defineBetaMetrics. If only a subset of those is desired, then metrics should take
#' the form of a character vector corresponding to named functions from defineBetaMetrics.
#' The available metrics can be determined by running names(defineBetaMetrics()).
#' If the user would like to define a new metric on the fly, the argument can take
#' the form of a named list of new functions (metrics).
#' 
#' @details This function wraps a number of other wrapper functions into
#' one big beta metric + null performance tester function. Unlike the basic betaLinker
#' function, multiple tests can be run, with results saved as RDS files.
#'
#' @return Multiple iterations of the the betaLinker function.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' #not run
#' #system.time(betaMultiLinker(no.taxa=50, arena.length=300, mean.log.individuals=3.2, 
#' 	#length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.3, 
#'	#competition.iterations=2, no.plots=20, plot.length=30,
#'	#randomizations=3, cores="seq", iterations=2, prefix="test",
#'	#nulls=c("richness", "frequency")))

betaMultiLinker <- function(no.taxa, arena.length, mean.log.individuals, length.parameter, 
	sd.parameter, max.distance, proportion.killed, competition.iterations, no.plots, 
	plot.length, randomizations, cores, iterations, prefix,
	simulations, nulls, metrics)
{
	#create a simple file name
	for(i in 1:iterations)
	{
		if(is.null(prefix))
		{
			filename <- paste("iteration", i, ".RDS", sep="")
		}
		else
		{
			filename <- paste(prefix, "_", "iteration", i, ".RDS", sep="")
		}
		temp <- betaLinker(no.taxa, arena.length, mean.log.individuals, length.parameter, 
			sd.parameter, max.distance, proportion.killed, competition.iterations, 
			no.plots, plot.length, randomizations, cores,
			simulations, nulls, metrics)
		saveRDS(temp, file=filename)
	}
	return("Files saved to working directory")
}
