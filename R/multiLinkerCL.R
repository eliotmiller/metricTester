#' Cluster version of multiLinker
#'
#' This function runs multiple iterations of the linker function, saving results to file.
#'
#' @param tree Phylo object
#' @param arena.length A numeric, specifying the length of a single side of the arena
#' @param mean.log.individuals Mean log of abundance vector from which species abundances
#' will be drawn
#' @param length.parameter Length of vector from which species' locations are drawn. Large
#' values of this parameter dramatically decrease the speed of the function but result in
#' nicer looking communities
#' @param sd.parameter Standard deviation of vector from which species' locations are 
#' drawn
#' @param max.distance The geographic distance within which neighboring
#' indivduals should be considered to influence the individual in question
#' @param proportion.killed The percent of individuals in the total arena that should be
#' considered (as a proportion, e.g. 0.5 = half)
#' @param competition.iterations Number of generations over which to run competition 
#' simulations
#' @param no.quadrats Number of quadrats to place
#' @param quadrat.length Length of one side of desired quadrat
#' @param concat.by Whether to concatenate the randomizations by richness or quadrat
#' @param randomizations The number of randomized CDMs, per null, to generate. These are
#' used to compare the significance of the observed metric scores.
#' @param cores The number of cores to be used for parallel processing.
#' @param iterations The number of complete tests to be run. For instance, 1 iteration
#' would be considered a complete cycle of running all spatial simulations, randomly
#' placing quadrats in the arenas, sampling the contents, creating a community data
#' matrix, calculating observed metric scores, then comparing these to the specified
#' number of randomizations of the original CDMs. 
#' @param prefix Optional character vector to affix to the output RData file names, e.g.
#' "test1". 
#' 
#' @details This function wraps a number of other wrapper functions into
#' one big metric + null performance tester function. Only a single test is performed, 
#' with results saved into memory. To perform multiple complete tests, use the
#' multiLinker function, which saves results to file.
#'
#' @return A list of lists of data frames. The first level of the output has one element 
#' for each simulation. The second level has one element for each null model. Each of
#' these elements is a list of two data frames, one that summarizes the quadrat-level
#' significance and another and arena-level significance.
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
#' system.time(multiLinker(tree=tree, arena.length=300, mean.log.individuals=3.2, 
#' 	length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.3, 
#'	competition.iterations=2, no.quadrats=20, quadrat.length=30, concat.by="richness", 
#'	randomizations=3, cores=8, iterations=3, prefix="test"))

multiLinkerCL <- function(tree, arena.length, mean.log.individuals, length.parameter, 
	sd.parameter, max.distance, proportion.killed, competition.iterations, no.quadrats, 
	quadrat.length, concat.by, randomizations, cores, iterations, prefix)
{
	#create a simple file name, specify whether concatenating by quadrat or richness
	for(i in 1:iterations)
	{
		if(is.null(prefix))
		{
			filename <- paste("iteration", i, "by_", concat.by, ".RDS", sep="")
		}
		else
		{
			filename <- paste(prefix, "_", "iteration", i, "by_", concat.by, ".RDS", sep="")
		}
		temp <- linkerCL(tree, arena.length, mean.log.individuals, length.parameter, 
			sd.parameter, max.distance, proportion.killed, competition.iterations, 
			no.quadrats, quadrat.length, concat.by, randomizations, cores)
		saveRDS(temp, file=filename)
	}
	return("Files saved to working directory")
}
