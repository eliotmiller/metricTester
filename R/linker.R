#' Run spatial simulations, null and metric calculations to test metric + null performance
#'
#' This function wraps a number of wrapper functions into one big metric + null 
#' tester function. Only a single test is performed, with results saved into memory.
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
#' system.time(test <- linker(tree=tree, arena.length=300, mean.log.individuals=2, 
#' 	length.parameter=5000, sd.parameter=50, max.distance=30, proportion.killed=0.2, 
#'	competition.iterations=3, no.quadrats=15, quadrat.length=30, concat.by="richness", 
#'	randomizations=3, cores=3))

linker <- function(tree, arena.length, mean.log.individuals, length.parameter, 
	sd.parameter, max.distance, proportion.killed, competition.iterations, no.quadrats, 
	quadrat.length, concat.by, randomizations, cores, cluster=FALSE)
{
	#prep the data for spatial simulations
	prepped <- prepSimulations(tree, arena.length, mean.log.individuals, length.parameter, 
		sd.parameter, max.distance, proportion.killed, competition.iterations)
	#run the spatial simulations
	arenas <- runSimulations(prepped)
	#derive CDMs. quadrats are placed in the same places across all spatial simulations
	cdms <- multiCDM(arenas, no.quadrats, quadrat.length)
	#calculate observed metrics for all three spatial simulations
	observed <- lapply(cdms, function(x) observedMetrics(tree=tree, picante.cdm=x))
	#randomize all observed CDMs the desired number of times. this will generate a list of
	#lists of data frames. first level of list is for each spatial simulation (e.g. 3 if
	#there is random, habitat filtering and competitive exclusion). second level is for
	#randomizations, one for each. third level is data frames, one per null model
	allRandomizations <- lapply(1:length(cdms), function(x) metricsNnulls(tree=tree, 
		picante.cdm=cdms[[x]], regional.abundance=arenas[[x]]$regional.abundance,
		cores=cores, cluster, randomizations=randomizations))
	#reduce the randomizations to a list of lists of (first order of lists is for each
	#spatial simulation) data frames
	reduced <- lapply(allRandomizations, reduceRandomizations)
	#now lapply the errorChecker over each spatial simulation
	#the output of this is similar to above. list of lists of
	#data frames. first level for each simulation. second level for each null model.
	#the two data frames per second level summarizing the quadrat and arena-level
	#significance results
	results <- lapply(1:length(reduced), function(x) 
		errorChecker(observed=observed[[x]], reduced.randomizations=reduced[[x]],
		concat.by=concat.by))
	names(results) <- names(arenas)
	results
}
