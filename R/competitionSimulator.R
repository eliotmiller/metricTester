#' Simulate competitive exclusion over generations
#'
#' Given a phylogenetic tree, a spatial arena of individuals with species identities,
#' and arguments for the desired distance and percent removed, removes some of the most
#' closely related individuals in the arena, settles individuals based on abundances from
#' a regional species pool, and repeats across the desired number of generations.
#'
#' @param tree Phylo object
#' @param initialArena A spatial arena with three columns: individuals (the species ID), 
#' X (the x axis location of that individual), and Y (the y axis location). The
#' initialArena actually needs a number of other elements in order for later functions to
#' work properly, so any modifications to the code should take note of this.
#' @param max.distance The geographic distance within which geographically neighboring
#' indivduals should be considered to influence the individual in question.
#' @param percent.killed The percent of individuals in the total arena that should be
#' considered (as a proportion, e.g. 0.5 = half).
#' @param iterations Number of generations to repeat simulation for.
#' 
#' @details This function combines the killSome and settleSome functions into a loop that
#' runs for the desired number of generations.
#'
#' @return A list of 5 elements: the average relatedness in the geographic neighbordhood
#' of consideration (appended to any previous values that were fed into the function), 
#' the number of individuals killed, the original input regional
#' abundance vector, the new spatial arena, and the dimensions of that arena. On the last
#' iteration, it returns the arena BEFORE settling new individuals randomly.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #create a random arena
#' arena <- randomArena(tree, x.min=0, x.max=300, y.min=0, y.max=300, mean.log.individuals=3)
#'
#' #run the competitionSimulator for 25 generations
#' temp <- competitionSimulator(tree, arena, 30, 0.2, 25)
#'
#' #create a quick vector for plotting
#' generations <- 1:25
#'
#' #plot the average relatedness in geographic neighborhoods over generations
#' plot(temp$related[2:length(temp$related)]~generations)

competitionSimulator <- function(tree, initialArena, max.distance, percent.killed, iterations)
{
	for(i in 1:iterations)
	{
		#take the initialArena and kill off some of the individuals in genetically clustered neighborhoods
		killed.arena <- killSome(tree, initialArena, max.distance, percent.killed)

		#add individuals back in, but save this as initialArena, so that it gets plugged back
		#in next iteration instead of the original random arena
		initialArena <- settleSome(killed.arena)
	}

	return(killed.arena)
}
