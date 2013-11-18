#' Randomly settle individuals in a spatial arena
#'
#' Given output from the killSome function, randomly settles individuals in the arena.
#'
#' @param killSomeOutput Output from the killSome function
#' 
#' @details This function uses the number killed element of the killSome output to
#' randomly draw from the regional abundance vector, then settles the individuals at
#' random in the arena. 
#'
#' @return A list of 4 elements: the average relatedness in the geographic neighbordhood
#' of consideration (passed directly from the killSome output, not re-calculated here), 
#' the regional abundance vector, the new spatial arena, and the dimensions of that arena.
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
#' arena <- randomArena(tree, x.min=0, x.max=300, y.min=0, y.max=300, mean.log.individuals=2)
#'
#' #remove some of the most closely related individuals
#' new.arena <- killSome(tree, arenaOutput=arena, max.distance=50, percent.killed=0.2)
#'
#' dim(arena$arena)
#' dim(arena$new.arena)
#'
#' #now settle some indiviudals
#'
#' newer.arena <- settleSome(new.arena)
#'
#' dim(new.arena$arena)
#' dim(newer.arena$arena)

settleSome <- function(killSomeOutput)
{
	#sample the same number of individuals you killed from the regional abundance vector
	individuals <- sample(killSomeOutput$regional.abundance, size=killSomeOutput$no.killed)
	
	#start a dataframe to bind X,Y coordinates into	
	to.bind <- data.frame(individuals)

	#generate random X,Y coordinates centered around the middle of the arena
	to.bind$X <- sample(killSomeOutput$dim[1]:killSomeOutput$dim[2], size=length(individuals), replace=TRUE)
	to.bind$Y <- sample(killSomeOutput$dim[3]:killSomeOutput$dim[4], size=length(individuals), replace=TRUE)
	
	output <- list(related=killSomeOutput$related, regional.abundance=killSomeOutput$regional.abundance, arena=rbind(killSomeOutput$arena, to.bind), dims=killSomeOutput$dims)
}
