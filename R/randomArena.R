#' Generate a random spatial arena
#'
#' Given a phylogenetic tree, the desired dimensions of the arena, and the mean log of the
#' regional abundance pool, randomly generates
#' spatial arena.
#'
#' @param tree Phylo object
#' @param x.min The x minimum of the output arena, e.g. 0
#' @param x.max The x maximum of the output arena
#' @param y.min The y minimum of the output arena, e.g. 0
#' @param y.max The y maximum of the output arena
#' @param mean.log.individuals Mean of the log-normal distribution
#' 
#' @details This function generates a log-normal regional abundance distribution and
#' assigns those abundances to random species. It then draws from this regional abundance
#' distribution to settle individuals at random in the landscape. 
#'
#' @return A list of 4 elements: the mean of the genetic distance matrix of the input
#' phylogeny, the regional abundance vector (where each element is a species name, 
#' repeated as many times as is present in pool), the spatial arena, and the dimensions of
#' that arena.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#' library(colorRamps)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #generate the random arena
#' arena <- randomArena(tree, x.min=0, x.max=300, y.min=0, y.max=300, mean.log.individuals=2)
#'
#' #calculate genetic distances
#' gen.dists <- cophenetic(tree)
#'
#' #define species' colors for plotting
#' cols <- blue2green2red(nrow(gen.dists))
#'
#' #plot the arena
#' plot(arena$arena$X, arena$arena$Y, pch=20, cex=0.5, xlim=c(0,300), ylim=c(0,300), col=cols[arena$arena$individuals])

randomArena <- function(tree, x.min, x.max, y.min, y.max, mean.log.individuals)
{
	#generate log-normal regional abundance curve, and randomly assign abundances to species
	indivs.per.species <- rlnorm(n=length(tree$tip.label), mean.log.individuals, sdlog=1)
	
	#set species with < 0 individuals to 0 abundance
	indivs.per.species[indivs.per.species < 0] <- 0

	#round abundances to no decimal places
	indivs.per.species <- round(indivs.per.species)

	#actually generate a vector individuals with species identities (the "regional pool")
	individuals <- c()

	individuals <- rep(tree$tip.label, times=indivs.per.species)

	#start a dataframe to bind X,Y coordinates into	
	arena <- data.frame(individuals)

	#generate random X,Y coordinates centered around the middle of the arena
	arena$X <- sample(x.min:x.max, size=length(individuals), replace=TRUE)
	arena$Y <- sample(y.min:y.max, size=length(individuals), replace=TRUE)
	
	#create and return the output
	
	output <- list(related=mean(cophenetic(tree)), regional.abundance=individuals, arena=arena, dims=c(x.min, x.max, y.min, y.max))
	
	return(output)
}
