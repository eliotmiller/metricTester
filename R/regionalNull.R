#' Randomize community data matrix with regional null model
#'
#' Null model that simulates community assembly where species probabilities of occurrence
#' are proportional to their regional abundance.
#'
#' @param picante.cdm Picante-style community data matrix with
#' communities/quadrats/plots/etc as rows and species as columns
#' @param tree Ape-style phylogeny
#' @param regional.abundance Vector of species names, where each species' name is repeated
#' the number of times necessary to accomodate its abundance in the regional species pool
#' 
#' @details Although nowhere near as fast as, e.g. randomizeMatrix, this function still
#' runs fairly quickly (no for or while loops). It works by drawing the total number of
#' individuals observed in the input plot from the regional abundance vector. Thus, while
#' a randomized quadrat will not necessarily have the same number of species as the
#' observed quadrat, over many iterations it will likely be sampled. We can then
#' concatenate the results by richness at the end, which will only compare observed values
#' to random quadrats of the same richness. As an example, an observed quadrat might have
#' two individuals of speciesA and two of speciesB. If the regional abundance vector is
#' c("spA","spA","spA","spA","spB","spB","spB","spC"), and we draw four individuals, it
#' would be possible to draw 1, 2, or 3 species, but in general, two species would be seen
#' in the randomized quadrats.
#'
#' @return A matrix with all species in the input tree in phylogenetic order, and the same
#' number of randomized quadrats as used in the input community data matrix
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #prep the data for the simulation
#' prepped <- prepSimulations(tree, arena.length=300, mean.log.individuals=2, 
#' 	length.parameter=5000, sd.parameter=50, max.distance=20, proportion.killed=0.2,
#' 	competition.iterations=3)
#'
#' positions <- competitionArena(prepped)
#'
#' boundResults <- quadratPlacer(no.quadrats=15, arena.length=300, quadrat.length=30)
#'
#' #return a CDM in picante format
#' cdmTemp <- quadratContents(positions$arena, boundResults)
#'
#' test <- regionalNull(cdmTemp$cdm, tree, 
#'	regional.abundance=abundanceVector(cdmTemp$cdm))

regionalNull <- function(picante.cdm, tree, regional.abundance)
{
	#find the total number of individuals in each quadrat
	sums <- apply(picante.cdm, 1, sum)

	#this command works well, lucky guess on how to write it. it ends up sampling
	#the required number of individuals (the sum of all individuals in a quadrat) 
	#where each species gets drawn with a probability proportional to its abundance
	#in the regional abundance vector. this does not strictly maintain species richness
	#but it approximates it, and by concatenating by richness at end, we get same result
				
	indiv.list <- lapply(sums, sample, x=regional.abundance)
		
	#unlisting the list here will generate one long vector of individuals
	indiv.list <- unlist(indiv.list)

	#generate a vector of plot IDs, where each plot ID gets repeated the number of indivs
	#in that plot. end up us
	plotIDs <- rep(paste("quadrat", 1:length(sums), sep=""), sums)
		
	#create a dummy phylocom style dataframe (col1=quadratID, 2=abund, 3=spID)
	temp.df <- data.frame(plotIDs, abund=rep(1, length(indiv.list)), indiv.list)
		
	#use picante's sample to matrix function to turn this into an appropriate cdm
	#picante sums individuals that occur multiple times in a given quadrat
	new.cdm <- sample2matrix(temp.df)
	
	#sort the cdm into quadrat order
	new.cdm <- new.cdm[unique(plotIDs),]
	
	#add columns for species that weren't recorded in any quadrats
	not.found <- setdiff(tree$tip.label, names(new.cdm))
	
	if(length(not.found > 0))
	{
		to.bind <- matrix(nrow=dim(new.cdm)[[1]], ncol=length(not.found), 0)
		colnames(to.bind) <- not.found
		new.cdm <- cbind(new.cdm, to.bind)
	}
	
	else
	{
		new.cdm <- new.cdm
	}

	#sort the cdm into phylogenetic order
	new.cdm <- new.cdm[,tree$tip.label]
	
	#re-class as matrix and return
	
	new.cdm <- as.matrix(new.cdm)
	
	return(new.cdm)
}
