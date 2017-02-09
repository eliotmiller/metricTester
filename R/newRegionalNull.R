#' Randomize community data matrix with second-gen regional null model
#'
#' Null model that simulates community assembly where species probabilities of occurrence
#' are proportional to their regional abundance.
#'
#' @param picante.cdm Picante-style community data matrix with
#' communities/plots/plots/etc as rows and species as columns
#' @param regional.abundance Vector of species names, where each species' name is repeated
#' the number of times necessary to accomodate its abundance in the regional species pool.
#' Note that this can be created, if needed, from the observed picante.cdm with the
#' abundanceVector() function.
#' 
#' @details This model is slightly different than the regional null model introduced in
#' our Ecography paper. It samples from the same regional abundance vector, but fills
#' individuals into a blank CDM until the richness of that plot in the input CDM is
#' reached. Thus, the results are equally valid whether concatenated by richness or by
#' plot. Unlike the original regional null model, however, the same number of 
#' individuals per plot is not maintained. Also, because the model uses a while loop, it
#' is conceivable that some parameters could result in long runs, e.g., while the requisite
#' number of species are sampled from a highly skewed abundance distribution. Preliminary
#' tests did not show this to be an issue, but worth paying attention to if plots contain
#' large numbers of species. There is a check for the most extreme situation: when the
#' abundance vector contains fewer unique species than are in the plot with the maximum
#' number of species.
#'
#' @return A matrix of the same size and names as the input community data matrix
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
#' positions <- competitionArena(prepped)
#'
#' boundResults <- plotPlacer(no.plots=15, arena.length=300, plot.length=30)
#'
#' #return a CDM in picante format
#' cdmTemp <- plotContents(positions$arena, boundResults)
#'
#' test <- newRegionalNull(cdmTemp$picante.cdm,
#'	regional.abundance=abundanceVector(cdmTemp$picante.cdm))

newRegionalNull <- function(picante.cdm, regional.abundance)
{
	#calculate the species richness of the input cdm
	richness <- apply(picante.cdm, 1, lengthNonZeros)

	#create a check to make sure there are at least as many species in the abundance
	#vector as the maximum richness plot
	if(max(richness) > length(unique(regional.abundance)))
	{
		stop("You have more species in your CDM than your abundance vector")
	}

	#set up a new, blank cdm with same dimensions and names as input
	newCDM <- picante.cdm
	newCDM[] <- 0

	#start for loop where i elements relate to rows (sites) of cdm
	for(i in 1:dim(newCDM)[1])
	{
		#number of species as were in that given row in the input cdm is sampled
		while(lengthNonZeros(newCDM[i,]) < richness[i])
		{
			#sample an individual from the regional abundance vector and add it to the
			#cdm in the correct spot
			newIndiv <- sample(regional.abundance, 1)
			newCDM[i,newIndiv] <- newCDM[i,newIndiv]+1
		}
	}

	#class as a matrix and return the new cdm
	newCDM <- as.matrix(newCDM)
	newCDM
}
