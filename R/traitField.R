#' Calculate a species' trait field
#'
#' Calculate the similarity in trait space of a species to those it occurs with.
#'
#' @param trait.distance Symmetrical matrix summarizing pairwise trait distances
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param metric Phylogenetic metric of choice (see details)
#' 
#' @details The trait distance matrix should be symmetrical and "complete". See example.
#' Currently this is only programmed to use either non-abundance-weighted mean
#' pairwise or interspecific abundance-weighted mean pairwise phylogenetic distance.
#' The function could be improved by tapping into any of the phylogenetic metrics defined
#' in defineMetrics.
#'
#' @return Named vector of species' trait fields. 
#'
#' @export
#'
#' @references Miller, Wagner, Harmon & Ricklefs. In review. Radiating despite a lack of
#' character: closely related, morphologically similar, co-occurring honeyeaters have
#' diverged ecologically.
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #simulate trait evolution up the tree. Make 2-d trait space and find distances between
#' #species in that space
#' traits <- evolveTraits(tree)
#'
#' #calculate the distances betweeen species
#' dists <- as.matrix(dist(traits[[2]], diag=T, upper=T))
#'
#' #simulate log-normal abundances
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' #simulate a community data matrix with these inputs
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' #example trait field calculations
#' exampleField <- traitField(trait.distance=dists, picante.cdm=cdm, metric="naw.mpd")

traitField <- function(trait.distance, picante.cdm, metric)
{
	#calculate the metric for each cell in the cdm
	if(metric=="naw.mpd")
	{
		cellResults <- modifiedMPD(samp=picante.cdm, dis=trait.distance,
			abundance.weighted=FALSE)
	}
	else if(metric=="interspecific")
	{
		cellResults <- modifiedMPD(samp=picante.cdm, dis=trait.distance,
			abundance.weighted="interspecific")
	}
	else
	{
		stop("metric currently must be either 'naw.mpd' or 'interspecific'")
	}

	#go into a simple for loop that for each species, takes a weighted mean of the vector
	#of assemblage-specific metric values. if the species is absent, the weight will be
	#zero and it won't be included, so don't need to do any subsetting. if non-abundance-
	#weighted take a "weighted" mean where weights are either 0s or 1s
	results <- c()
	for(i in 1:dim(trait.distance)[1])
	{
		if(metric=="naw.mpd")
		{
			#derive a quick vector of presence-absence style weights so that if the metric
			#is not abundance-weighted it treats a presence as a 1, otherwise a 0.
			naWeights <- picante.cdm[,i]
			naWeights[naWeights > 0] <- 1

			results[i] <- weighted.mean(x=cellResults, w=naWeights, na.rm=TRUE)
		}
		else if(metric=="interspecific")
		{
			results[i] <- weighted.mean(x=cellResults, w=picante.cdm[,i], na.rm=TRUE)
		}
	}
	
	#give the vector names and return
	names(results) <- colnames(picante.cdm)
	results
}
