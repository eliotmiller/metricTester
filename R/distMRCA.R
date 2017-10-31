#' Calculate plot-level distances to most recent common ancestors
#'
#' Given a picante-style community data matrix (sites are rows, species are columns), 
#' and a phylogeny, calculate the distances of sets of taxa to their MRCA.
#'
#' @param samp A picante-style community data matrix with sites as rows, and
#' species as columns.
#' @param tree An ape-style phylogeny.
#' @param pairwise Whether to use the MRCA of all taxa in the sample, or the MRCA of each
#' pairwise comparison in the sample. See details.
#' 
#' @details Experimental metrics! This function calculates two simple but potentially
#' useful measures. The first, accessed by setting pairwise to FALSE,
#' is the mean branch length between a set of taxa and their most recent common ancestor
#' (MRCA). I have not seen this used in the literature before, but it seems likely I'm
#' wrong. This metric was not tested in our recent Ecography review, but given
#' certain data structures, it seems potentially useful. In other cases, the MRCA will
#' often simply be the root of the tree, and the metric will perhaps be of less use. 
#' Large values of the version of distMRCA correspond to taxa with a
#' distant MRCA, while small values correspond to taxa with a more recent MRCA.
#' Given an ultrametric tree, the mean distance
#' between a set of taxa and a single ancestor is of course equal to the distance between
#' one of those taxa and the ancestor. However, in case an ultrametric tree is passed
#' to the function, I do define it as the mean distance between all present taxa
#' and their MRCA. It will throw a warning if a non-ultrametric tree is passed along. 
#'
#' The second measure calculated by this function is accessed by setting pairwise to TRUE.
#' Here, per plot, the metric finds the distance of the MRCA of each pairwise taxon
#' comparison from the root. The value returned per plot is then the mean of these
#' distances. DANGER. Because this second option calculates all
#' pairwise comparisons, the time it takes to run grows exponentially with the size of the
#' community data matrix. For instance, on my personal computer,
#' pairwise distMRCA was calculated in 0.2 seconds
#' for a CDM with 16 plots containing between 10 and 25 species each. However, for a CDM
#' with 100 plots containing between 25 and 55 species, it took 42s.
#' In contrast to the first flavor of this metric, large values of this metric
#' correspond to plots where the taxa present are more recently derived, while small
#' values correspond to plots where the taxa are less recently derived (average common
#' ancestor closer to the root). To make these measures more comparable, it may be better
#' subtract the final values from the total tree height (with caveat about ultrametric
#' tree above). It would also be easy to derive an abundance weighted version of this
#' function. UPDATE. It appears that this second form is yet another (slower) way of
#' deriving the calculation of MPD/PSV.
#'
#' @return A vector of distMRCA values.
#'
#' @export
#'
#' @importFrom ape getMRCA
#'
#' @references Miller, E. T. 2016. Random thoughts.
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' results <- distMRCA(cdm, tree, pairwise=FALSE)

distMRCA <- function(samp, tree, pairwise)
{
	#warn if tree is not ultrametric
	if(!is.ultrametric(tree))
	{
		warning("Tree is not ultrametric. Will affect metric values.")
	}

	#find the distances between all nodes
	allDists <- dist.nodes(tree)
	
	#coerce samp to a matrix. this is useful because when you subset each row to those
	#species that are actually present, it keeps species' names associated with numbers
	samp <- as.matrix(samp)
	
	#if there are issues with speed of this function, create some simple functions here
	#and instead apply them over the matrix. in the mean time, run this for loop and save
	#results into this vector
	results <- c()

	#if pairwise is false, just get the MRCA of all the taxa in the sample
	if(pairwise==FALSE)
	{
		for(i in 1:dim(samp)[1])
		{
			#each plot, subset to those taxa present, then find the MRCA
			taxa <- samp[i,][samp[i,]!=0]
			MRCA <- getMRCA(tree, names(taxa))
		
			#subset allDists to those between the tips and the MRCA
			mrcaDists <- allDists[MRCA, 1:length(tree$tip.label)]
		
			#find the mean and save the results
			results[i] <- mean(mrcaDists)
		}
	}
	
	#if pairwise is TRUE, get the MRCA of all taxon pairs in the sample, then take the
	#mean of that
	else if(pairwise==TRUE)
	{
		#outer i-level of loop runs over plots
		for(i in 1:dim(samp)[1])
		{
			#each plot, subset to those taxa present, then find the MRCA
			taxa <- samp[i,][samp[i,]!=0]

			#quickly get all pairwise comparisons with this neat trick
			allPairs <- outer(names(taxa), names(taxa),
				function(x, y) paste(x, y, sep=","))
			allPairs <- allPairs[upper.tri(allPairs)]

			#this is imperfect (unnecessary extra step), but stringsplit then lapply
			#mrca over it. unlist and run a for loop over it
			temp <- strsplit(allPairs, ",")
			MRCAs <- unlist(lapply(temp, function(x) getMRCA(tree, x)))
			
			#set up a vector to save sample mean MRCA into
			mrcaDists <- c()
			
			#now, per plot, this finds the distance of each MRCA from the root
			for(j in 1:length(MRCAs))
			{		
				#subset allDists to find the relevant distance (this is calculating the
				#distance between that node and the ROOT)
				mrcaDists[j] <- allDists[length(tree$tip.label)+1, MRCAs[j]]
			}
		
			#find the mean of all the pairwise MRCA dists in the sample
			results[i] <- mean(mrcaDists)
		}
	}
	else
	{
		stop("pairwise must be either TRUE or FALSE")
	}

	results
}
