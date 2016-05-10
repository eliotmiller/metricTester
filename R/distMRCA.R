#' Calculate plot-level distances to most recent common ancestors
#'
#' Given a picante-style community data matrix (sites are rows, species are columns), 
#' and a phylogeny, calculate the distances of sets of taxa to their MRCA.
#'
#' @param samp A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param tree An ape-style phylogeny.
#' 
#' @details This function calculates a simple but potentially useful measure:
#' the mean branch length between a set of taxa and their most recent common ancestor
#' (MRCA). I have not seen this defined in the literature before, but it seems likely I'm
#' wrong. Regardless, this metric was not tested in our recent Ecography review, but it
#' seems potentially useful. Large values of distMRCA correspond to taxa with a
#' distant MRCA, while small values correspond to taxa with a more recent MRCA.
#' Given an ultrametric tree, the mean distance
#' between a set of taxa and a single ancestor is of course equal to the distance between
#' one of those taxa and the ancestor. However, in case an ultrametric tree is passed
#' to the function, I do indeed define it as the mean distance between all present taxa
#' and their MRCA. It will throw a warning if a non-ultrametric tree is passed along.
#'
#' @return A vector of distMRCA values.
#'
#' @export
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
#' dists <- ape::cophenetic.phylo(tree)
#'
#' results <- modifiedMPD(cdm, dists, abundance.weighted = "interspecific")

distMRCA <- function(samp, tree)
{
	#find the distances between all nodes
	allDists <- dist.nodes(tree)
	
	#coerce samp to a matrix. this is useful because when you subset each row to those
	#species that are actually present, it keeps species' names associated with numbers
	samp <- as.matrix(samp)
	
	#if there are issues with speed of this function, create some simple functions here
	#and instead apply them over the matrix. in the mean time
	for(i in 1:dim(samp)[1])
	{
		#each plot, subset to those taxa present, then find the MRCA
		taxa <- samp[i,][samp[i,]!=0]
		MRCA <- getMRCA(tree, taxa)
		
		#subset allDists to those between the tips and the MRCA
	
	#subset allDists to those between the root and the tips. this reduces to the number of
	#nodes between the root and tips when node.based=TRUE.
	rootDists <- allDists[length(tree$tip.label)+1, 1:length(tree$tip.label)]

	#give better names to rootDists
	
	tips.to.root <- data.frame(tipnames=tree$tip.label,root.dist)
	cdm <- phylocom.cdm[,-2]
	names(cdm) <- c("grdid","tipnames")
	MRD.ini <- merge(cdm, tips.to.root, sort = FALSE)
	MRD.ini <- MRD.ini[order(MRD.ini$grdid), ]
	MRD.table <- ddply(MRD.ini, "grdid", summarise, RD=sum(root.dist), richness=length(unique(tipnames)))
	MRD.table <- cbind(MRD.table, MRD=MRD.table$RD/MRD.table$richness)
	return(MRD.table)
}
