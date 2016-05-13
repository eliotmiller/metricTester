#' Calculate mean root distance
#'
#' Given a picante-style community data matrix (sites are rows, species are columns), 
#' and a phylogeny, calculate the mean root distance of the set of taxa in each site.
#'
#' @param samp A picante-style community data matrix with sites as rows, and
#' species as columns.
#' @param tree An ape-style phylogeny.
#' @param abundance.weighted Whether to weight the calculation by the abundance of a given
#' species in a given plot.
#' 
#' @details Mean root distance (MRD) as originally formulated by Kerr & Currie (1999)
#' defined MRD as the mean number of nodes between a set
#' of taxa and the root. It is not clear to me whether the number of nodes includes
#' the tip itself. In other words, should a species connected directly to the
#' root of the tree be considered to be separated by zero or one nodes from the root? I
#' have chosen to define it as one, but am open to changing it. This definition emphasizes
#' that "one" speciation event has occurred along that branch since the origin of the
#' clade (all caveats about extinction and phylogenetic sampling aside). The
#' abundance-weighted form of this calculation takes a row-wise (plot-wise) weighted-mean
#' where the values are a species' node-distance to the root, and the
#' weights are a species' abundance in the input community data matrix. I ran a quick test
#' to see whether abundance-weighted MRD might be equal to IAC of Cadotte et al. (2010),
#' and it does not seem to be. Therefore its utility is unknown (as is that of
#' non-abundance weighted MRD).
#'
#' @return A vector of MRD values.
#'
#' @export
#'
#' @importFrom ape dist.nodes
#' @importFrom picante matrix2sample sample2matrix
#'
#' @references Kerr, J. T. and D. J. Currie. 1999. The relative importance of evolutionary
#' and environmental controls on broad-scale patterns of species richness in North
#' America. Ecoscience 6:329-337.
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' results <- MRD(cdm, tree, abundance.weighted=FALSE)

MRD <- function(samp, tree, abundance.weighted)
{
	#set all branches equal to length of 1, then compute distances between all nodes.
	#because branches are set to 1, this is just the number of nodes between each node
	tree <- ape::compute.brlen(tree, 1)
	allDists <- ape::dist.nodes(tree)

	#subset allDists to those between the root and the tips
	rootDists <- allDists[length(tree$tip.label)+1, 1:length(tree$tip.label)]

	#bind these root distances into a dataframe with species labels
	tipsToRoot <- data.frame(id=tree$tip.label, nodes=rootDists)

	#convert the cdm into a phylocom-style cdm, merge in the root distances
	phylocomCDM <- picante::matrix2sample(samp)	
	temp <- merge(phylocomCDM, tipsToRoot) 

	#rearrange the phylocom cdm into a format to coerce back into picante cdm. NOTE THAT 
	#WE TRICK IT HERE AND SET ABUND EQUAL TO NODE DISTANCES
	temp <- data.frame(plot=temp$plot, abund=temp$nodes, id=temp$id)
	cdm <- picante::sample2matrix(temp)

	if(abundance.weighted==FALSE)
	{
		#set 0s to NA, then take the row-wise mean, excluding NAs
		cdm[cdm==0] <- NA
		results <- rowMeans(cdm, na.rm=TRUE)
	}
	
	if(abundance.weighted==TRUE)
	{
		#had trouble getting an apply function to work combining these two frames within
		#the function, so combine them first
		combined <- cbind(cdm, samp)
		
		#now take a weighted mean where the first half of each row of each data frame is
		#the root distance matrix, and the second half are the weights, and run this kind
		#of inelegant apply statement over it to take weighted means
		results <- apply(combined, 1, function(y)
			weighted.mean(x=y[1:(dim(combined)[2]/2)],
			w=y[((dim(combined)[2]/2)+1):dim(combined)[2]]))
	}
	results
}
