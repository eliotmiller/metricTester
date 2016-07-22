#' Generate a simulated community data matrix
#'
#' Given a phylo object, a vector of desired species richnesses, and a vector of potential
#' species abundances, will generate a community data matrix with these characteristics.
#'
#' @param tree Phylo object
#' @param richness.vector Vector of desired species richness, one for each desired plot
#' @param abundances A vector of potential abundances, e.g. a log-normal distribution
#' 
#' @details There is currently no implementation to control the frequency with which a
#' given species is selected. As of metricTester 1.2.2, this function no longer can
#' occasionally return a CDM missing species that are in the input phylogeny.
#'
#' @return A community data matrix (as a data frame) with species as columns and sites as
#' rows.
#'
#' @export
#'
#' @importFrom ape drop.tip
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)

simulateComm <- function(tree, richness.vector, abundances)
{
	#set up a quick check to confirm that you are not asking the function to sample more
	#species than are in the tree
	if(max(richness.vector) > length(tree$tip.label))
	{
		stop("Cannot create a CDM with more species than are in the input phylogeny")
	}

	#check that there are no values less than 1 in the abundances vector
	if(min(abundances <= 0))
	{
		stop("simulateComm cannot handle abundances of less than or equal to 0")
	}

	#set up a matrix of zeros of the correct dimensions for the output cdm
	cdm <- matrix(nrow=length(richness.vector), ncol=length(tree$tip.label), 0)
	
	#give the cdm row and column names
	colnames(cdm) <- tree$tip.label
	row.names(cdm) <- paste("plot",1:dim(cdm)[1], sep="")
	
	#go into a for loop where each iteration you sample the number of species as are in
	#that element of the richness vector, then assign abundances from that vector
	for(i in 1:length(richness.vector))
	{
		#first select the species
		species <- sample(colnames(cdm), richness.vector[i])
		
		#then correctly subset the cdm to just the elements that correspond to those spp,
		#and replace the 0s with values from abundances. 
		cdm[i,][names(cdm[i,]) %in% species] <- sample(abundances, richness.vector[i])
	}

	return(as.data.frame(cdm))
}
