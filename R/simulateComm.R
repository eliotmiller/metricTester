#' Generate a simulated community data matrix
#'
#' Given a phylo object, a vector of desired species richnesses, and a vector of potential
#' species abundances, will generate a community data matrix with these characteristics.
#'
#' @param tree Phylo object
#' @param richness.vector Vector of desired species richness, one for each desired quadrat
#' @param abundances A vector of potential abundances, e.g. a log-normal distribution
#' 
#' @details There is currently no implementation to control the frequency with which a
#' given species is selected.
#'
#' @return A community data matrix (a data frame) with species as columns and sites as
#' rows.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)

simulateComm <- function(tree, richness.vector, abundances)
{
	col.1 <- c()
	col.2 <- c()
	col.3 <- c()
	for (i in 1:length(richness.vector))
	{
		#this generates a vector of community names by repeating i times whatever the
		#value of richness.vector[i] is
		temp.name <- rep(i, richness.vector[i]) 
		col.1 <- append(col.1, temp.name)
		numbers <- sample(abundances, richness.vector[i])
		#this assigns abundances to species
		col.2 <- append(col.2, numbers)
		#this assigns species names to those abundances
		species <- sample(tree$tip.label, richness.vector[i])
		col.3 <- append(col.3, species)
	}
		cdm.fake <- data.frame(col.1, col.2, col.3)
		cdm <- sample2matrix(cdm.fake)

		#sort cdm into same order as phylogeny. seems to be necessary for pscCorr and 
		#perhaps other functions
		#need to fake prune the phylo here in case not all the species are in the cdm as 
		#are in the phylo

		dropped <- setdiff(tree$tip.label, colnames(cdm))
		
		drop.tree <- drop.tip(tree, dropped)

		cdm <- cdm[drop.tree$tip.label]

		quadrat <- paste("quadrat",1:dim(cdm)[1], sep="")

		dimnames(cdm)[[1]] <- quadrat

		return(cdm)
}
