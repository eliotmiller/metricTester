#' Generate a simulated community data matrix
#'
#' Given a phylo object, desired min and max species richnesses, and a vector of potential
#' species abundances, will generate a community data matrix with these characteristics.
#'
#' @param tree Phylo object
#' @param min.rich Minimum richness of the resulting cdm
#' @param max.rich Maximum richness of the resulting cdm
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
#' library(geiger)
#' library(picante)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))
#'
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)

simulateComm <- function(tree, min.rich, max.rich, abundances)
{
	col.1 <- c()
	col.2 <- c()
	col.3 <- c()
	for (i in seq(from=min.rich, to=max.rich))
	{
		rich <- rep(i, i) ##this generates a vector of community names by repeating whatver the value of richness is (i) i times
		col.1 <- append(col.1, rich)
		numbers <- sample(abundances, i)
		col.2 <- append(col.2, numbers)
		species <- sample(tree$tip.label, i)
		col.3 <- append(col.3, species)
	}
		cdm.fake <- data.frame(col.1, col.2, col.3)
		cdm <- sample2matrix(cdm.fake)

		#sort cdm into same order as phylogeny. seems to be necessary for psc.corr and perhaps other functions
		cdm <- cdm[tree$tip.label]

		return(cdm)
}
