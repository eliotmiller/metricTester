#' Generate regional abundance vector
#'
#' Given a community data matrix of sites by species, extract the column-wise sums (the
#' total number of individuals of each species) and expand to create a regional abundance
#' vector.
#'
#' @param cdm Community data matrix in picante format
#' 
#' @details Simple function to create a regional abundance vector given a "regional"
#' community data matrix.
#'
#' @return A character vector in the form "s1, s1, s1, s2, s2, s3, etc".
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
#'
#' abund <- abundanceVector(cdm)

abundanceVector <- function(cdm)
{
	temp <- apply(cdm, 2, sum)
	abundances <- rep(names(temp), temp)
	return(abundances)
}
