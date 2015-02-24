#' Prep data to test phylogenetic community structure metrics
#'
#' Given a phylo object, and a picante-style community data matrix (sites are rows,
#' species are columns), prepare data for analysis.
#'
#' @param tree Phylo object. 
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param optional.dists A symmetric distance matrix can be directly supplied. This option is
#' experimental. Performance depends on the metric being used. If the metric in question
#' relies on the dists element of the result of this function, then this optional distance
#' matrix will be inserted. But other metrics that rely on the ecoPD.cdm object will still
#' employ the tree data.
#' 
#' @details Returns a named list with four elements: the original phylogenetic tree
#' phylogenetic distances among species, the original picante-style CDM, and
#' an ecoPD-style CDM. This is used internally by metricTester to handle varying inputs 
#' for different metrics in different packages.
#'
#' @return An object of class metrics.input
#'
#' @export
#'
#' @import phylobase grid ecoPDcorr
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
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)
#'
#' prepped <- prepData(tree, cdm)

prepData <- function(tree, picante.cdm, optional.dists)
{
	if(!missing(optional.dists))
	{
		#this is a very rudimentary check to see if the optional distance matrix contains
		#all spp that are in the CDM. note that it can contain species that are not in the
		#CDM, but not vice versa
		if(length(setdiff(colnames(picante.cdm), row.names(optional.dists))) > 0)
		{
			stop("Some species in your CDM are not in your distance matrix")
		}
		dists <- optional.dists
	}
	else
	{
		dists <- cophenetic(tree)
	}
		
	#you have a check in quadratContents to exclude quadrats w < 2 spp, but after
	#randomizations it is possible to end up with quadrats that include < 2 spp. exclude
	#these. note that dplyr does not need even sample sizes or anything like that, so
	#this should hopefully work
	picante.cdm <- picante.cdm[apply(picante.cdm, 1, lengthNonZeros) >= 2,]
	ecoPD.cdm <- suppressWarnings(phylo4com(tree, t(picante.cdm)))
	dat   <- list("tree"=tree, "dists"=dists, "picante.cdm"=picante.cdm, "ecoPD.cdm"=ecoPD.cdm)
	class(dat) <- c("list", "metrics.input")
	dat
}
