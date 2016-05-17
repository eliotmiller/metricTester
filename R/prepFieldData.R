#' Prep data to to calculate phylogenetic fields
#'
#' Given a phylo object, and a picante-style community data matrix (sites are rows,
#' species are columns), prepare data for calculation of phylogenetic or trait fields.
#'
#' @param tree Phylo object. 
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param dists A symmetric distance matrix can be directly supplied. By doing
#' this, the user is implying that trait fields should be calculated.
#' 
#' @details Returns a named list with four elements: the original phylogenetic tree
#' phylogenetic distances among species, the original picante-style CDM, and an argument,
#' specifying whether the phylogenetic or trait field should be calculated. This is
#' determined by the inputs. If a tree is supplied, the phylogenetic fields will be
#' calculated. If a distance matrix is supplied, the trait fields will be calculated.
#' Importantly, note that some metrics require a tree for calculation. These metrics
#' include PSV, PSC, PD, and QE. If a trait distance matrix is supplied, and these metrics
#' are called, the distances will be automatically coerced into a dendrogram via the
#' hclust function and conversion to an ape phylogeny.
#'
#' @return An object of class field.input
#'
#' @export
#'
#' @importFrom ape as.phylo
#' @importFrom stats as.dist cophenetic hclust
#'
#' @references Miller, Wagner, Harmon & Ricklefs. In review. Radiating despite a lack of
#' character: closely related, morphologically similar, co-occurring honeyeaters have
#' diverged ecologically.
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' prepped <- prepFieldData(tree=tree, picante.cdm=cdm)

prepFieldData <- function(tree, dists, picante.cdm)
{
	#if missing the tree and not missing distances, convert the distances into a
	#dendrogram and warn about what happened
	if(missing(tree) & !missing(dists))
	{
		temp <- hclust(as.dist(dists))
		tree <- as.phylo(temp)
		field <- "trait"
		warning("Converted distance matrix to a tree for some metrics. See details.")
	}

	#if provided with a tree and not with dists, calulate the distances
	else if(!missing(tree) & missing(dists))
	{
		dists <- cophenetic(tree)
		field <- "phylo"
	}

	#if a user ever provides both a tree and a distance matrix, throw an error
	else
	{
		stop("You must provide either a phylogenetic tree or distance matrix, not both.")
	}
	
	#you have a check in plotContents to exclude plots w < 2 spp, but after
	#randomizations it is possible to end up with plots that include < 2 spp. exclude
	#these. note that dplyr does not need even sample sizes or anything like that, so
	#this should hopefully work
	picante.cdm <- picante.cdm[apply(picante.cdm, 1, lengthNonZeros) >= 2,]
	dat   <- list("tree"=tree, "dists"=dists, "picante.cdm"=picante.cdm, "field"=field)
	class(dat) <- c("list", "field.input")
	dat
}
