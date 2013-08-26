#' Calculate phylogenetic community structure metrics
#'
#' Given a phylo object, and a picante-style community data matrix (sites are rows,
#' species are columns), calculate all phylogenetic community structure metrics of
#' interest.
#'
#' @param tree Phylo object
#' @param picante_cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' 
#' @details Currently we are calculating 19 phylogenetic community structure metrics
#'
#' @return A data frame with the calculated 19 metrics and the associated species richness
#' of all input "communities".
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
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))
#'
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)
#'
#' results <- allMetrics(tree, cdm)

##phylo4com takes an x = phylo, n = abundance matrix with species as rows and sites as columns

##in a picante cdm, sites are rows, species are columns

allMetrics <- function(tree, picante_cdm)
{
	spacodiR_cdm <- t(picante_cdm) ##species are rows, sites are columns
	
	ecoPD_cdm <- suppressWarnings(phylo4com(tree, spacodiR_cdm)) ##this is by far the slowest thing happening in this function so far! who knows why?! we need to fix that probably

	dists <- cophenetic(tree)

	quadratNames <- dimnames(cdm)[[1]]
	
	richness <- apply(picante_cdm, 1, lengthNonZeros)

	NAW_MPD <- modified.mpd(picante_cdm, dists, abundance.weighted=FALSE)
	
	inter_MPD <- modified.mpd(picante_cdm, dists, abundance.weighted="interspecific")

	intra_MPD <- modified.mpd(picante_cdm, dists, abundance.weighted="intraspecific")
	
	complete_MPD <- modified.mpd(picante_cdm, dists, abundance.weighted="complete")

	NAW_MNTD <- mntd(picante_cdm, dists, abundance.weighted=FALSE)

	AW_MNTD <- mntd(picante_cdm, dists, abundance.weighted=TRUE)

	PSV <- psv(picante_cdm, tree)
	
	PSV <- PSV$PSVs
	
	PSC <- psc.corr(picante_cdm, tree) 
	
	PSC <- PSC$PSCs
	
	PSE <- pse(picante_cdm, tree)
	
	PSE <- PSE$PSEs
	
	##all of Cadotte's metrics run horribly slow
	
	PAE <- pae(ecoPD_cdm)
	
	IAC <- iac(ecoPD_cdm) ##this runs very slow!

	Haed <- haed(ecoPD_cdm)

	Eaed <- eaed(ecoPD_cdm)

	Eed <- eed(ecoPD_cdm)

	Hed <- hed(ecoPD_cdm)
	
	SimpsonsPhy <- simpson(ecoPD_cdm, method="phylogenetic")
	
	PD <- pd(picante_cdm, tree, include.root=TRUE)
	
	PD <- PD$PD
	
	PD_Cadotte <- pd(picante_cdm, tree, include.root=FALSE)
	
	PD_Cadotte <- PD_Cadotte$PD
	
	QE <- raoD(picante_cdm, tree)
	
	QE <- QE$Dkk
	
	results <- data.frame(quadratNames, richness, NAW_MPD, inter_MPD, intra_MPD, complete_MPD, NAW_MNTD, AW_MNTD, PSV, PSC, PSE, PAE, IAC, Haed, Eaed, Eed, Hed, SimpsonsPhy, PD, PD_Cadotte, QE)
	
	row.names(results) <- row.names(picante_cdm) ##there is a very important caveat here in that you have to make sure EVERY AND ALL function returns the communities in the order they were input in
	
	return(results)	
}
