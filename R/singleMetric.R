#' Calculate specific phylogenetic community structure metric
#'
#' Given a phylo object, and a picante-style community data matrix (sites are rows,
#' species are columns), calculate a phylogenetic community structure metric of
#' interest.
#'
#' @param tree Phylo object
#' @param picante_cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param metric A phylogenetic community structure metric of interest. Options are:
#' "mpd" (non-abundance weighted MPD), "interspecific" (interspecific abundance-weighted
#' MPD), "intraspecific", "complete", "mntd" (non-abundance weighted MNTD), "aw.mntd",
#' "psv", "psc", "pse", "pae", "iac", "haed", "eaed", "eed", "hed", "simpson", "pd",
#' "pd.c" (Cadotte's re-defined PD), and "qe".
#' 
#' @details Useful wrapper function to calculate a number of phylogenetic community 
#' structure metrics from different packages.
#'
#' @return A data frame with the calculated metric and the associated species richness
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
#' results <- singleMetric(tree, cdm, "mpd")

##phylo4com takes an x = phylo, n = abundance matrix with species as rows and sites as columns

##in a picante cdm, sites are rows, species are columns

singleMetric <- function(tree, picante_cdm, metric)
{
	if(metric=="pae" | metric=="iac" | metric=="haed" | metric=="eaed" | metric=="eed" | metric=="hed" | metric=="simpson")
	{
		spacodiR_cdm <- t(picante_cdm) ##species are rows, sites are columns	
		ecoPD_cdm <- suppressWarnings(phylo4com(tree, spacodiR_cdm))
	}

	else
	{
		spacodiR_cdm <- NULL
		ecoPD_cdm <- NULL
	}
	
	dists <- cophenetic(tree)

	quadratNames <- dimnames(picante_cdm)[[1]]
	
	richness <- apply(picante_cdm, 1, lengthNonZeros)

	if(metric=="mpd")
	{
		result <- modified.mpd(picante_cdm, dists, abundance.weighted=FALSE)
	}
	
	else if(metric=="interspecific")
	{
		result <- modified.mpd(picante_cdm, dists, abundance.weighted="interspecific")
	}

	else if(metric=="intraspecific")
	{
		result <- modified.mpd(picante_cdm, dists, abundance.weighted="intraspecific")
	}

	else if(metric=="complete")
	{
		result <- modified.mpd(picante_cdm, dists, abundance.weighted="complete")
	}

	else if(metric=="mntd")
	{
		result <- mntd(picante_cdm, dists, abundance.weighted=FALSE)
	}

	else if(metric=="aw.mntd")
	{
		result <- mntd(picante_cdm, dists, abundance.weighted=TRUE)
	}

	else if(metric=="psv")
	{
		result <- psv(picante_cdm, tree)
		result <- result$PSVs
	}
	
	else if(metric=="psc")
	{
		result <- psc.corr(picante_cdm, tree) 
		result <- result$PSCs
	}
	
	else if(metric=="pse")
	{
		result <- pse(picante_cdm, tree)
		result <- result$PSEs
	}
	
	##all of Cadotte's metrics run horribly slow
	
	else if(metric=="pae")
	{
		result <- pae(ecoPD_cdm)
	}
	
	else if(metric=="iac")
	{
		result <- iac(ecoPD_cdm)
	}

	else if(metric=="haed")
	{
		result <- haed(ecoPD_cdm)
	}

	else if(metric=="eaed")
	{
		result <- eaed(ecoPD_cdm)
	}

	else if(metric=="eed")
	{
		result <- eed(ecoPD_cdm)
	}

	else if(metric=="hed")
	{
		result <- hed(ecoPD_cdm)
	}
	
	else if(metric=="simpson")
	{
		result <- simpson(ecoPD_cdm, method="phylogenetic")
	}
	
	else if(metric=="pd")
	{
		result <- pd(picante_cdm, tree, include.root=TRUE)	
		result <- result$PD
	}
	
	else if(metric=="pd.c")
	{
		result <- pd(picante_cdm, tree, include.root=FALSE)
		result <- result$PD
	}

	else if(metric=="qe")
	{	
		result <- raoD(picante_cdm, tree)
		result <- result$Dkk
	}
	
	else
	{
		return("Metric name not recognized")
	}
	
	results <- data.frame(quadratNames, richness, result)
	
	row.names(results) <- row.names(picante_cdm)
	
	return(results)	
}
