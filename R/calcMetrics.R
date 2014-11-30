#' Calculate phylogenetic community structure metrics
#'
#' Given a prepped metrics.input object, calculate all phylogenetic community structure
#' metrics of interest.
#'
#' @param metrics.input metrics.input object
#' @param metrics Untested. It should be possible to provided a list of named metrics
#' here, rather than needing to define them in defineNulls. 
#' 
#' @details Currently we are calculating 19 phylogenetic community structure metrics.
#' This function first confirms that the input is of class metrics.input and, if so, then
#' confirms that the metrics to be calculated are in a named list (via checkMetrics),
#' then lapplies all metric functions to the input metrics.input object.
#'
#' @return A data frame with the calculated metrics and the associated species richness
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
#' prepped <- prepData(tree, cdm)
#'
#' results <- calcMetrics(prepped)

calcMetrics <- function(metrics.input, metrics=NULL)
{
	if(!inherits(metrics.input, "metrics.input"))
	{
		stop("Input needs to be of class 'metrics.input'")
	}
		
	metrics <- checkMetrics(metrics)
		
	tempResults <- lapply(metrics, function(x) x(metrics.input))
	
	#convert the list to a data frame
	results <- as.data.frame(tempResults)
	
	#add quadrat names to data frame
	results <- data.frame(quadrat=row.names(results), results)
	
	#get rid of row names
	row.names(results) <- NULL
	
	results
}
