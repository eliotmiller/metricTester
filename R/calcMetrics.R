#' Calculate phylogenetic community structure metrics
#'
#' Given a prepped metrics.input object, calculate all phylogenetic community structure
#' metrics of interest.
#'
#' @param metrics.input Prepped metrics.input object
#' @param metrics Optional. If not provided, defines the metrics as all of those in
#' defineMetrics. If only a subset of those metrics is desired, then metrics should take
#' the form of a character vector corresponding to named functions from defineMetrics.
#' The available metrics can be determined by running names(defineMetrics()). Otherwise,
#' if the user would like to define a new metric on the fly, the argument metrics can take
#' the form of a named list of new functions (metrics). If the
#' latter, new_ must be set to TRUE. 
#' @param new_ Whether or not new metrics are being defined on the fly. Default is FALSE.
#' Set to TRUE if a new metric is being used.
#' 
#' @details Determine which metrics will be calculated by running names(defineMetrics()).
#' If only a subset of these is desired, supply metrics with a character vector of the
#' named, available metrics. IMPORTANTLY, note that some downstream functions expect the
#' first column returned from this function to be the species richness of each plot. It is
#' best practice therefore to always pass "richness" along as the first metric, even when
#' only a subset of metrics is being calculated. It is possible to provide this function
#' with both predefined metrics and metrics that are defined on the fly, but the call is
#' rather convoluted. See examples.
#'
#' @return A data frame with the calculated metrics of all input "communities".
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' prepped <- prepData(tree, cdm)
#'
#' results <- calcMetrics(prepped)
#'
#' #an example of how to only calculate a subset of pre-defined metrics
#' results2 <- calcMetrics(prepped, metrics=c("richness","NAW_MPD"))
#'
#' #an example of how to define ones own metrics for use in the metricTester framework
#' #this "metric" simply calculates the richness of each plot in the CDM
#' exampleMetric <- function(metrics.input)
#' {
#'	output <- apply(metrics.input$picante.cdm, 1, lengthNonZeros)
#'	output
#' }
#'
#' calcMetrics(prepped, metrics=list("richness"=metricTester:::my_richness,
#'	"example"=exampleMetric), new_=TRUE)

calcMetrics <- function(metrics.input, metrics, new_=FALSE)
{
	if(!inherits(metrics.input, "metrics.input"))
	{
		stop("Input needs to be of class 'metrics.input'")
	}
	
	#if a list of named metric functions is not passed in, assign metrics to be NULL, in
	#which case all metrics will be calculated
	if(missing(metrics))
	{
		metrics <- NULL
	}
		
	metrics <- checkMetrics(metrics, new_=new_)
	
	tempResults <- lapply(metrics, function(x) x(metrics.input))
	
	#convert the list to a data frame
	results <- as.data.frame(tempResults)
	
	#add plot names to data frame
	results <- data.frame(plot=row.names(metrics.input$picante.cdm), results)
	
	#get rid of row names
	row.names(results) <- NULL
	
	results
}
