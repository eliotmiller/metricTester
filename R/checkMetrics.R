#' Confirm that the metric functions are in suitable format
#'
#' Utility function. Creates a list of functions, either those defined in defineMetrics
#' or a named list of metric functions.
#'
#' @param x Optional. If not provided, defines the metrics as those in defineMetrics. Else
#' either a character vector or a named list of functions, depending on whether new_ is
#' set to TRUE or FALSE. See calcMetrics.
#' @param new_ Whether or not new metrics are being defined on the fly. Default is FALSE.
#' Set to TRUE if a new metric is being used.
#' 
#' @details A few quick checks to confirm the metric functions are input in suitable
#' format.
#'
#' @return A list of functions.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' checkMetrics(names(defineMetrics()))

checkMetrics <- function(x, new_=FALSE)
{
	#if nothing is passed to checkMetrics, then just calculate all metrics
	if (is.null(x))
	{
		metrics <- defineMetrics()
	}

	#if a character vector is passed to checkMetrics, and new_ remains FALSE, run the
	#unexported function metricNameMatcher. this checks whether all the names in the
	#character vector match named functions from defineMetrics, then outputs a named list
	else if(!is.null(x) & new_==FALSE)
	{
		if (!inherits(x, "character"))
		{
			stop("The metrics need to be input as a character vector of named functions")
		}
					
		metrics <- metricNameMatcher(metric.name.vector=x)
	}
	
	#if a new metric is passed to checkMetrics, then new_ needs to be set to TRUE. 
	#in this case, it needs to be passed as a named list
	else if(!is.null(x) & new_==TRUE)
	{
		if (!inherits(x, "list"))
		{
			stop("New metrics need to be input as a list of named functions")
		}
		if (is.null(names(x)))
		{
			stop("New metrics need to be input as a list of named functions")
		}
		
		metrics <- x
	}					
	metrics
}

metricNameMatcher <- function(metric.name.vector)
{
	#define all metrics here
	allPossible	<- defineMetrics()
	
	#throw an error if the metric.name.vector does not perfectly match named metrics
	if(length(setdiff(metric.name.vector, names(allPossible))) > 0)
	{
		stop("Not all of your specified metrics match those named in defineMetrics()")
	}
	
	#now subset them to just those defined in metric.name.vector
	output <- allPossible[metric.name.vector]
	
	output
}
