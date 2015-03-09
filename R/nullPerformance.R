#' Summarize null model performance of a series of summarized simulation results
#'
#' Flexible function that summarizes null model performance after reading in and testing
#' per-simulation results with a function like sesIndiv.
#'
#' @param summarized.results The results of a call to sesIndiv() or something similar.
#' @param simulations Default is "all". Alternatively, can supply a vector of simulation
#' names to summarize the results over.
#' @param metrics Default is "all". Alternatively, can supply a vector of metric
#' names to summarize the results over.
#' @param concat.by Default is "both". Alternatively, can supply either "quadrat" or
#' "richness".
#' 
#' @details 
#'
#' @return A data frame of summarized results
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- sesIndiv(results)
#' #examp <- nullPerformance(summ)

nullPerformance <- function(summarized.results, simulations="all", metrics="all",
	concat.by="both")
{
	if(simulations=="all")
	{
		simulations <- unique(summarized.results$simulation)
	}
	#complicated if statement here. it says if the specified simulations do not contain
	#an entry of character "all" and there is any difference between the specified sims
	#and the unique simulations in the summarized.results table, throw an error
	else if(all(!(simulations %in% "all")) &
		length(setdiff(simulations, unique(summarized.results$simulation))) > 0)
	{
		stop("Specified simulations do not match those in the results table")
	}
	else
	{
		simulations <- simulations
	}

	if(concat.by=="both")
	{
		concat.by <- unique(summarized.results$concat.by)
	}
	else if(all(!(concat.by %in% "both")) &
		length(setdiff(concat.by, unique(summarized.results$concat.by))) > 0)
	{
		stop("concat.by must be set to quadrat, richness, or both")
	}
	else
	{
		concat.by <- concat.by
	}

	if(metrics=="all")
	{
		metrics <- unique(summarized.results$metric)
	}
	else if(all(!(metrics %in% "all")) &
		length(setdiff(metrics, unique(summarized.results$metric))) > 0)
	{
		stop("Specified metrics do not match those in the results table")
	}
	else
	{
		metrics <- metrics
	}

	nulls <- unique(summarized.results$null.model)
	typeI <- c()
	typeII <- c()
	for(i in 1:length(nulls))
	{
		temp <- summarized.results[summarized.results$null.model %in% nulls[i]
			& summarized.results$simulation %in% simulations
			& summarized.results$concat.by %in% concat.by
			& summarized.results$metric %in% metrics,]
		typeI[i] <- mean(temp$typeIrate, na.rm=TRUE)
		typeII[i] <- mean(temp$typeIIrate, na.rm=TRUE)
	}
	results <- data.frame(nulls, typeI, typeII)
	results
}
