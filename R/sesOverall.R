#' Overall per simulation-null-metric SES test
#'
#' This function provides one of many ways of summarizing and considering simulation
#' results.
#'
#' @param simulation.list A summarized results list such as one output from
#' reduceResults(). See examples.
#'
#' @details This function provides one way of summarizing and considering simulation
#' results. It takes as input a vector of all standardized effect sizes for all quadrats
#' from a given simulation-null-metric combination, and calculates the mean of the vector
#' and whether it differs significantly from a mean of zero. 
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- reduceResults(results)
#' #test <- sesOverall(summ$ses)

sesOverall <- function(simulation.list)
{
	#lapply tWrapLApply over simulation.list
	temp <- lapply(simulation.list, tWrapLApply)
	
	#reduce the output list into a single data frame
	output <- Reduce(rbind, temp)
	
	#create a vector of expanded simulation names. note that this code is sensitive to
	#changes. for instance, if one simulation tests certain nulls that another does not,
	#this will not end up being correct. this generates a data frame, but we only save the
	#second column
	simNames <- expand.grid(temp[[1]]$null.model, names(simulation.list))[,2]
	
	output$simulation <- simNames
	
	output <- dplyr::select(output, simulation, null.model, metric, estimate, p.value)
	
	output
}
