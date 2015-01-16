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

quadratOverall <- function(simulation.list)
{
	#lapply all ones, twos and length LApply functions over the entire simulation list
	tempOnes <- lapply(simulation.list, onesLApply)
	tempTwos <- lapply(simulation.list, twosLApply)
	tempLength <- lapply(simulation.list, lengthLApply)
	
	#unlist and bind together into a data frame
	ones <- unlist(tempOnes)
	twos <- unlist(tempTwos)
	lengths <- unlist(tempLength)
	
	temp <- data.frame(ones, twos, lengths)
	
	#create better categories (i.e. separate the single string names into simulation,
	#null and metric columns). this is a long command but it runs really quickly
	betterNames <- suppressWarnings(data.frame(Reduce(rbind, 
		strsplit(names(ones), split="[.]"))))
	names(betterNames) <- c("simulation", "null", "metric")
	
	output <- cbind(betterNames, temp)
	
	row.names(output) <- NULL
	
	output
}
