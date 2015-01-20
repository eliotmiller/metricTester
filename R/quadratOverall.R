#' Overall per simulation-null-metric quadrat test
#'
#' This function provides one of many ways of summarizing and considering simulation
#' results.
#'
#' @param simulation.list A summarized results list such as one output from
#' reduceResults(). See examples.
#'
#' @details This function provides one way of summarizing and considering simulation
#' results. It takes as input a vector of 0s, 1s and 2s (corresponding to within, less,
#' and greater than the 95% CIs, respectively) for all quadrats
#' from a given simulation-null-metric combination, and determines how many quadrats
#' overall deviated beyond expectations. A number of utility functions are also defined
#' but not exported in this function. 
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

lengthApply <- function(dataframe)
{
	#exclude "richness" and "quadrat" columns
	exclude <- c("richness", "quadrat")
	temp <- dataframe[ ,!(names(dataframe) %in% exclude)]

	#apply length
	output <- apply(temp, 2, length)
	
	output
}

lengthLApply <- function(null.list)
{
	#lapply tWrapApply over null.list
	temp <- lapply(null.list, lengthApply)

	#unlist the output into a single vector. let it assign names, it does a decent job
	output <- unlist(temp)

	#just return the simple vector with ugly names, fix the names in a higher function
	#later
	output
}

lengthOnes <- function(input.vector)
{
	ones <- input.vector[input.vector == 1]
	return(length(ones))
}

lengthTwos <- function(input.vector)
{
	twos <- input.vector[input.vector == 2]
	return(length(twos))
}

onesApply <- function(dataframe)
{
	#exclude "richness" and "quadrat" columns
	exclude <- c("richness", "quadrat")
	temp <- dataframe[ ,!(names(dataframe) %in% exclude)]

	#apply tWrap over a data frame of metric SES scores for a given null and spatial sim
	output <- apply(temp, 2, lengthOnes)
	
	output
}

onesLApply <- function(null.list)
{
	#lapply tWrapApply over null.list
	temp <- lapply(null.list, onesApply)

	#unlist the output into a single vector. let it assign names, it does a decent job
	output <- unlist(temp)

	#just return the simple vector with ugly names, fix the names in a higher function
	#later
	output
}

twosApply <- function(dataframe)
{
	#exclude "richness" and "quadrat" columns
	exclude <- c("richness", "quadrat")
	temp <- dataframe[ ,!(names(dataframe) %in% exclude)]

	#apply lengthTwos over a df of metric SES scores for a given null and spatial sim
	output <- apply(temp, 2, lengthTwos)
	
	output
}

twosLApply <- function(null.list)
{
	#lapply tWrapApply over null.list
	temp <- lapply(null.list, twosApply)

	#unlist the output into a single vector. let it assign names, it does a decent job
	output <- unlist(temp)

	#just return the simple vector with ugly names, fix the names in a higher function
	#later
	output
}
