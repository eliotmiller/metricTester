#' Reduce randomized results to a manageable list of dataframes
#'
#' The metricsNnulls function creates lists of lists of dataframes. This function will
#' combine the dataframes from each null model into a single data frame. The output is a
#' more manageable list of dataframes. 
#'
#' @param randomizations.list The results of a call to metricsNnulls()
#'
#' @details Given a list of lists of dataframes, such as those that come from a call to
#' metricsNnulls, where the first level of the list relates to a given randomization, and
#' each second level is a data frame containing the calculated metrics after randomization
#' according to a given null model, reduces the results to a simpler list of data frames,
#' where each data frame contains all the results from a given null model from the input
#' randomizations.list.
#'
#' @return A list of data frames. 
#'
#' @export
#'
#' @references Miller, Trisos and Farine.

reduceResults <- function(results.list)
{
	#this command successively combines each element from the long list together via an
	#inner anonymous function that mapply(rbinds) things. the result is a list where the
	#first level relates to a spatial simulation, and each second level is the results of
	#a single iteration from multiLinker. this second level is a matrix, where the first
	#column relates to the ses, the second to the quadrat significance. there are as many
	#rows in the matrix as there are iterations from multiLinker
	firstLevel <- reduceRandomizations(results.list)
	
	#pull the arena and quadratTest results out separately
	ses <- list()
	
	for(i in 1:length(firstLevel))
	{
		ses[[i]] <- firstLevel[[i]][,1]
	}
	
	quadrat <- list()
	
	for(i in 1:length(firstLevel))
	{
		quadrat[[i]] <- firstLevel[[i]][,2]
	}
	
	names(ses) <- names(defineSimulations())
	names(quadrat) <- names(defineSimulations())
	
	secondLevel <- list("ses"=ses, "quadrat"=quadrat)
	
	results <- lapply(secondLevel, function(x) lapply(x, reduceRandomizations))
	
	results
}
