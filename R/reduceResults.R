#' Reduce results from multiLinker into a manageable format
#'
#' Calling multiLinker creates .RDS files, one per iteration. This function will
#' combine these results into a more manageable format.
#'
#' @param results.list The results of a call to readIn()
#'
#' @details Given a list of results readIn() from multiLinker, this function will reduce
#' those results into a manageable format like that expected for calls to quadratOverall
#' and sesOverall. Currently has not been tested on the results from concatenating by
#' both quadrat and richness, will likely need to be updated.
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
