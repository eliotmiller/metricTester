#' Reduce randomized results to a manageable list of dataframes
#'
#' The metricsNnulls function creates lists of lists of dataframes. This function will
#' combine the dataframes from each null model into a single data frame. The output is a
#' more manageable list of dataframes. 
#'
#' @examples
#' library(geiger)
#' library(picante)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)
#'
#' rawResults <- metricsNnulls(tree, cdm)
#'
#' results <- reduceRandomizations(rawResults)

reduceRandomizations <- function(randomizations.list)
{
	#this command successively combines each element from the long list together via an
	#inner anonymous function that mapply(rbinds) things
	finalResults <- Reduce(function(y, z) mapply(rbind, y, z, SIMPLIFY=FALSE), 
		randomizations.list, accumulate=FALSE)
	finalResults
}
