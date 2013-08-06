#' Generate null expectations for community structure metrics
#'
#' Given a phylo object, a picante-style community data matrix (sites are rows,
#' species are columns), a desired null method (sensu picante), a desired number of
#' iterations, and an output file name, will shuffle matrix according to null method, then
#' calculate all phylogenetic community structure metrics as defined in the allMetrics()
#' function, then save each iteration's worth of shufffled values to a csv file for later
#' import. Also calculates the richness of the corresponding community.
#'
#' @param tree Phylo object
#' @param orig.matrix A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param null.method A picante-style null, e.g. "richness" or "frequency"
#' @param iterations The desired number of iterations the function will run, i.e. the
#' number of times the orig.matrix will be shuffled and the metrics calculated on it
#' @param file.name The desired name of the output csv file
#' 
#' @details This runs much faster than trying to do this in memory in R. I will upload
#' some of those type of functions in the near future anyhow. 
#'
#' @return A csv file with each column equal to the value of a given metric for the
#' shuffled community in question (a row in the input matrix). 
#'
#' @export
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
#' system.time(allMetricsNull.csv(tree, cdm, "richness", 10, "output.csv"))

allMetricsNull.csv <- function(tree, orig.matrix, null.method, iterations, file.name)
{
	for (i in 1:iterations)
	{
		new.matrix <- randomizeMatrix(orig.matrix, null.method)
		temp.results <- allMetrics(tree, new.matrix)
		if(i == 1)
		{
			write.table(temp.results, file=file.name, append=FALSE, row.names=FALSE, sep=",")
		}
		else if(i/iterations == 0.2)
		{
			print("20% complete")
			write.table(temp.results, file=file.name, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
		}
		else if(i/iterations == 0.5)
		{
			print("50% complete")
			write.table(temp.results, file=file.name, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
		}
		else
		{
			write.table(temp.results, file=file.name, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
		}
	}
	print("File saved to working directory")
}
