#' Generate null expectations for community structure metrics
#'
#' Given a phylo object, a picante-style community data matrix (sites are rows,
#' species are columns), a desired null method (any of picante or also 2x, 3x, 1s, & 2s of
#' spacodiR), a desired number of randomizations, and an output file name, will shuffle
#' matrix according to null method, then calculate all phylogenetic community structure
#' metrics as defined in the allMetrics() function, then save each iteration's worth of 
#' shufffled values to a csv file for later import. Also calculates the richness of the 
#' corresponding community.
#'
#' @param tree Phylo object
#' @param orig.matrix A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param null.method A picante-style null, e.g. "richness" or "frequency", or "2x", "3x"
#' "1s" or "2s", which will call spacodiR. It can also now accomodate calls to
#' "regionalNull"
#' @param regional.abundance Optional vector of species names repeated the number of times
#' present in the regional abundance pool. For use with regionalNull.
#' @param no.randomizations The desired number of no.randomizations the function will run,
#' i.e. the number of times orig.matrix will be shuffled and the metrics calculated on it
#' @param temp.file The desired name of the output csv file
#' 
#' @details This runs much faster than trying to do this in memory in R. I will upload
#' some of those type of functions in the near future anyhow. If you call null metrics
#' 2x, 3x, 1s or 2s, it will call spacodiR for the resampling
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
#' library(spacodiR)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))
#'
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)
#'
#' system.time(allMetricsNull(tree=tree, orig.matrix=cdm, null.method="richness", no.randomizations=10, temp.file="output.csv"))

allMetricsNull <- function(tree, orig.matrix, null.method, regional.abundance, no.randomizations, temp.file)
{
	for (j in 1:no.randomizations)
	{
		print(paste("randomization",j))
		if(null.method=="2x")
		{
			spacodi.cdm <- suppressMessages(as.spacodi(orig.matrix))
			temp.matrix <- resamp.2x(spacodi.cdm)
			new.matrix <- suppressMessages(as.picante(temp.matrix))
		}
		
		else if(null.method=="3x")
		{
			spacodi.cdm <- suppressMessages(as.spacodi(orig.matrix))
			temp.matrix <- resamp.3x(spacodi.cdm)
			new.matrix <- suppressMessages(as.picante(temp.matrix))
		}

		else if(null.method=="1s")
		{
			spacodi.cdm <- suppressMessages(as.spacodi(orig.matrix))
			temp.matrix <- resamp.1s(spacodi.cdm)
			new.matrix <- suppressMessages(as.picante(temp.matrix))
		}

		else if(null.method=="2s")
		{
			spacodi.cdm <- suppressMessages(as.spacodi(orig.matrix))
			temp.matrix <- resamp.2s(spacodi.cdm)
			new.matrix <- suppressMessages(as.picante(temp.matrix))
		}

		else if(null.method=="regionalNull")
		{
			new.matrix <- regionalNull(cdm=orig.matrix, tree=tree, regional.abundance=regional.abundance)
		}

		else
		{
			new.matrix <- randomizeMatrix(orig.matrix, null.method)
		}
		
		temp.results <- allMetrics(tree, new.matrix)
		
		if(j == 1)
		{
			write.table(temp.results, file=temp.file, append=FALSE, row.names=FALSE, sep=",")
		}
		else if(j/no.randomizations == 0.2)
		{
			print("20% complete")
			write.table(temp.results, file=temp.file, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
		}
		else if(j/no.randomizations == 0.5)
		{
			print("50% complete")
			write.table(temp.results, file=temp.file, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
		}
		else
		{
			write.table(temp.results, file=temp.file, append=TRUE, col.names=FALSE, row.names=FALSE, sep=",")
		}
	}
	print("File saved to working directory")
}
