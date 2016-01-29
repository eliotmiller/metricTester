#' Wrapper for summarizing randomizations and testing significance of observed metrics
#'
#' Given a data frame of observed metrics and a list of randomizations based on different
#' null models, returns a list of data frames summarizing the significance of observed 
#' metrics both at the single quadrat and the entire arena level.
#'
#' @param observed Data frame of observed metric scores, such as from observedMetrics()
#' @param reduced.randomizations List of random, reduced results, such as those from
#' reduceRandomizations()
#' @param concat.by Whether to concatenate the randomizations by richness, quadrat or both
#' @param metrics Optional list of named metric functions to use. If invoked, this option
#' will likely be used to run a subset of the defined metrics.
#' 
#' @details This function wraps a number of smaller functions into a useful type I and II
#' error checker. It takes a reduced list of randomizations such as those reduced from
#' metricsNnulls with reduceRandomizations, summarizes the mean,
#' SD, and CI of each metric plus null model either at the richness or quadrat level,
#' then compares the observed metric scores to those summarized metrics. It return a list
#' with two elements. The first is a list of data frames, where each corresponds to the 
#' standardized effect scores of the observed metrics for a given null model. The second
#' is a list of data frames, where each corresponds to whether a given quadrat deviates
#' beyond CI. For the latter, 0 corresponds to within CI, 1 corresponds to less than the
#' CI, and 2 corresponds to greater than the CI.
#'
#' @return A list of lists of data frames.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #simulate a log normal abundance distribution
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' #simulate a community of varying richness
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' #run the metrics and nulls combo function
#' rawResults <- metricsNnulls(tree, cdm, randomizations=3)
#'
#' #summarize the results
#' results <- reduceRandomizations(rawResults)
#'
#' #calculate the observed metrics from the input CDM
#' observed <- observedMetrics(tree, cdm)
#'
#' test <- errorChecker(observed, results, "richness")

betaErrorChecker <- function(single.iteration)
{
	#this is a pretty lame way to define what metricsNnulls got used, but it is a hack
	#for now. if we add other beta metrics they likely will need a new significance
	#test and we should re-write this whole function
	usedMetrics <- c("Ist", "Pst", "Bst", "PIst")
	
	usedNulls <- names(single.iteration$randomized[[1]])

	#set up a blank matrix to save results in. will re-use this matrix for each spatial
	#simulation
	resultMatrix <- matrix(nrow=1, ncol=length(usedMetrics))

	#set up a blank list to save results of blank matrix into as it gets filled. note that
	#because we have the option to set elements in result matrix to 0, we do not need to
	#reset it after each iteration. to facilitate moving down the list with each element
	#of k, just start a placeholder here
	results <- list()
	placeholder <- 0

	#i refers to simulations. set up a sublist for each spatial simulation, where each
	#element of that list refers to a given null model
	for(i in 1:length(single.iteration$observed))
	{
		temp <- list()
		#j refers to null models.
		for(j in 1:length(usedNulls))
		{
			#k refers to metrics
			for(k in 1:length(usedMetrics))
			{
				if(names(single.iteration$observed)[i]=="random")
				{
					if(single.iteration$observed[[i]][usedMetrics[k]] < 
						quantile(single.iteration$randomized[[i]][[usedNulls[j]]][,usedMetrics[k]], 0.025))
					{
						resultMatrix[1, k] <- 1
					}
					else if(single.iteration$observed[[i]][usedMetrics[k]] > 
						quantile(single.iteration$randomized[[i]][[usedNulls[j]]][,usedMetrics[k]], 0.975))
					{
						resultMatrix[1, k] <- 1
					}
					else
					{
						resultMatrix[1, k] <- 0
					}
					rownames(resultMatrix) <- "typeI"
				}
				else if(names(single.iteration$observed)[i]=="filtering")
				{
					if(single.iteration$observed[[i]][usedMetrics[k]] <
						quantile(single.iteration$randomized[[i]][[usedNulls[j]]][,usedMetrics[k]], 0.95))
					{
						resultMatrix[1, k] <- 1
					}
					else
					{
						resultMatrix[1, k] <- 0
					}
					rownames(resultMatrix) <- "typeII"
				}
				else if(names(single.iteration$observed)[i]=="competition")
				{
					if(single.iteration$observed[[i]][usedMetrics[k]] >
						quantile(single.iteration$randomized[[i]][[usedNulls[j]]][,usedMetrics[k]], 0.05))
					{
						resultMatrix[1, k] <- 1
					}
					else
					{
						resultMatrix[1, k] <- 0
					}
					rownames(resultMatrix) <- "typeII"
				}
			}
		placeholder <- placeholder + 1
		colnames(resultMatrix) <- usedMetrics
		temp[[j]] <- resultMatrix
		}
		names(temp) <- usedNulls
		results[[i]] <- temp
	}
	
	names(results) <- names(single.iteration$observed)
	results
}
