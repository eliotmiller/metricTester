#' Return summary statistics from a data frame of randomized metric values
#'
#' Summarizes observed metric scores. Returns the mean, standard deviation and 
#' 95% confidence intervals of each quadrat or observed richness. 
#'
#' @param null.output Vector of numbers
#' @param concat.by Either richness or quadrat
#' 
#' @details Given a data frame of metric values, summarizes either by quadrat or richness.
#' Outputs the mean, standard deviation and 95% confidence intervals of each quadrat or
#' observed richness. 
#'
#' @return A data frame of summarized metric scores
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(dplyr)
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
#'
#' test <- summaries(results$frequency, concat.by="richness")

summaries <- function(null.output, concat.by="richness")
{
	#set up the results table to be the appropriate size if concat.by = richness
	#you want the table to have one row for every richness value and four columns for each
	#metric plus an additional column for the richness
	if(concat.by=="richness")
	{
		results <- matrix(ncol=4 * (dim(null.output)[2]-2) + 1, 
			nrow=length(unique(null.output$richness)), 0)
	}
	#set up the results table to be the appropriate size if concat.by = quadrat
	else if(concat.by=="quadrat")
	{
		results <- matrix(ncol=4 * (dim(null.output)[2]-2) + 1, 
			nrow=length(unique(null.output$quadrat)), 0)
	}
	#throw an error if neither of above options
	else
	{
		stop("Argument concat.by must be either 'richness' or 'quadrat'")
	}
	
	if(concat.by=="richness")
	{
		#start the for loop at i=3 to skip the richness and quadrat columns
		for(i in 3:dim(null.output)[2])
		{
			#create a temporary data frame because difficult to use dplyr over big tables
			temp <- data.frame(richness=null.output$richness, metric=null.output[,i])
			grouped <- group_by(temp, richness)
			#note that we want to start the last column at 5, to leave the first blank for
			#either quadrat or richness names
			lastCol <- (4*(i-2))+1
			results[,lastCol-3] <- as.data.frame(summarize(grouped, 
				mean(metric, na.rm = TRUE)))[,2]
			results[,lastCol-2] <- as.data.frame(summarize(grouped, 
				sd(metric, na.rm = TRUE)))[,2]
			results[,lastCol-1] <- as.data.frame(summarize(grouped, 
				quantile(metric, 0.025, na.rm=TRUE)))[,2]
			results[,lastCol] <- as.data.frame(summarize(grouped, 
				quantile(metric, 0.975, na.rm=TRUE)))[,2]
		}
		#dplyr automatically creates title for summarized columns, so just pull last used
		rows <- summarize(grouped, mean(metric, na.rm = TRUE))[,1]
	}
	else if(concat.by=="quadrat")
	{
		#start the for loop at i=3 to skip the richness and quadrat columns
		for(i in 3:dim(null.output)[2])
		{
			#create a temporary data frame because difficult to use dplyr over big tables
			temp <- data.frame(quadrat=null.output$quadrat, metric=null.output[,i])
			grouped <- group_by(temp, quadrat)
			#note that we want to start the last column at 5, to leave the first blank for
			#either quadrat or richness names
			lastCol <- (4*(i-2))+1
			results[,lastCol-3] <- as.data.frame(summarize(grouped, 
				mean(metric, na.rm = TRUE)))[,2]
			results[,lastCol-2] <- as.data.frame(summarize(grouped, 
				sd(metric, na.rm = TRUE)))[,2]
			results[,lastCol-1] <- as.data.frame(summarize(grouped, 
				quantile(metric, 0.025, na.rm=TRUE)))[,2]
			results[,lastCol] <- as.data.frame(summarize(grouped, 
				quantile(metric, 0.975, na.rm=TRUE)))[,2]
		}
		#dplyr automatically creates title for summarized columns, so just pull last used
		rows <- summarize(grouped, mean(metric, na.rm = TRUE))[,1]
	}
	#set the results to a dataframe so it can have a column with quadrat names if needed
	results <- as.data.frame(results)
	#add in the first column info
	results[,1] <- rows
	#create column names
	metricNames <- names(null.output)[names(null.output)!="richness" & names(null.output)!="quadrat"]
	summaryNames <- c("average", "sd", "lower", "upper")
	#this last set of names is the metric name plus each of the different summaries
	comboNames <- paste(rep(metricNames, each = length(summaryNames)), 
		rep(summaryNames, length(metricNames)), sep = ".")
	#bind this to either "richness" or "quadrat" for name of first column
	if(concat.by=="richness")
	{
		comboNames <- c("richness", comboNames)
	}
	else if(concat.by=="quadrat")
	{
		comboNames <- c("quadrat", comboNames)
	}
	names(results) <- comboNames
	return(results)
}
