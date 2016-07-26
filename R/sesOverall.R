#' Overall per simulation-null-metric SES test
#'
#' This function provides one of many ways of summarizing and considering simulation
#' results.
#'
#' @param simulation.list A summarized results list such as one output from
#' reduceResults(). See examples.
#' @param test Either "ttest" or "wilcotest", depending on whether the user wants to run
#' a two-sided t-test or a Wilcoxon signed rank test.
#' @param direction Character vector that needs to be provided if spatial simulations
#' beyond the standard "random", "filtering", and "competition" simulations are run.
#' The character vector must be the same length as the number of spatial simulations that
#' were run, and can take the possible values of "two.sided" (for a two-tailed test when
#' the SES scores are expected to be centered around 0), "less" (for when the observed SES
#' scores are expected to be less than 0), and "greater" (for when the observed SES scores
#' are expected to be greater than 0). For instance, habitat filtering would be set to
#' "less". The relevant simulation to which these directional tests will be applied can be
#' determined by calling names(simulation.list).
#'
#' @details This function provides one way of summarizing and considering simulation
#' results. It takes as input a vector of all standardized effect sizes for all plots
#' from a given simulation-null-metric combination, and calculates the mean of the vector
#' and whether it differs significantly from a mean of zero. It does this either with a
#' simple two-sided t-test, or with a Wilcoxon signed rank test. If the latter, and if
#' there are three different spatial simulations with names random, filtering and
#' competition, the test is two-sided, less and greater, respectively. If additional
#' spatial simulations are included, requiring modified expectations, these can be passed
#' along with the "direction" argument.
#'
#' @return A data frame summarizing the mean, overall standardized effect sizes and the
#' significance of those devations from expectations for each simulation, null, metric
#' combination. This test works across all iterations, and looks for overall shifts in
#' SES from expectations (see details for for expectations).
#'
#' @export
#'
#' @importFrom dplyr select
#' @importFrom stats cor dist sd t.test weighted.mean wilcox.test
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- reduceResults(results)
#' #examp <- sesOverall(summ$ses, test="wilcotest")

sesOverall <- function(simulation.list, test, direction)
{
	#dumb hack to pass R CMD check
	simulation <- "hack"
	null.model <- "hack"
	metric <- "hack"
	estimate <- "hack"
	p.value <- "hack"

	#determine whether the results were concatenated by plot, richness, or both. if by
	#both, then this will return true
	if(class(simulation.list[[1]][[1]])=="list")
	{
		concat.by <- "both"
	}
	
	#this will be a data frame and the first column will be named "richness" if
	#concatenated by that
	else if(is.data.frame(simulation.list[[1]][[1]]))
	{
		if(names(simulation.list[[1]][[1]])[1]=="richness")
		{
			concat.by <- "richness"
		}
		else if(names(simulation.list[[1]][[1]])[1]=="plot")
		{
			concat.by <- "plot"
		}
	}
	else
	{
		stop("Unexpected function input")
	}

	#add a line to throw an error if test is not properly specified
	if(test != "ttest" & test != "wilcotest")
	{
		stop("test must be set to one of 'ttest' or 'wilcotest'")
	}

	#if test is set to ttest, just do a simple t.test to see if mean differs from 0.
	if(test=="ttest" & (concat.by=="richness" | concat.by=="plot"))
	{
		#lapply tWrapLApply over simulation.list
		tempAll <- lapply(simulation.list, tWrapLApply)
	}
	
	else if(test=="ttest" & concat.by=="both")
	{
		stop("t.test with concat by both not currently operational")
	}
	
	#more probably people will use the wilcoxon signed rank test. go into that here
	else if(test=="wilcotest" & (concat.by=="richness" | concat.by=="plot"))
	{
		#if the names of the spatial simulations are "random" "competition" and 
		#"filtering", create a character vector of expected alternative hypotheses to feed
		#into anonymous function below
		if(setequal(names(simulation.list), c("random","filtering","competition")))
		{
			direction <- names(simulation.list)
			direction[direction=="random"] <- "two.sided"
			direction[direction=="filtering"] <- "less"
			direction[direction=="competition"] <- "greater"
		}
		
		#if the names of the spatial simulations do not match the standard, direction
		#needs to be supplied as a character vector of same length as simulation.list
		else
		{
			if(length(direction) != length(simulation.list))
			{
				stop("direction must be a character vector of same length as number of spatial simulations")
			}
		
			if(length(setdiff(direction, c("two.sided", "less", "greater"))) > 0)
			{
				stop("possible directions are limited to two.sided, less, and greater")
			}
		}
		
		tempAll <- lapply(seq_along(direction), function(x)
			wilcoWrapLApply(simulation.list[[x]], alternative=direction[x]))
	}
	
	else if(test=="wilcotest" & concat.by=="both")
	{
		if(setequal(names(simulation.list), c("random","filtering","competition")))
		{
			direction <- names(simulation.list)
			direction[direction=="random"] <- "two.sided"
			direction[direction=="filtering"] <- "less"
			direction[direction=="competition"] <- "greater"
		}
		
		#if the names of the spatial simulations do not match the standard, direction
		#needs to be supplied as a character vector of same length as simulation.list
		else
		{
			if(length(direction) != length(simulation.list))
			{
				stop("direction must be a character vector of same length as number of spatial simulations")
			}
		
			if(length(setdiff(direction, c("two.sided", "less", "greater"))) > 0)
			{
				stop("possible directions are limited to two.sided, less, and greater")
			}
		}
		
		tempAll <- list()
		
		#i refers to spatial simulation (you will only feed in $ses components)
		for(i in 1:length(direction))
		{
			#j refers to null models
			tempNull <- list()
			for(j in 1:length(simulation.list[[1]]))
			{
				#k refers either to concatenating by richness or by plot
				for(k in 1:length(simulation.list[[1]][[1]]))
				{
					temp <- wilcoWrapLApply(simulation.list[[i]][[j]],
						alternative=direction[i])
					#this function was designed for use over a list of null models, so
					#change name here to reflect what it is actually being used for
					names(temp)[4] <- "concat.by"
				}
				#set the jth element of tempNull equal to the dataframe temp
				tempNull[[j]] <- temp
				#repeat the name of the null the correct length of the null model and bind
				tempNull[[j]]$null.model <- rep(names(simulation.list[[i]][j]),
					dim(tempNull[[j]])[1])
			}
			#set the ith element of tempAll equal to the list tempNull
			tempAll[[i]] <- tempNull
			#reduce the list down
			tempAll[[i]] <- Reduce(rbind, tempAll[[i]])
			#repeat the sim name the correct length and bind
			tempAll[[i]]$simulation <- rep(names(simulation.list[i]),
				dim(tempAll[[i]])[1])
		}
	}
	
	#reduce the output list into a single data frame
	output <- Reduce(rbind, tempAll)
	
	if(concat.by=="richness" | concat.by=="plot")
	{
		#create a vector of expanded simulation names. note that this code is sensitive to
		#changes. for instance, if one simulation tests certain nulls that another does
		#not, this will not end up being correct. this generates a data frame, but we only
		#save the second column
		simNames <- expand.grid(tempAll[[1]]$null.model, names(simulation.list))[,2]
	
		output$simulation <- simNames
	
		output <- cbind(output, concat.by=rep(concat.by, dim(output)[1]))
	
		output <- select(output, simulation, null.model, metric, concat.by,
			estimate, p.value)		
	}
	
	else if(concat.by=="both")
	{
		output <- select(output, simulation, null.model, metric, concat.by,
			estimate, p.value)

		#to make the output from reduceResults legible, I called results summarized by
		#each option "by.plot", "by.richness". this looks weird in the output of
		#this function, so fix quickly here
		output$concat.by <- as.character(output$concat.by)
		output$concat.by[output$concat.by=="by.richness"] <- "richness"
		output$concat.by[output$concat.by=="by.plot"] <- "plot"
	}

	output
}
