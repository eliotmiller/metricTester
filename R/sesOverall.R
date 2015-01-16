#' Overall per simulation-null-metric SES test
#'
#' This function provides one of many ways of summarizing and considering simulation
#' results.
#'
#' @param simulation.list A summarized results list such as one output from
#' reduceResults(). See examples.
#' @param test Either "ttest" or "wilcotest", depending on whether the user wants to run
#' a two-sided t-test or a Wilcoxon signed rank test.
#'
#' @details This function provides one way of summarizing and considering simulation
#' results. It takes as input a vector of all standardized effect sizes for all quadrats
#' from a given simulation-null-metric combination, and calculates the mean of the vector
#' and whether it differs significantly from a mean of zero. It does this either with a
#' simple two-sided t-test, or with a Wilcoxon signed rank test. If the latter, and if
#' there are three different spatial simulations with names random, filtering and
#' competition, the test is two-sided, less and greater, respectively. 
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- reduceResults(results)
#' #examp <- sesOverall(summ$ses)

sesOverall <- function(simulation.list, test)
{
	#add a line to throw an error if from.node is not properly specified
	if(test != "ttest" & test != "wilcotest")
	{
		stop("test must be set to one of 'ttest' or 'wilcotest'")
	}

	#if test is set to ttest, just do a simple t.test to see if mean differs from 0.
	if(test=="ttest")
	{
		#lapply tWrapLApply over simulation.list
		temp <- lapply(simulation.list, tWrapLApply)
	}
	
	#more probably people will use the wilcoxon signed rank test. go into that here
	else if(test=="wilcotest")
	{
		#if the names of the spatial simulations are "random" "competition" and 
		#"filtering", create a character vector of expected alternative hypotheses to feed
		#into anonymous function below
		if(setequal(names(simulation.list), c("random","filtering","competition")))
		{
			toFeed <- names(simulation.list)
			toFeed[toFeed=="random"] <- "two.sided"
			toFeed[toFeed=="filtering"] <- "less"
			toFeed[toFeed=="competition"] <- "greater"
		}
		
		else if(!setequal(names(simulation.list), c("random","filtering","competition")))
		{
			print("You included new spatial simulations. Modify expectations manually")
			toFeed <- rep("two.sided", 3)
		}
		
		temp <- lapply(seq_along(toFeed), function(x)
			wilcoWrapLApply(simulation.list[[x]], alternative=toFeed[x]))
	}
	
	#reduce the output list into a single data frame
	output <- Reduce(rbind, temp)
	
	#create a vector of expanded simulation names. note that this code is sensitive to
	#changes. for instance, if one simulation tests certain nulls that another does not,
	#this will not end up being correct. this generates a data frame, but we only save the
	#second column
	simNames <- expand.grid(temp[[1]]$null.model, names(simulation.list))[,2]
	
	output$simulation <- simNames
	
	output <- dplyr::select(output, simulation, null.model, metric, estimate, p.value)
	
	output
}
