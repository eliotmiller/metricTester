#' Summary of results from a single iteration
#'
#' Use Wilcoxon signed rank test to determine whether quadrats from a SINGLE iteration 
#' differ from expectations
#'
#' @param single.iteration The results of a single iteration from multiLinker.
#'
#' @details This function uses a Wilcoxon signed rank test to determine whether the
#' quadrats from a spatial simulation/null/metric from a SINGLE iteration differ from
#' expectations. Assuming there are three spatial simulations named random, filtering, and
#' competition, this function will use two.sided, lesser and greater Wilcoxon tests,
#' respectively.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- sesSingle(results[[1]])

sesSingle <- function(single.iteration)
{
	#identify any null model/spatial sim combos that failed. 
	problems <- failed(single.iteration)
	
	#if there were any problems use a for loop to go through each failed run and set that 
	#element to NULL in the single.iteration results
	if(dim(problems)[1] > 0)
	{
		for(i in 1:dim(problems)[1])
		{
			sim <- problems[i,"simulation"]
			null <- problems[i,"null"]
			single.iteration[[sim]]$ses[[null]] <- NULL
		}
	}

	#if the names of the spatial simulations are "random" "competition" and 
	#"filtering", create a character vector of expected alternative hypotheses to feed
	#into anonymous function below
	if(setequal(names(single.iteration), c("random","filtering","competition")))
	{
		toFeed <- names(single.iteration)
		toFeed[toFeed=="random"] <- "two.sided"
		toFeed[toFeed=="filtering"] <- "less"
		toFeed[toFeed=="competition"] <- "greater"
	}
		
	else if(!setequal(names(single.iteration), c("random","filtering","competition")))
	{
		print("You included new spatial simulations. Modify expectations manually")
		toFeed <- rep("two.sided", 3)
	}
	
	temp <- lapply(seq_along(toFeed), function(x)
		wilcoWrapLApply(single.iteration[[x]]$ses, alternative=toFeed[x]))

	#reduce the output list into a single data frame
	output <- Reduce(rbind, temp)
	
	#create a data frame of expanded simulation names
	simNames <- expand.grid(temp[[1]]$null.model, names(single.iteration))
	
	#give it names
	names(simNames) <- c("null", "simulation")
	
	#if there were any problems, take these names out of simNames
	if(dim(problems)[1] > 0)
	{
		simNames <- simNames[!(simNames$null %in% problems$null & 
			simNames$simulation %in% problems$simulation),]
	}
	
	#use just the second col from simNames
	output$simulation <- simNames[,2]
	
	#select the columns so they come out in a nice order
	output <- dplyr::select(output, simulation, null.model, metric, estimate, p.value)
	
	output
}
