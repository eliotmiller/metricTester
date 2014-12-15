#example
#results <- readIn()
#summ <- reduceResults(results)
#test <- sesOverall(summ$ses)

sesOverall <- function(simulation.list)
{
	#lapply tWrapLApply over simulation.list
	temp <- lapply(simulation.list, tWrapLApply)
	
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
