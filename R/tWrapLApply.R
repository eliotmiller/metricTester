#null.list is a list of lists, where each element of the second level of the list is a
#data frame for a given null model

tWrapLApply <- function(null.list)
{
	#lapply tWrapApply over null.list
	temp <- lapply(null.list, tWrapApply)

	#reduce the output list into a single data frame
	output <- Reduce(rbind, temp)

	#create a vector of expanded null model names. note that this code is sensitive to
	#changes. for instance, if one null model tests certain metrics that another does not,
	#this will not end up being correct. this generates a data frame, but we only save the
	#first column
	nullNames <- expand.grid(temp[[1]]$metric, names(null.list))[,2]
	
	output$null.model <- nullNames
	
	output
}
