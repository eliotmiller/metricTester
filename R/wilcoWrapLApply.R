#' lapply wrapper for the wilcoWrapApply function
#'
#' lapplies wilcoWrapApply over a list of dataframes.
#'
#' @param null.list A list of dataframes, one per null model, of observed metric scores.
#'
#' @details lapplies wilcoWrapApply over a list of dataframes. 
#'
#' @return A dataframe, with one row for each metric. The first column is the mean of the
#' vector of metric values, the second is the p.value of whether it differs from mu=0,
#' and the third is the name of the metric.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' a <- rnorm(n=100)
#' b <- rnorm(n=100, mean=100)
#' ex <- data.frame(a, b)
#' test <- list("ex1"=ex, "ex2"=ex)
#' wilcoWrapLApply(test, alternative="two.sided")

wilcoWrapLApply <- function(null.list, alternative)
{
	#lapply wilcoWrapApply over null.list. note that null.list is a list of data frames
	#from a single spatial simulation. you will further lapply this function over a list
	#of lists of data frames in a final test function
	temp <- lapply(null.list, wilcoWrapApply, alternative=alternative)

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
