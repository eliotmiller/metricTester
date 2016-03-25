#' lapply wrapper for t-tests
#'
#' Just a utility function. lapplies t-tests over lists of data frames
#'
#' @param null.list A list of dataframes, one per null model, of observed metric scores.
#'
#' @details lapplies tWrapApply over a list of dataframes. There is currently no easy
#' way to pass arguments (like changing mu or whether the test is two- or one-tailed) down
#' to tWrap.
#'
#' @return A dataframe, with one row for each metric. The first column is the mean of the
#' vector of metric values, the second is the p.value of whether it differs from mu=0,
#' and the third is the name of the metric.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' a <- rnorm(n=100)
#' b <- rnorm(n=100, mean=100)
#' ex <- data.frame(a, b)
#' test <- list("ex1"=ex, "ex2"=ex)
#' tWrapLApply(test)

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

tWrapApply <- function(dataframe)
{
	#exclude "richness" and "plot" columns
	exclude <- c("richness", "plot")
	temp <- dataframe[ ,!(names(dataframe) %in% exclude)]

	#apply tWrap over a data frame of metric SES scores for a given null and spatial sim
	output <- apply(temp, 2, tWrap)
	
	#transform the table, convert to a data frame, save the row names as an actual column,
	#exclude "richness" as a metric. output a data frame with three columns
	output <- t(output)

	#convert to data frame
	output <- as.data.frame(output)
	
	#add column names
	names(output) <- c("estimate", "p.value")
	
	#get rid of row names
	output$metric <- row.names(output)
	
	row.names(output) <- NULL

	output
}

tWrap <- function(vect, mu=0)
{
	#set up a blank matrix to save results into
	output <- matrix(nrow=1, ncol=2)
	
	#run a quick t.test on the vector
	temp <- t.test(x=vect, mu=mu)
	
	#pull out the observed mean and p.value from temp and retain these
	output[1,1] <- temp$estimate
	output[1,2] <- temp$p.value
	
	output
}
