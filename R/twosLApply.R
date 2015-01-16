#' lapply wrapper for the tWrapApply function
#'
#' lapplies tWrapApply over a list of dataframes.
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
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' a <- rnorm(n=100)
#' b <- rnorm(n=100, mean=100)
#' ex <- data.frame(a, b)
#' test <- list("ex1"=ex, "ex2"=ex)
#' tWrapLApply(test)

twosLApply <- function(null.list)
{
	#lapply tWrapApply over null.list
	temp <- lapply(null.list, twosApply)

	#unlist the output into a single vector. let it assign names, it does a decent job
	output <- unlist(temp)

	#just return the simple vector with ugly names, fix the names in a higher function
	#later
	output
}
