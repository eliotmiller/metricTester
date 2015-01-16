#' Apply wrapper for the tWrap function
#'
#' Applies tWrap over a dataframe of metric values, excluding richness.
#'
#' @param dataframe A dataframe of numeric values
#'
#' @details Applies tWrap over a dataframe of metric values, excluding richness, which is
#' considered a metric in some previous functions. Note that there is currently no easy
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
#' tWrapApply(ex)

twosApply <- function(dataframe)
{
	#exclude "richness" and "quadrat" columns
	exclude <- c("richness", "quadrat")
	temp <- dataframe[ ,!(names(dataframe) %in% exclude)]

	#apply lengthTwos over a df of metric SES scores for a given null and spatial sim
	output <- apply(temp, 2, lengthTwos)
	
	output
}
