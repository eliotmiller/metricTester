#' Simple wrapper for Wilcoxon signed rank test
#'
#' Given a numeric vector, a proposed true mean value, and an optional alternative
#' hypothesis, return the mean of the input vector and p.value.
#'
#' @param vect A numeric vector
#' @param mu Proposed true mean. Default is 0.
#' @param alternative Optional alternative hypothesis. Default is "two-sided". Use 
#' "greater" for competition; "less" for habitat filtering.
#'
#' @details Given a vector, a proposed true mean value (default is mu=0), and an 
#' optional alternative hypothesis, return the mean of the vector and the p.value as to
#' whether it differs from the proposed mean. Using "greater" or "less" employ one-sided
#' tests when there is good reason for proposing the alternative hypothesis.
#'
#' @return A one-row matrix, where the first column is the mean of the input vector, and
#' the second is the p.value as to whether the vector differs from the proposed true mean.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' a <- rnorm(100)
#' wilcoWrap(a)

wilcoWrap <- function(vect, mu=0, alternative)
{
	if(missing(alternative))
	{
		alternative <- "two.sided"
	}

	#set up a blank matrix to save results into
	output <- matrix(nrow=1, ncol=2)
	
	#run a quick t.test on the vector
	temp <- wilcox.test(x=vect, mu=mu, alternative=alternative)

	#pull out the observed mean and p.value from temp and retain these
	output[1,1] <- mean(vect, na.rm=TRUE)
	output[1,2] <- temp$p.value
	
	output
}
