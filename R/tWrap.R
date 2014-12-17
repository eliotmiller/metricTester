#' Simple wrapper for t-test
#'
#' Given a numeric vector and a proposed true mean value, return the mean of the vector
#' and the p.value as to whether it differs from the proposed mean.
#'
#' @param vect A numeric vector
#' @param mu Proposed true mean. Default is 0.
#'
#' @details Given a vector and a proposed true mean value (default is mu=0), return the 
#' mean of the vector and the p.value as to whether it differs from the proposed mean.
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
#' tWrap(a)

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
