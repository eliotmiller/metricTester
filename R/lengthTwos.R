#' Calculate the length of 1s in a vector
#'
#' Explain
#'
#' @param input.vector A vector of 0s, 1s, and 2s
#' 
#' @details
#'
#' @export
#'
#' @return
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples

lengthTwos <- function(input.vector)
{
	twos <- input.vector[input.vector == 2]
	return(length(twos))
}
