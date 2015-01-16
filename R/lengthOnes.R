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

lengthOnes <- function(input.vector)
{
	ones <- input.vector[input.vector == 1]
	return(length(ones))
}
