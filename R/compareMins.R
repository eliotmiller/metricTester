#' Utility function to identify minimum values
#'
#' Given a vector where the last element is the minimum, identifies which elements in that
#' vector match the last element.
#'
#' @param x A vector
#' 
#' @details Simple utility function
#'
#' @return A logical vector of length input vector minus 1, corresponding to whether an
#' element of the input vector equals the last element of the input vector.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #create a basic input vector
#' temp <- c(1,2,3,4,5,6,1)
#'
#' #use the compareMins function
#' compareMins(temp)

compareMins <- function(x)
{
	output <- x[1:(length(x)-1)] == x[length(x)]
	return(output)
}
