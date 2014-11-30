#' Confirm that the null model functions are in suitable format
#'
#' Utility function. Creates a list of null models, either those defined in defineNulls
#' or a named list of null model functions.
#'
#' @param x Optional named list of null models. Else, defines the nulls as those
#' defined in defineNulls.
#' 
#' @details A few quick checks to confirm the null model functions are input in suitable
#' format.
#'
#' @return A list of functions.
#'
#' @export
#'
#' @import phylobase grid ecoPDcorr
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#' library(picante)
#'
#' checkNulls(defineNulls())

checkNulls <- function(x)
{
	if (is.null(x))
	{
		nulls <- defineNulls()
	}
	else
	{
		if (!inherits(x, "list"))
		{
			stop("The null models need to be input as a list of named functions")
		}
		if (is.null(names(x)))
		{
			stop("The null models need to be input as a list of named functions")
		}
				
		nulls <- x
	}
	
	nulls
}
