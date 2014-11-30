#' Confirm that the spatial simulation functions are in suitable format
#'
#' Utility function. Creates a list of spatial simulations, either those defined in
#' defineSimulations or a named list of simulation functions.
#'
#' @param x Optional named list of spatial simulation functions. Else, defines the
#' spatial simulations as those in defineSimulations.
#' 
#' @details A few quick checks to confirm the spatial simulations are in suitable format.
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
#' checkSimulations(defineSimulations())

checkSimulations <- function(x)
{
	if(is.null(x))
	{
		simulations <- defineSimulations()
	}
	else
	{
		if(!inherits(x, "list"))
		{
			stop("The simulations need to be input as a list of named functions")
		}
		if(is.null(names(x)))
		{
			stop("The simulations need to be input as a list of named functions")
		}
		
		simulations <- x
	}
	
	simulations
}
