#' Output all spatial simulations as a list of named functions
#'
#' Creates a list of named functions, each of which accept a simulations.input object
#'
#' @details All of the spatial simulations we calculated for our manuscript are included 
#' in this function. To add additional functions, they need to be defined and included
#' here. The function needs to be included with a name, and it must accept a
#' simulations.input object. If the function needs additional elements not included in 
#' that input, then the prepSimulations function must also be revised.
#'
#' @return A list of named functions
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
#' defineSimulations()

defineSimulations <- function()
{
	list("random"=randomArena, "filtering"=filteringArena)
}
