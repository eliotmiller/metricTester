#' Scale output of phyloNtraits to arena size
#'
#' Given a matrix of two traits, and the minimum and maximum extent of the desired arena,
#' will return a data frame of species' traits scaled to the new arena size.
#'
#' @param input.traits Second element of the results of a call to phyloNtraits()
#' @param min.arena Minimum size of arena, e.g. 0
#' @param max.arena Maximum size of arena
#' 
#' @details Scales a matrix of species' traits to a desired mininimum-maximum range.
#' Intended for use in a spatially explicit scenario with two traits, but could easily
#' be co-opted.
#'
#' @return A scaled and named dataframe of species traits
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#'
#' results <- phyloNtraits(50)
#'
#' scaled <- scaler(results[[2]], min.arena=0, max.arena=300)

##write a function that will take the second element of the output of the phyloNtraits
##function, and the min and max arena arguments, and output a data frame of scaled traits
##where min and max traits are min and max of arena

scaler <- function(input.traits, min.arena, max.arena)
{
	std1 <- (input.traits[,1] - min(input.traits[,1]))/(max(input.traits[,1])-min(input.traits[,1]))
	std2 <- (input.traits[,2] - min(input.traits[,2]))/(max(input.traits[,2])-min(input.traits[,2]))
	
	output.trait1 <- (max.arena - min.arena) * std1 + min.arena
	output.trait2 <- (max.arena - min.arena) * std2 + min.arena
	
	output.traits <- cbind(output.trait1, output.trait2)
	
	return(output.traits)
}
