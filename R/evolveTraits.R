#' Evolve two traits up a tree
#'
#' Given a phylogeny, will generate associated trait data for two traits following a 
#' Brownian motion evolution model.
#'
#' @param tree A phylogeny
#' 
#' @details Evolves two traits independently up phylogeny with Brownian motion evolution 
#' process. Sigma is currently set to 0.1 as default.
#'
#' @return A list where the first object is a phylogeny with the desired number of species
#' and the second object is a matrix of trait values for those species.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#'
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' results <- evolveTraits(tree)

evolveTraits <- function(tree, sigma=0.1)
{
	trait1 <- rTraitCont(tree, model="BM", sigma=sigma)
	trait2 <- rTraitCont(tree, model="BM", sigma=sigma)

	traits <- cbind(trait1, trait2)

	output <- list(tree, traits)

	return(output)
}
