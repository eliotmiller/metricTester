#' Generate phylogeny with trait data
#'
#' Given a desired number of species, will generate a tree with that many species and
#' associated trait data for two traits following a Brownian motion evolution model.
#'
#' @param no.species Desired number of species in resulting phylogeny
#' 
#' @details Uses geiger's sim.bdtree function with b=0.1 and d=0. Evolves two traits up
#' phylogeny with Brownian motion evolution process. Sigma from the Brownian motion
#' process is set to 0.1 and cannot currently be manipulated without modifying and
#' redefining the function itself.
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
#' results <- phyloNtraits(50)

phyloNtraits <- function(no.species)
{
	tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=no.species)
	
	trait1 <- rTraitCont(tree, model="BM")
	trait2 <- rTraitCont(tree, model="BM")

	traits <- cbind(trait1, trait2)

	output <- list(tree, traits)

	return(output)
}
