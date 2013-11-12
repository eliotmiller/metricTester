randomArena <- function(tree, x_min, x_max, y_min, y_max, mean.log.individuals, sd.parameter)
{
	#generate log-normal regional abundance curve, and randomly assign abundances to species
	indivs.per.species <- rlnorm(n=length(tree$tip.label), mean.log.individuals, sdlog=1)
	
	#set species with < 0 individuals to 0 abundance
	indivs.per.species[indivs.per.species < 0] <- 0

	#round abundances to no decimal places
	indivs.per.species <- round(indivs.per.species)

	#actually generate a vector individuals with species identities (the "regional pool")
	individuals <- c()

	individuals <- rep(tree$tip.label, times=indivs.per.species)

	#start a dataframe to bind X,Y coordinates into	
	arena <- data.frame(individuals)

	#generate random X,Y coordinates centered around the middle of the arena
	arena$X <- sample(x_min:x_max, size=length(individuals), replace=TRUE)
	arena$Y <- sample(y_min:y_max, size=length(individuals), replace=TRUE)
	
	#create and return the output
	
	output <- list(regional.abundance=individuals, arena=arena, dims=c(x_min, x_max, y_min, y_max))
	
	return(output)
}
