settleSome <- function(killSomeOutput)
{
	#sample the same number of individuals you killed from the regional abundance vector
	individuals <- sample(killSomeOutput$regional.abundance, size=killSomeOutput$no.killed)
	
	#start a dataframe to bind X,Y coordinates into	
	to.bind <- data.frame(individuals)

	#generate random X,Y coordinates centered around the middle of the arena
	to.bind$X <- sample(killSomeOutput$dim[1]:killSomeOutput$dim[2], size=length(individuals), replace=TRUE)
	to.bind$Y <- sample(killSomeOutput$dim[3]:killSomeOutput$dim[4], size=length(individuals), replace=TRUE)
	
	output <- list(regional.abundance=killSomeOutput$regional.abundance, arena=rbind(killSomeOutput$arena, to.bind), dims=killSomeOutput$dims)
}
