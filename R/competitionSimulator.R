competitionSimulator <- function(tree, initialArena, max.distance, percent.killed, iterations)
{
	for(i in 1:iterations)
	{
		#take the initialArena and kill off some of the individuals in genetically clustered neighborhoods
		killed.arena <- killSome(tree, initialArena, max.distance, percent.killed)

		#add individuals back in, but save this as initialArena, so that it gets plugged back
		#in next iteration instead of the original random arena
		initialArena <- settleSome(killed.arena)
	}

	return(killed.arena)
}
