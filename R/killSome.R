killSome <- function(tree, arenaOutput, max.distance, percent.killed)
{
	#save the species identities of all individuals
	individual.identities <- arenaOutput$arena$individuals

	#create a genetic distance matrix
	gen.dist <- cophenetic(tree)
	
	#create a matrix of individuals for use in geographic distance calculations. obviously very similar to the input data frame, but dist doesn't work right with data frames
	for.geo.dist <- matrix(cbind(arenaOutput$arena$X, arenaOutput$arena$Y), ncol=2)
	
	#calculate all pairwise geographic distances
	geo.dist <- dist(for.geo.dist)
	
	#convert the distance matrix into a real matrix
	geo.dist.matrix <- as.matrix(geo.dist)

	#set all geographic distances from one individual to itself to NA	
	diag(geo.dist.matrix) <- NA
	
	#set all geographic distances greater than the max distance to consider to NA
	geo.dist.matrix[geo.dist.matrix > max.distance] <- NA
	
	#replace these geographic distances with genetic distances, so we can calculate the 
	#genetic neighborhood of individuals. first figure out who the species involved in
	#each pairwise comparison are
	
	#the individuals involved in each comparison
	individual.involved <- which(!is.na(geo.dist.matrix), arr.ind=TRUE)
	
	#replace with species' identities. there is probably a better way of doing this, but
	#subsetting in this manner seems to work, and just spins right through the whole data
	#frame one column of individual.involved at a time. if i split the results in two and
	#stack into two columns it gives the right results
	species.involved <- matrix(individual.identities[individual.involved], nrow=length(individual.identities[individual.involved])/2, ncol=2)
	
	#find the appropriate genetic distances for all these species combinations
	specific.gen.dist <- gen.dist[species.involved]
	
	#plug these genetic distances into the geographic distance matrix of just the closest individuals
	specific.gen.dist.matrix <- geo.dist.matrix
	specific.gen.dist.matrix[which(!is.na(specific.gen.dist.matrix))] <- specific.gen.dist
	
	#find the average relatedness of every individual to its closest neighbors
	average.relatedness <- apply(specific.gen.dist.matrix, 2, mean, na.rm=TRUE)
	
	#define the mean genetic distance below which individuals will be considered to be in genetically clustered geographic neighborhoods
	cutoff <- quantile(average.relatedness, probs=percent.killed, na.rm=TRUE)
	
	#find the individual IDs of those in the most genetically clustered neighborhoods
	individuals.considered <- which(average.relatedness <= cutoff)
	
	####NEED TO INCLUDE AN ARGUMENT HERE TO NOT CONSIDER MIN of ZERO IF YOU WANT TO NOT
	####CONSIDER CONSPECIFICS

	#find the minimum genetic distance between every individual (that has a geographic
	#neighbor within the maximum distance), and its nearest genetic neighbor
	mins <- suppressWarnings(apply(specific.gen.dist.matrix, 2, min, na.rm=TRUE))
	
	#make a temporary table where you bind the minimum value onto the bottom of the table
	temp.table <- rbind(specific.gen.dist.matrix, mins)
	
	#use the compareMins utility function to compare the last element in a column to all other
	#elements in that column
	semifinal.matrix <- apply(temp.table, 2, compareMins)
	
	final.matrix <- semifinal.matrix
	
	#set all FALSE elements in the table to NA
	final.matrix[final.matrix==FALSE] <- NA
	
	#then identify who the most closely related individual is to every other individual
	closest.table <- which(!is.na(final.matrix), arr.ind=T)
	
	#subset this table to only those individuals that were in genetically clustered geographic neighborhoods
	final.table <- closest.table[closest.table[,2] %in% individuals.considered,]
	
	#randomly select half of the individuals to kill. BE SURE TO ONLY TAKE THE UNIQUE
	#individuals involved in each comparison, or your plots will get denser with time
	kill.list <- unique(sample(final.table[,1], size=(dim(final.table)[1])/2))

	new.arena <- arenaOutput$arena[-kill.list,]
	
	#create and return the output
	
	output <- list(no.killed=length(kill.list), regional.abundance=arenaOutput[[1]], arena=new.arena, dims=arenaOutput$dims)
	
	return(output)
}
