#' Calculate functional dispersion (FDis)
#'
#' Calculate the functional dispersion of clouds of multivariate points.
#'
#' @param ordination.results Matrix of ordination results, e.g. the $x element from a
#' prcomp object, or the $points element from a metaMDS object in vegan. Could also be
#' raw trait values, but cannot currently handle categorical variables.
#' @param road.map Identical to the input for the 'a' argument in the dbFD function of
#' the FD package, and to the picante.cdm argument used elsewhere in this package.
#' Thus, this is a matrix or data frame containing the abundance of each 'species' in
#' the ordination results. Rows are "sites" and columns are "species". Rather than
#' abundances, the values can simply be presence/absences. Moreover, sites could be
#' species and species could be individuals. See details.
#' 
#' @details The definition of FDis provided by Laliberte and Legendre (2010) and
#' implemented in the dbFD function of the FD package is geared towards calculating the
#' functional diversity of a community, given a set of species with an array of traits.
#' Another useful way in which FDis might be implemented is as a measure of a species'
#' niche breadth. In this case, ordination.results would be measures (e.g.
#' foraging observations) of multiple individuals of multiple species', and road.map would
#' be a matrix describing which observations belong to which species. The abundances in
#' the matrix in this case would describe how much weight to assign to each individual
#' observation. Regardless of the scale of calculation (either across species within
#' a community or across individuals within a species), this function determines the
#' weighted centroid of each cloud of points and then determines the weighted mean
#' absolute deviation from each centroid.
#'
#' @return Named numeric with FDis values.
#'
#' @export
#'
#' @references Miller, E. T. 2016. Random thoughts, though please cite metricTester via
#' our 2016 Ecography paper.
#'
#' Laliberte, E. & P. Legendre. 2010. A distance-based framework for measuring functional
#' diversity from multiple traits. Ecology 91:299-305.
#'
#' @examples
#' #example of how to calculate the FDis of a series of plots based on the trait values
#' #of a set of species. begin by simulating a phylogeny with a birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #create a log-normal abundance distribution
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' #simulate a community data matrix, with species as columns and sites as rows
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' #simulate two traits and combine into a matrix. because species are sometimes absent
#' #from the cdm, also exclude any species from the trait data frame that are not in the
#' #cdm (to avoid errors), then ordinate with a PCA
#' traits <- evolveTraits(tree)[[2]]
#' traits <- traits[row.names(traits) %in% colnames(cdm),]
#' ord <- prcomp(traits)
#' 
#' #the FDis of the species in each plot
#' FDis(ordination.results=ord$x, road.map=cdm)
#'
#' #now an example of how to calculate the FDis of a series of species based on the trait
#' #values of a set of individuals. begin by simulating trait data for a series of
#' #individuals. to illustrate the point, simulate varying numbers of individuals per
#' #species, and where there are varying degrees of variance in traits per species. 
#' traits2 <- data.frame(trait1=c(rnorm(n=30, sd=1), rnorm(n=60, sd=2), rnorm(n=120,sd=4)),
#'	trait2=c(rnorm(n=30, sd=1), rnorm(n=60, sd=2), rnorm(n=120, sd=4)))
#'
#' #ordinate the traits. could readily use another ordination here, e.g. nmds with gower
#' ord2 <- prcomp(traits2)
#'
#' #create a road.map where species are rows and individual observations are columns. the
#' #first 30 observations belong to sp1, the following 60 to sp2, and the following 120
#' #to sp3.
#' cdm2 <- matrix(nrow=3, ncol=dim(traits2)[1], 0)
#' colnames(cdm2) <- row.names(traits2)
#' row.names(cdm2) <- c("sp1", "sp2", "sp3")
#' cdm2[1,1:30] <- 1
#' cdm2[2,31:90] <- 1
#' cdm2[3,91:210] <- 1
#'
#' #the FDis of each species (i.e. niche breadth)
#' FDis(ordination.results=ord2$x, road.map=cdm2)

FDis <- function(ordination.results, road.map)
{	
	#the function expects ordination.results to be a matrix. if provided with a data.frame
	#convert to matrix.
	if(is.matrix(ordination.results) != TRUE)
	{
		warning("Converting ordination.results to matrix. Ensure this is ok for your data.")
		ordination.results <- as.matrix(ordination.results)
	}
	
	results <- c()

	centerPoints <- centers(ordination.results, road.map)

	temp <- rbind(ordination.results, centerPoints)
	allDistances <- dist(temp, diag=TRUE, upper=TRUE)
	distMatrix <- as.matrix(allDistances)
	
	for(i in 1:dim(road.map)[1])
	{
		#figure out which column corresponds to distances from the weighted centroids
		#pull it out and get rid of the final elements (distances among centroids)
		centroidDists <- distMatrix[,dim(road.map)[2]+i]
		centroidDists <- centroidDists[1:(length(centroidDists)-(dim(road.map)[1]))]
		#now calculate the weighted mean distance from this centroid
		results[i] <- weighted.mean(centroidDists, road.map[i,])
	}
	
	names(results) <- row.names(road.map)

	return(results)
}
