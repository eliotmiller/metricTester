#' Create synthetic community niche space
#'
#' Create niche spaces with similar characteristics to input.
#'
#' @param ordination.results Total niche space, e.g. the $x element of a PCA object. This
#' should be a matrix. If it is not, it will be converted to one. This means categorical
#' variables will not be handled well.
#' @param road.map Identical to the input for the 'a' argument in the dbFD function of
#' the FD package, and to the picante.cdm argument used elsewhere in this package.
#' Thus, this is a matrix containing the abundance of each 'species' in
#' the ordination results. Rows are "sites" and columns are "species". Rather than
#' abundances, the values can simply be presence/absences. Moreover, sites could be
#' species and species could be individuals. See details and examples.
#' @param cov.matrix Optional covariance matrix, e.g. from a larger total trait space.
#' 
#' @details This function is totally untested and may not do what I think it does. Use
#' with care! In theory, it takes the points that describe the total trait space of a
#' community (or multiple communities, depending upon the analysis) and a community road
#' map, optionally a cov.matrix, and will return a new total trait space with the
#' same number of species (or sites) and observations per species (or community) as the
#' input. It works by first simulating a multivariate normal distribution with
#' centroids like those from ordination.results. It bases the positions of these centroids
#' on the covariance matrix of the input ordination.results unless a different covariance
#' matrix (e.g., from a larger trait space) is provided. Points (either akin to multiple
#' individuals or species, depending on the level of analysis) are then distributed around
#' those points with multivariate normal distributions like those in the input.
#'
#' @return Dataframe with the coordinates and species identities of the new niche space.
#'
#' @importFrom stats cov
#' @importFrom MASS mvrnorm
#'
#' @export
#'
#' @references Miller, E. T. 2016. Random thoughts, though please cite metricTester via
#' our 2016 Ecography paper.
#'
#' @examples
#' #generate six species' niche spaces of 20 observations each in 3 dimensions. first
#' #define the centroids of their distributions
#' centroids <- matrix(nrow=6, ncol=3, rep(seq(from = -2, to = 2, length.out=6), 3))
#'
#' #brute force the points into a list and reduce back into a large simulated
#' #ordination.results object
#'
#' output <- list()
#'
#' for(i in 1:6)
#' {
#'		temp1 <- rnorm(n=20, mean=centroids[i,1], sd=1/i)
#'		temp2 <- rnorm(n=20, mean=centroids[i,2], sd=1/i)
#'		temp3 <- rnorm(n=20, mean=centroids[i,3], sd=1/i)
#'		output[[i]] <- data.frame(temp1, temp2, temp3)
#' }
#'
#' totalNiche <- Reduce(rbind, output)
#'
#' #add a species then color column to totalNiche then scramble to simulate real data
#' totalNiche$species <- sort(rep(paste("species", 1:6, sep=""),20))
#'
#' toMerge <- data.frame(species=paste("species", 1:6, sep=""), color=1:6)
#'
#' totalNiche <- merge(totalNiche, toMerge)
#' 
#' totalNiche <- totalNiche[sample(row.names(totalNiche)),]
#'
#' #plot the points to give some sense of what it looks like (not run, but works)
#' #plot(totalNiche[,3]~totalNiche[,2], col=totalNiche$color, pch=20)
#'
#' #create a road map identifying which points belong to which species
#' roadMap <- matrix(nrow=6, ncol=120, 0)
#' 
#' row.names(roadMap) <- paste("species", 1:6, sep="")
#' colnames(roadMap) <- 1:120
#' roadMap[1,][row.names(totalNiche)[totalNiche$species=="species1"]] <- 1
#' roadMap[2,][row.names(totalNiche)[totalNiche$species=="species2"]] <- 1
#' roadMap[3,][row.names(totalNiche)[totalNiche$species=="species3"]] <- 1
#' roadMap[4,][row.names(totalNiche)[totalNiche$species=="species4"]] <- 1
#' roadMap[5,][row.names(totalNiche)[totalNiche$species=="species5"]] <- 1
#' roadMap[6,][row.names(totalNiche)[totalNiche$species=="species6"]] <- 1
#'
#' roadMap <- as.data.frame(roadMap)
#'
#' #now run the synthComm null model. exclude 1st and 5th columns since these are species
#' #names and color, which are not normal inputs
#' temp <- synthComm(totalNiche[,c(-1,-5)], roadMap)
#'
#' #plot the points to give some sense of what it looks like (not run, but works)
#' #plot(temp[,3]~temp[,2], col=temp$species, pch=20)

synthComm <- function(ordination.results, road.map, cov.matrix=NULL)
{
	#calculate each species' centroid, and the means of these centroids
	
	calcCenters <- centers(ordination.results, road.map)
	meanCenters <- apply(calcCenters, 2, mean)
	
	#if cov.matrix is missing, calculate it based on the species' centroids. otherwise
	#use the cov.matrix passed down (e.g. from a larger ordination space)

	if(is.null(cov.matrix))
	{
		cov.matrix <- stats::cov(calcCenters)
	}
	
	#now use the centroids and the cov.matrix to generate a multivariate normal
	#distribution of the requisite number of species' centroids. will use these as new
	#species' centroids

	newCenters <- MASS::mvrnorm(n=dim(calcCenters)[1], mu=meanCenters, Sigma=cov.matrix)
	
	#split community ordination into single species and calculate covariance matrix for
	#each, saving each matrix into an element of a list

	covList <- list()
	
	for(i in 1:dim(road.map)[1])
	{
		indices <- names(road.map)[road.map[i,]!=0]
		tempPoints <- ordination.results[indices,]
		covList[[i]] <- stats::cov(tempPoints)
	}
	
	#generate a multivariate normal distribution with these means and covariances for 
	#each species. VERY IMPORTANT. note that because we created a multivariate normal
	#distribution of new species' centroids, we are in effect randomizing the identity of
	#which new species gets which covariance structure, no need to randomize identities
	
	distributions <- list()
	
	for(i in 1:length(covList))
	{
		distributions[[i]] <- MASS::mvrnorm(n=sum(road.map[i,] != 0), mu=newCenters[i,],
			Sigma=covList[[i]])
	}

	#add species names to the new community observations.
	# first give each element of list a name, then rep it by the
	#length of that element. unlist into a character vector. then Reduce distributions
	#and cbind the species names in
	
	names(distributions) <- row.names(road.map)
	species <- unlist(lapply(seq_along(distributions), function(x) 
		rep(x=names(distributions[x]), times=dim(distributions[[x]])[1])))
	
	distributions <- Reduce(rbind, distributions)
	distributions <- as.data.frame(distributions)
	distributions <- cbind(species, distributions)
	distributions
}
