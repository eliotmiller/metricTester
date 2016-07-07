#' New spatial sim guts
#'
#' Longer description
#'
#' @param cells The number of cells to divide each side of the arena into.
#' @param exponent The exponent to which the distances will be raised. Default is 1.
#' values larger than 1 have the effect of , while values smaller than 1 have the effect
#' of .
#' @param seeds The number of "peaks" or trait "optima" that will be chosen in the
#' landscape. Default is 1.
#' 
#' @details More details
#'
#' @return A matrix with more details
#'
#' @export
#'
#' @references Miller, E. T. 2016. A new dispersal-informed null model for
#' community ecology shows strong performance. biorxiv.

varLandscape <- function(cells, exponent=1, seeds=1)
{
	#create a trait landscape. it is a square matrix, with sides equal to cells,
	#and is initially populated with zeros 
	landscape <- matrix(nrow=cells, ncol=cells, 0)

	#find the x,y coordinates of each cell in the input matrix. although the landscape
	#matrix is currently coded as a square, this should be robust to rectangluar matrices
	#this takes each column as a position along the x axis
	xDim <- dim(landscape)[2]
	#this takes each row as a position along the y axis
	yDim <- dim(landscape)[1]
	
	#use expand.grid to create an xy data frame
	xyDF <- expand.grid(1:xDim, 1:yDim)
	names(xyDF) <- c("x","y")

	#find all pairwise distances between all cells
	allDists <- as.matrix(dist(xyDF, diag=TRUE, upper=TRUE))
	#raise these distances to the specified exponent (default is 1)
	allDists <- allDists^exponent
	
	#now go into a for loop of length seeds, where each iteration it randomly selects a 
	#grid cell in the matrix, finds a new optimum trait and corresponding cell, creates a 
	#matrix of same size as landscape, but filled with the optimum value, then finds the
	#distances between the focal cell and all other cells, and uses 1-these distances to
	#take a weighted average so that cells are more influenced the closer they are to
	#the focal cell
	for(i in 1:seeds)
	{
		#sample a random cell from xyDF
		focal <- xyDF[sample(1:dim(xyDF)[1], size=1),]
		
		#find a new trait optimum by sampling from a uniform distribution between -1 & 1
		optimum <- runif(n=1, min=-1, max=1)
		
		#create a matrix of same size as landscape but filled with optimum trait value
		optMatrix <- matrix(nrow=cells, ncol=cells, optimum)

		#use the row name of the focal cell to find the relevant distances 
		focalDists <- allDists[row.names(focal),]
		
		#scale these distances to min 0, max 1, then subtract the distances from 1
		reverseDists <- 1-(focalDists - min(focalDists))/(max(focalDists)-min(focalDists))
		
		#stack these distances into a matrix of the same dimensions as the other matrices
		distMatrix <- matrix(nrow=cells, ncol=cells, reverseDists, byrow=TRUE)
		
		#also create a reversed distance matrix for the weighted mean below
		reversedMatrix <- 1-distMatrix
		
		#take a weighted average of the landscape matrix and the optimum matrix using
		#the distMatrix as the weight. importantly, multiply the weights by 2 so that the
		#focal cell gets the optimum value
		landscape <- (landscape * reversedMatrix + optMatrix * distMatrix)/(reversedMatrix + distMatrix)
	}

	#return the now-modified landscape matrix
	landscape
}
