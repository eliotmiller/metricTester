#' Create landscapes with varying degrees of heterogeneity
#'
#' This function will simulate 3-dimensional landscapes of varying complexity.
#'
#' @param cells The number of cells to divide each side of the arena into. Larger values
#' provide smoother looking surfaces, but values larger than 100 can require too much RAM
#' to run.
#' @param seeds The number of "peaks" or trait "optima" that will be chosen in the
#' landscape. Default is 1.
#' @param exponent The exponent to which the distances will be raised. Default is 1.
#' Values larger than 1 have the effect of making distance decay slowly at first, then
#' drop off more quickly at the end, while values smaller than 1 have the effect
#' of dropping off quickly and then decreasing slowly.
#' @param cutoff Values below which distances from the focal cell will be converted to
#' zero. This operates after the exponent is applied to the distance matrix, and after the
#' distances specific to a given focal cell have been scaled to min 0 max 1. The default
#' cutoff is zero, meaning that all but the most distanct cells are still influenced by
#' the new optimum of the focal cell. Increasing this number towards 1 has the effect of
#' minimizing the distance over which the focal cell influences neighboring cells.
#' 
#' @details This function forms the guts of a new habitat filtering spatial simulation.
#' The output from the function is a square matrix with values corresponding, in my mind,
#' to optimum trait values for a location in 2d space. Alternatively, this might be useful
#' for simulations of elevational gradients. A good sequence to show how landscapes can be
#' varied might be (all with cells = 100 and exponent = 1) to change seeds from 1 to 2 to
#' 10 while holding cutoff at 0. Then change cutoff from 0.01 to 0.1 to 0.9 while holding
#' seeds at 10.
#'
#' @return A square matrix of dimensions cells x cells.
#'
#' @export
#'
#' @importFrom plotrix color2D.matplot
#' 
#' @references Miller, E. T. 2016. A new dispersal-informed null model for
#' community ecology shows strong performance. biorxiv.
#'
#' @examples
#' plotrix::color2D.matplot(varLandscape(10, seeds=1, exponent=1, cutoff=0),
#'	cs1=c(0.2,0.4,0.8), cs2=c(0,0.5,0.8), cs3=c(1,0.5,0), border=NA)

varLandscape <- function(cells, seeds=1, exponent=1, cutoff=0)
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
		
		#convert all distances below the cutoff to zero
		reverseDists[reverseDists < cutoff] <- 0

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
