#' Randomly place quadrats in arena
#'
#' Given a desired number of quadrats, the arena size, and the quadrat size, will attempt
#' to place quadrats down in a non-overlapping fashion
#'
#' @param no.quadrats Number of quadrats to place
#' @param x.max Maximum x bounds of arena
#' @param y.max Maximum y bounds of arena
#' @param quadrat_size Size of desired quadrat
#' 
#' @details Places quadrats down in non-overlapping fashion according to parameters
#' supplied. Will run indefinitely if unacceptable parameters are supplied, but will not
#' crash.
#'
#' @return A matrix with the X & Y coordinates of the four corners of each quadrat placed
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#'
#' bounds <- quadratPlacer(no.quadrats=15, x.max=300, y.max=300, quadrat_size=50)

quadratPlacer <- function(no.quadrats, x.max, y.max, quadrat_size)
{

	##define quadrat bounds, etc

	quadrat_bounds <- matrix(0,nrow=no.quadrats,ncol=4)
	colnames(quadrat_bounds) <- c("X1","X2","Y1","Y2")

	for (i in c(1:no.quadrats))
	{
		repeat 
		{
			OK <- TRUE
			quadrat_bounds[i,1] <- sample(c(0:(x.max-quadrat_size)),1)
			quadrat_bounds[i,2] <- quadrat_bounds[i,1] + quadrat_size
			quadrat_bounds[i,3] <- sample(c(0:(y.max-quadrat_size)),1)
			quadrat_bounds[i,4] <- quadrat_bounds[i,3] + quadrat_size
			if (i > 1) 
			{
				for (j in c(1:(i-1)))
				{
					if (any(
						quadrat_bounds[i,1] %in% c(quadrat_bounds[j,1]:quadrat_bounds[j,2]) & quadrat_bounds[i,3] %in% c(quadrat_bounds[j,3]:quadrat_bounds[j,4]),
						quadrat_bounds[i,2] %in% c(quadrat_bounds[j,1]:quadrat_bounds[j,2]) & quadrat_bounds[i,3] %in% c(quadrat_bounds[j,3]:quadrat_bounds[j,4]),
						quadrat_bounds[i,1] %in% c(quadrat_bounds[j,1]:quadrat_bounds[j,2]) & quadrat_bounds[i,4] %in% c(quadrat_bounds[j,3]:quadrat_bounds[j,4]),
						quadrat_bounds[i,2] %in% c(quadrat_bounds[j,1]:quadrat_bounds[j,2]) & quadrat_bounds[i,4] %in% c(quadrat_bounds[j,3]:quadrat_bounds[j,4])
						)) 
					{
						OK <- FALSE
					}
				}
			}
			if (OK == TRUE) 
			{
				break;
			}
		}
	}
	return(quadrat_bounds)
}
