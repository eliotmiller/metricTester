#' Identify failed runs
#'
#' This identifies spatial sim/null model runs that failed
#'
#' @param single.iteration The results of a single iteration of multiLinker
#' @param concat.by Whether randomizations were concatenated by richness, quadrat or both.
#'
#' @details It is possible that a given null model, e.g. regional, failed on a given run. 
#' These failures, in our experience so far, can only occur if a given species richness is
#' insufficiently sampled by the null model when concat.by="richness". Otherwise, I
#' believe the run will always succeed. Thus, to simplify coding, this function only
#' evaluates the by "richness" runs for failure when concat.by="both". If it is possible
#' for a null to fail if concatenating by quadrat then this function will need to be
#' re-written. The main point of this function it to identify failed runs and later
#' remove them from the results of a given iteration. This prevents errors further
#' down the line when summarizing results.
#'
#' @return A data frame summarizing which simulation/null runs failed.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- failed(results[[1]], "both")

failed <- function(single.iteration, concat.by)
{
	#it is really tough to properly go across each sub element within a single iteration
	#result with lapply, so just get in there with some for loops and brute force it.
	#First set up an empty matrix where we will save the dimensions of each null model
	#from each spatial simulation. If any of these have 0s, then something failed with
	#the null model. Similarly, the regional null very infrequently fails and throws NAs.
	#For these, if the sum of is.na of any the columns equals the length of the column
	#then there is a problem. So,
	#add a part at end of the for loop where even if the dimensions are ok, if the sum of
	#NAs equals the length then we say the dimensions are 0. This will allow easy 
	#identification later. The following dimensions assume that each spatial simulation 
	#was tested with the same null models and each has an element named ses.
	
	temp <- matrix(ncol=4, nrow=length(single.iteration) * 
		length(single.iteration[[1]]$ses))
	
	temp <- as.data.frame(temp)
	
	for(i in 1:length(single.iteration))
	{
		for(j in 1:length(single.iteration[[i]]$ses))
		{
			#define the length of the null models
			nullLength <- length(single.iteration[[i]]$ses)
			#now define the row ID as nullLength times i minus 1 + j
			rowID <- nullLength * (i - 1) + j
			#pull the relevant name for the spatial simulation
			temp[rowID,1] <- names(single.iteration)[i]
			#pull the relevant name for the null model
			temp[rowID,2] <- names(single.iteration[[i]]$ses)[j]
			#find the dimensions of the null model
			if(concat.by=="richness" | concat.by=="quadrat")
			{
				dims <- dim(single.iteration[[i]]$ses[[j]])
			}
			else if(concat.by=="both")
			{
				dims <- dim(single.iteration[[i]]$ses[[j]]$richness)
			}
			else
			{
				stop("concat.by must equal either both, richness, or quadrat")
			}
			#set the relevant row equal to the dimensions
			temp[rowID,3:4] <- dims
			#now add a cheap fix to skip to next iteration of loop if the dimensions have
			#0 or 1 or else the lengthNA double apply statement below will fail
			if(temp[rowID,3] <= 1)
			{
				next()
			}
			#otherwise go into the remainder of the for loop
			#determine the length of each column and of NAs per column
			if(concat.by=="richness" | concat.by=="quadrat")
			{
				lengthNA <- apply(apply(single.iteration[[i]]$ses[[j]], 2, is.na), 2, sum)
				lengthCol <- apply(single.iteration[[i]]$ses[[j]], 2, length)
			}
			else
			{
				lengthNA <- apply(apply(single.iteration[[i]]$ses[[j]]$richness, 
					2, is.na), 2, sum)
				lengthCol <- apply(single.iteration[[i]]$ses[[j]]$richness, 2, length)
			}
			#delete the element named "richness" from each of these
			lengthNA <- lengthNA[names(lengthNA) != "richness"]
			lengthCol <- lengthCol[names(lengthCol) != "richness"]
			#now if these things are ever equal there is a problem
			if(sum(lengthNA == lengthCol) > 0)
			{
				temp[rowID,3:4] <- 0
			}
		}
	}
	
	#set the column names to be nice for output
	names(temp) <- c("simulation", "null", "dim1", "dim2")

	#subset it to instances where dim1 == 0.
	temp <- temp[temp$dim1 == 0,]
	
	temp
	
}
