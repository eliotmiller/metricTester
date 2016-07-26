#' Calculate phylogenetic and trait fields
#'
#' Given a prepped field.input object, calculate all fields of interest.
#'
#' @param field.input Prepped field.input object.
#' @param metrics Optional. If not provided, defines the metrics as all of those in
#' defineMetrics. If only a subset of those is desired, then metrics should take
#' the form of a character vector corresponding to named functions from defineMetrics.
#' The available metrics can be determined by running names(defineMetrics()). Otherwise,
#' if the user would like to define a new metric on the fly, the argument can take
#' the form of a named list of new functions (metrics).
#' 
#' @details Currently we are calculating 19 phylogenetic community structure metrics.
#' This function first confirms that the input is of class metrics.input and, if so, then
#' confirms that the metrics to be calculated are in a named list (via checkMetrics),
#' then lapplies all metric functions to the input metrics.input object.
#'
#' @return A data frame with the calculated metrics and the associated species richness
#' of all input "communities".
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' #simulate tree with birth-death process
#' tree <- geiger::sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm <- simulateComm(tree, richness.vector=10:25, abundances=sim.abundances)
#'
#' #in this example, occasionally some species are not in the CDM, so prune the tree
#' #accordingly so as not to throw any errors
#' tree <- drop.tip(tree, setdiff(tree$tip.label, colnames(cdm)))
#'
#' prepped <- prepFieldData(tree=tree, picante.cdm=cdm)
#'
#' results <- calcField(prepped)

calcField <- function(field.input, metrics)
{
	if(!inherits(field.input, "field.input"))
	{
		stop("Input needs to be of class 'field.input'")
	}
	
	#if a list of named metric functions is not passed in, assign metrics to be NULL, in
	#which case all metrics will be calculated
	if(missing(metrics))
	{
		metrics <- NULL
	}
		
	metrics <- checkMetrics(metrics)
	
	tempResults <- lapply(metrics, function(x) x(field.input))
	
	#convert the list to a data frame. do not add quadrat names like you do in calcMetrics
	cellResults <- as.data.frame(tempResults)
	
	#get rid of the richness column if it exists
	cellResults$richness <- NULL
	
	#go into a for loop that, for each metric i, determines if it is abundance-weighted or 
	#not. if non-abundance-weighted take a "weighted" mean where weights are either 0s or
	#1s. then in the inner for loop, for each species, take a weighted mean of the vector
	#of assemblage-specific metric values. if the species is absent, the weight will be
	#zero and it won't be included, so don't need to do any subsetting.
	results <- list()
	for(i in 1:dim(cellResults)[2])
	{
		#make a temporary vector of fields you will save the species' fields into
		temp <- c()
		
		for(j in 1:length(field.input$tree$tip.label))
		{
			if(names(cellResults)[i] %in% c("NAW_MPD", "NAW_MNTD", "PSV", "PSC", "PD",
				"PD_Cadotte"))
			{
				#derive a quick vector of presence-absence style weights so that if metric
				#is not abundance-weighted it treats a presence as a 1, otherwise a 0.
				#j elements are columns from the cdm, so it is taking the abundance of a
				#species across all the plots as a vector and setting those where it is
				#present to 1
				naWeights <- field.input$picante.cdm[,j]
				naWeights[naWeights > 0] <- 1

				#for a given species j, calculate the weighted mean of the cells it is
				#present in
				temp[j] <- weighted.mean(x=cellResults[,i], w=naWeights, na.rm=TRUE)
			}
			else if(names(cellResults)[i] %in% c("inter_MPD", "intra_MPD", "complete_MPD",
				"AW_MNTD", "PSE", "QE"))
			{
				temp[j] <- weighted.mean(x=cellResults[,i], 
					w=field.input$picante.cdm[,j], na.rm=TRUE)
			}
			else
			{
				stop("Cannot currently calculate field with new metrics")
			}
		}
		
		#set that metric equal to the vector of now-filled fields
		results[[i]] <- temp
	}
	
	#convert the list to a data frame and add the metric names
	results <- as.data.frame(results)
	names(results) <- names(cellResults)
	
	#add species names as its own column and return
	results <- cbind(species=colnames(field.input$picante.cdm), results)
	
	results
}
