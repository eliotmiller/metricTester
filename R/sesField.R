#' Calculate a species' standardized trait field
#'
#' Calculate the null-model standardized effect size of a species' trait field.
#'
#' @param field.input Prepped field.input object.
#' @param metrics Optional list of named metric functions to use. If invoked, this option
#' will likely be used to run a subset of the defined metrics.
#' @param nulls Optional list of named null model functions to use. If invoked, this 
#' option will likely be used to run a subset of the defined null models.
#' @param randomizations The number of times the input CDM should be randomized and the
#' metrics calculated across it.
#' @param regional.abundance A character vector in the form "s1, s1, s1, s2, s2, s3, etc".
#' Optional, will be generated from the input CDM if not provided.
#' @param distances.among A symmetric distance matrix, summarizing the distances among all
#' quadrats from the cdm. For use with the dispersal null.
#' @param cores This function can run in parallel. In order to do so, the user must
#' specify the desired number of cores to utilize. The default is "seq", which runs the
#' calculations sequentially.
#' 
#' @details The trait distance matrix should be symmetrical and "complete". See example.
#' Currently only non-abundance-weighted mean pairwise and interspecific
#' abundance-weighted mean pairwise phylogenetic distances are implemented. The
#' only null models that are currently implemented are the richness and dispersal nulls.
#' The function could be improved by tapping into any of the metrics and nulls defined
#' in defineMetrics and defineNulls.
#'
#' @return Data frame of standardized effect sizes of species' trait fields. Table
#' includes the observed trait field, the mean and standard deviation of the species'
#' trait field after randomization with the chosen null model, and the resulting
#' species-specific standarized effect size.
#'
#' @export
#'
#' @importFrom dplyr summarize_each funs
#'
#' @references Miller, Wagner, Harmon & Ricklefs. In review. Radiating despite a lack of
#' character: closely related, morphologically similar, co-occurring honeyeaters have
#' diverged ecologically.
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
#' results <- sesField(prepped, randomizations=3,
#'	metrics=list("NAW_MPD"=metricTester:::naw_mpd),
#'	nulls=list("richness"=metricTester:::my_richnessNull))

sesField <- function(field.input, metrics, nulls, randomizations, regional.abundance,
	distances.among, cores="seq")
{
	#dplyr will cause R CMD check to throw a note if you are summarizing based on
	#variables it does not know about. here is a stupid hack to avoid that problem.
	species <- "hack"
	. <- "hack"

	#set various things to NULL if they are missing
	if(missing(metrics))
	{
		metrics <- NULL
	}
	if(missing(nulls))
	{
		nulls <- NULL
	}
	if(missing(regional.abundance))
	{
		regional.abundance <- NULL
	}
	if(missing(distances.among))
	{
		distances.among <- NULL
	}

	#calculate the observed fields
	observed <- calcField(field.input=field.input, metrics=metrics)

	#THIS IS A DANGEROUS STEP. SORT OBSERVED BY SPECIES NAMES. THE REASON TO DO THIS IS
	#THAT THE DPLYR FUNCTIONS BELOW SORT ALSO, SO THIS THEORETICALLY ALLOWS YOU TO JUST
	#BIND COLUMNS OF RESULTS TOGETHER. SHOULD ALWAYS CHECK THIS IS ACTUALLY DOING WHAT
	#YOU THINK IT IS. A CAREFUL MERGE STATEMENT LATER WOULD BE PREFERABLE
	observed <- observed[order(observed$species),]
	
	#prep the inputs for null randomizations
	nullsPrepped <- prepNulls(field.input$tree, field.input$picante.cdm,
		regional.abundance, distances.among)

	#if cores is seq, run calculations sequentially	
	if(cores == "seq")
	{
		#warn that the analysis is being run sequentially
		warning("Not running analysis in parallel. See 'cores' argument.", call.=FALSE)

		#call the parallel for loop. each iteration, save a new list of lists, where each
		#inner element are the fields calculated for a given null model
		randomResults <- foreach(i = 1:randomizations) %do%
		{
			#run the nulls across the prepped data. this randomizes the CDMs all at once
			randomMatrices <- runNulls(nullsPrepped, nulls)
			#prep the randomized CDMs to calculate the field metrics across them. if
			#field.input$field is phylo, prep accordingly, else prep for trait field
			if(field.input$field == "phylo")
			{
				randomPrepped <- lapply(randomMatrices, function(x) 
					prepFieldData(tree=field.input$tree, picante.cdm=x))
			}
			else if(field.input$field == "trait")
			{
				randomPrepped <- lapply(randomMatrices, function(x) 
					prepFieldData(dists=field.input$dists, picante.cdm=x))
			}
			
			#calculate the field
			lapply(randomPrepped, calcField, metrics)
		}
	}

	#if cores is seq, run calculations sequentially	
	if(cores != "seq")
	{
		registerDoParallel(cores)

		#call the parallel for loop. each iteration, save a new list of lists, where each
		#inner element are the fields calculated for a given null model
		randomResults <- foreach(i = 1:randomizations) %dopar%
		{
			#run the nulls across the prepped data. this randomizes the CDMs all at once
			randomMatrices <- runNulls(nullsPrepped, nulls)
			#prep the randomized CDMs to calculate the field metrics across them. if
			#field.input$field is phylo, prep accordingly, else prep for trait field
			if(field.input$field == "phylo")
			{
				randomPrepped <- lapply(randomMatrices, function(x) 
					prepFieldData(tree=field.input$tree, picante.cdm=x))
			}
			else if(field.input$field == "trait")
			{
				randomPrepped <- lapply(randomMatrices, function(x) 
					prepFieldData(dists=field.input$dists, picante.cdm=x))
			}
			
			#calculate the field
			lapply(randomPrepped, calcField, metrics)
		}
	}

	#reduce the random results down
	reduced <- reduceRandomizations(randomResults)

	#lapply the dplyr group_by function and group by species
	grouped <- lapply(reduced, dplyr::group_by, species)
	
	#then use the dplyr summarize each and funs functions to get the means and sds, and
	#lapply this whole thing over the list of grouped data frames
	means <- lapply(seq_along(grouped), function(x) dplyr::summarize_each(grouped[[x]],
		dplyr::funs(mean(., na.rm=TRUE))))
	sds <- lapply(seq_along(grouped), function(x) dplyr::summarize_each(grouped[[x]],
		dplyr::funs(sd(., na.rm=TRUE))))
	
	#now go into an ugly nested for loop that for each null model i will take all species
	#scores for a given metric j and calculate the SES
	results <- list()

	#each i represents the results from a single null model
	for(i in 1:length(means))
	{
		#each j represents the results from a single metric. by starting at 2, you skip
		#the first column, which are the species. set up a matrix to save SES results into
		#where there are as many rows as species, and there are as many columns as
		#metrics. set the first column equal to the species names, so convert to a DF
		singleResult <- matrix(nrow=dim(means[[i]])[1], ncol=dim(means[[i]])[2])
		singleResult <- as.data.frame(singleResult)
		singleResult[,1] <- observed$species
		
		for(j in 2:dim(means[[i]])[2])
		{
			#set up an empty matrix for easy calculations where each row represents
			#a species, and there are four columns. first is observed, second is
			#mean, third is SD, and fourth will be used to calculate SES
			temp <- matrix(nrow=dim(means[[i]])[1], ncol=4)
			temp <- data.frame(temp)
			temp[,1] <- observed[,j]
			temp[,2] <- means[[i]][,j]
			temp[,3] <- sds[[i]][,j]
			temp[,4] <- (temp[,1]-temp[,2])/temp[,3]
			singleResult[,j] <- temp[,4]
		}

		#set the names of singleResult right, then set it as a single result in results
		names(singleResult) <- names(means[[i]])
		results[[i]] <- singleResult
	}
	
	#set the names of results and return
	names(results) <- names(reduced)
	results
}
