#' Generate expectations for null+metric combinations
#'
#' Will generate mean, SD and CI expectations for the desired metric + null combinations.
#'
#' @param picante.cdm A picante-style community data matrix with sites as rows, and
#' species as columns
#' @param tree Phylo object
#' @param optional.dists A symmetric distance matrix can be directly supplied. This option
#' is experimental. Behavior depends on metric being used. If the metric in question
#' relies on the phylogenetic distance matrix from a call to cophenetic(tree), then this 
#' optional distance matrix will be inserted instead.
#' @param regional.abundance A character vector in the form "s1, s1, s1, s2, s2, s3, etc".
#' Optional, will be generated from the input CDM if not provided.
#' @param distances.among A symmetric distance matrix, summarizing the distances among all
#' plots from the cdm. Optional, only used by some null models.
#' @param randomizations The number of times the input CDM should be randomized and the
#' metrics calculated across it.
#' @param cores This function can run in parallel. In order to do so, the user must
#' specify the desired number of cores to utilize.
#' @param metrics Optional list of named metric functions to use. If invoked, this option
#' will likely be used to run a subset of the defined metrics.
#' @param nulls Optional list of named null model functions to use. If invoked, this 
#' option will likely be used to run a subset of the defined null models.
#' @param concat.by Whether to concatenate the randomizations by richness, plot or both
#' @param output.raw Default is FALSE. Set to TRUE if raw randomized values are preferred
#' (as opposed to summarized mean, SD, CI, etc).
#'
#' @details Given a list of desired metrics (which should always include richness) and
#' null models, will generate the expected mean, standard deviation and confidence
#' intervals based on the number of specified randomizations. This function is flexible in
#' that new metrics and nulls can be added and tested with it. By setting output.raw to
#' TRUE, the function can also output raw randomized values as opposed to the summarized
#' values.
#'
#' @return A list of data frames, where each data frame corresponds to a given null model,
#' and contains the mean, SD, and CI for each metric for that null model.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' #set up a matrix to simulate lat/long
#' coordDF <- matrix(ncol=2, nrow=100)
#'
#' coordDF[,1] <- runif(n=100, min=40, max=50)
#' coordDF[,2] <- runif(n=100, min=-130, max=-120)
#'
#' #convert to data frame, give column names. also give row names such as if the cells had
#' #names (as they should or there'd be no way to track them)
#' coordDF <- as.data.frame(coordDF)
#'
#' row.names(coordDF) <- paste("cell", 1:100, sep="")
#'
#' names(coordDF) <- c("latitude","longitude")
#'
#' #calculate the distances among all of these points. in the real program you're going to
#' #want to calculate great arc distance or whatever it's called
#' distances <- dist(coordDF, diag=TRUE, upper=TRUE)
#'
#' #turn it into a symmetric distance matrix
#' distances <- as.matrix(distances)
#'
#' #simulate a regional phylogeny of 100 species
#' tree <- geiger::sim.bdtree(b=1, d=0, stop="taxa", n=100)
#'
#' #simulate a community data matrix of 100 cells by 100 species. do it 4 times so that
#' #you can use your simulateComm function and have it span a reasonable range of richness
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' cdm1 <- simulateComm(tree, richness.vector=10:34, abundances=sim.abundances)
#' cdm2 <- simulateComm(tree, richness.vector=10:34, abundances=sim.abundances)
#' cdm3 <- simulateComm(tree, richness.vector=10:34, abundances=sim.abundances)
#' cdm4 <- simulateComm(tree, richness.vector=10:34, abundances=sim.abundances)
#'
#' #bind these into a list and use dplyr rbind_all to bind together. recast as data frame
#'
#' cdmList <- list(cdm1, cdm2, cdm3, cdm4)
#'
#' cdm <- dplyr::rbind_all(cdmList)
#'
#' cdm <- as.data.frame(cdm)
#'
#' #fix as necessary manually here (i.e. make sure dimensions are 100 x 100), seems to 
#' #usually work. then give cell names
#'
#' row.names(cdm) <- paste("cell", 1:100, sep="")
#'
#' #fill NAs with 0s.
#'
#' cdm[is.na(cdm)] <- 0
#'
#' test <- expectations(picante.cdm=cdm, tree=tree, optional.dists=NULL,
#'	regional.abundance=NULL, distances.among=distances, randomizations=3, cores=1,
#'	nulls=list("richness"=metricTester:::my_richnessNull), 
#'	metrics=list("richness"=metricTester:::my_richness, "NAW_MPD"=metricTester:::naw_mpd),
#'	concat.by="both", output.raw=FALSE)
#'
#' #an example of how to explore behavior of a new metric in the metricTester framework
#' #this "metric" simply calculates the richness of each plot in the CDM
#' exampleMetric <- function(metrics.input)
#' {
#'	output <- apply(metrics.input$picante.cdm, 1, lengthNonZeros)
#'	output
#' }
#'
#' test2 <- expectations(picante.cdm=cdm, tree=tree, optional.dists=NULL,
#'	regional.abundance=NULL, distances.among=distances, randomizations=3, cores=1,
#'	nulls=list("frequency"=metricTester:::my_frequency), 
#'	metrics=list("richness"=metricTester:::my_richness, "exampleMetric"=exampleMetric),
#'	concat.by="both", output.raw=FALSE)

expectations <- function(picante.cdm, tree, optional.dists=NULL, regional.abundance=NULL, 
	distances.among=NULL, randomizations, cores, metrics, nulls, concat.by,
	output.raw=FALSE)
{
	#calculate the raw randomized results across whatever metrics and nulls are called
	rawResults <- metricsNnulls(picante.cdm=picante.cdm, tree=tree, 
		optional.dists=optional.dists, regional.abundance=regional.abundance, 
		distances.among=distances.among, randomizations=randomizations, cores=cores,
		metrics=metrics, nulls=nulls)
	#reduce these randomizations into more managable results
	results <- reduceRandomizations(rawResults)
	#if the user wants the raw data, provide that
	if(output.raw==TRUE)
	{
		return(results)
	}
	#otherwise return the summarized data
	else
	{
		output <- lapply(results, summaries, concat.by=concat.by)
		return(output)
	}
}
