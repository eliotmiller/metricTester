#' Fill in soon
#'
#' Fill in later
#'
#' @details Fill in later
#'
#' @return Fill in later
#'
#' @export
#'
#' @import phylobase grid ecoPDcorr
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' test <- expectations(cdm, tree, regional.abundance=NULL, distances.among=distances, 
#'	randomizations=3, cores=3, cluster=FALSE, 
#'	nulls=list("dispersal"=metricTester:::my_dispersal), 
#'	metrics=list("richness"=metricTester:::my_richness, "NAW_MPD"=metricTester:::naw_mpd),
#'	concat.by="both", output.raw=FALSE)

expectations <- function(picante.cdm, tree, regional.abundance=NULL, distances.among, 
	randomizations, cores, cluster=FALSE, metrics, nulls, concat.by, output.raw=FALSE)
{
	#calculate the raw randomized results across whatever metrics and nulls are called
	rawResults <- metricsNnulls(picante.cdm=cdm, tree=tree, 
		regional.abundance=regional.abundance, distances.among=distances.among, 
		randomizations=randomizations, cores=cores, cluster=cluster, metrics=metrics, 
		nulls=nulls)
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
