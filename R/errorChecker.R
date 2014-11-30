#' Wrapper for summarizing randomizations and testing significance of observed metrics
#'
#' Given a data frame of observed scores and a list of randomizations based on different
#' null models, returns a list of data frames summarizing the significance of observed 
#' scores both at the single quadrat and the entire arena level.
#'
#' @param observed Data frame of observed metric scores, such as from observedMetrics()
#' @param reduced.randomizations List of random, reduced results, such as those from
#' reduceRandomizations()
#' @param concat.by Whether to concatenate the randomizations by richness or quadrat
#' 
#' @details This function wraps a number of smaller functions into a useful type I and II
#' error checker. It takes a reduced list of randomizations such as those reduced from
#' metricsNnulls with reduceRandomizations, summarizes the mean,
#' SD, and CI of each metric plus null model either at the richness or quadrat level,
#' then compares the observed metric scores to those summarized scores. It return a list
#' with two elements. The first is a list of data frames, where each corresponds to the 
#' standardized effect scores of the observed metrics for a given null model. The second
#' is a list of data frames, where each corresponds to whether each observed metric
#' deviates beyond expectations.
#'
#' @return A list of lists of data frames.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(dplyr)
#' library(geiger)
#' library(picante)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' #simulate a log normal abundance distribution
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1)) + 1
#'
#' #simulate a community of varying richness
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)
#'
#' #run the metrics and nulls combo function
#' rawResults <- metricsNnulls(tree, cdm, randomizations=3)
#'
#' #calculate the observed metrics from the input CDM
#' observed <- observedMetrics(tree, cdm)
#'
#' test <- errorChecker(observed, rawResults, "richness")

errorChecker <- function(observed, reduced.randomizations, concat.by)
{
	summarized <- lapply(reduced.randomizations, summaries, concat.by)
	#this is an important command. depending on what you concatenated by, there should be
	#only a single matching column betweeen the tables (either richness or quadrat), and
	#so it should merge on that. there are not currently any checks for missing values
	#e.g. with regional null, so need to build something in soon
	merged <- lapply(summarized, merge, observed)
	#this will return a list of data frames, one for each null model, where the first col
	#is whatever we summarized on, and each successive column is the SES of the observed
	#score based on the randomizations
	sesResults <- lapply(1:length(merged), function(x) arenaTest(merged[[x]], concat.by))
	names(sesResults) <- names(merged)
	#this will return a list of data frames, one for each null model, where the first col
	#is whatever we summarized on, and each successive column is an indicator of whether
	#the observed score in a quadrat was bigger or lesser
	quadratResults <- lapply(1:length(merged), function(x) quadratTest(merged[[x]], 
		concat.by))
	names(quadratResults) <- names(merged)
	results <- list("ses"=sesResults, "quadrat"=quadratResults)
	results
}
