#' Test significance of observed metrics
#'
#' Given a table of results, where the expected confidence intervals are bound to the rows
#' of observed scores, and the name of the metric of interest, returns a vector of 0, 1 
#' and 2, where 0=not significant, 1=clustered, and 2=overdispersed.
#'
#' @param results.table Data frame of observed metrics with expected CIs bound in. See
#' example
#' @param observed Name of metric of interest
#' 
#' @details The column names need to be fairly carefully labeled, so follow convention.
#'
#' @return A vector of 0s 1s and 2s, corresponding to not significant, clustered and
#' overdispersed.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(plyr)
#' library(geiger)
#' library(picante)
#'
#' #simulate tree with birth-death process
#' tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=50)
#'
#' sim.abundances <- round(rlnorm(5000, meanlog=2, sdlog=1))
#'
#' cdm <- simulateComm(tree, min.rich=10, max.rich=25, abundances=sim.abundances)
#'
#' system.time(allMetricsNull.csv(tree, cdm, "richness", 10, "output.csv"))
#'
#' possibilities <- read.csv("output.csv")
#'
#' #call the summaries function from within a ddply statement
#' expectations <- ddply(possibilities, .(richness), summaries)
#'
#' #calculate the observed metrics
#' observed <- allMetrics(tree, cdm)
#'
#' #important merge command, confirm it works
#' results <- merge(observed, expectations, sort=FALSE)
#'
#' oneMetric <- sig.test(results, "PSV")
#'
#' #example of how to loop it over a table of results
#' metric.names <- names(observed)[2:20]
#'
#' sig.results <- list()
#'
#' for(i in 1:length(metric.names))
#' {
#'	sig.results[[i]] <- sig.test(results, metric.names[i])
#' }
#'
#' sig.results <- as.data.frame(sig.results)
#'
#' names(sig.results) <- metric.names

sig.test <- function(results.table, observed)
{
	upper.name <- paste(observed, "upper", sep=".")
	lower.name <- paste(observed, "lower", sep=".")
	upper <- results.table[,upper.name]
	lower <- results.table[,lower.name]
	overdispersed <- results.table[,observed] > upper
	overdispersed[overdispersed==TRUE] <- 2
	clustered <- results.table[,observed] < lower
	clustered[overdispersed==TRUE] <- 1
	significance <- overdispersed + clustered
	return(significance)
}
