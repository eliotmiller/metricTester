#' Test for type I errors
#'
#' Sloppy function that needs work. Intended to test for type I and II errors of results
#' of testing of various metrics against a single spatial simulations.
#'
#' @param significance.results Data frame of significance results from call to sig.test()
#' @param expectation Expected value: 0=not significant, 1=clustered, 2=overdispersed
#' @param wrong Value of a typeI error rate, e.g. 2 if expecting 1.
#' 
#' @details Note that IAC is thought to detect clustering if observed is greater than
#' upper CIs, so we have to explicitly flip our expectations in the function. We have not
#' yet determined whether "wrong" could take c(1,2), and therefore test for type I error
#' rates if expecting not significant. Note that it is possible to have a type I
#' error irrespective of power of test, so a row can have more than one 1 in it.
#'
#' @return Matrix with rows corresponding to metrics, and columns for type I errors,
#' "NoSignal" (i.e. < 50% of communities from community data matrix exhibiting expected
#' pattern), and "Good" (i.e. > 50% of communities exhibiting expected pattern). Values in
#' table are 0s and 1s, where 1 corresponds to a confirmation of the pattern in question.
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
#'
#' error.summ <- typeI(sig.results, expectation=1, wrong=2)

typeI <- function(significance.results, expectation, wrong)
{
	normal <- significance.results[,names(significance.results)!="IAC"]

	typeI.results <- matrix(nrow=dim(significance.results)[2], ncol=3)

	dimnames(typeI.results)[[2]] <- c("TypeI","NoSignal","Good")

	for(i in 1:dim(normal)[2])
	{
		if(sum(normal[,i]==wrong) > 0)
		{
			typeI.results[i,1] <- 1
		}
		if(sum(normal[,i]==expectation)/dim(normal)[1] < 0.5)
		{
			typeI.results[i,2] <- 1
		}
		if(sum(normal[,i]==expectation)/dim(normal)[1] > 0.5)
		{
			typeI.results[i,3] <- 1
		}
	}

	if(sum(significance.results$IAC==expectation) > 0)
	{
		typeI.results[dim(significance.results)[2],1] <- 1
	}
	if(sum(significance.results$IAC==wrong)/dim(significance.results)[1] < 0.5)
	{
		typeI.results[dim(significance.results)[2],2] <- 1
	}
	if(sum(significance.results$IAC==wrong)/dim(significance.results)[1] > 0.5)
	{
		typeI.results[dim(significance.results)[2],3] <- 1
	}

	dimnames(typeI.results)[[1]] <- c(names(normal),"IAC")

	typeI.results[is.na(typeI.results)]=0
	
	return(typeI.results)
}
