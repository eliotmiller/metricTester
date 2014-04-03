#' Test for type I errors
#'
#' Sloppy function that needs work. Intended to test for type I and II errors of results
#' of testing of various metrics against a single spatial simulations.
#'
#' @param significance.results Data frame of significance results from call to sigTest()
#' @param expectation Expected value: 0=not significant, 1=clustered, 2=overdispersed
#' @param wrong Value of a typeI error rate, e.g. 2 if expecting 1.
#' 
#' @details Note that IAC is thought to detect clustering if observed is greater than
#' upper CIs, so we have to explicitly flip our expectations in the function. We have not
#' yet determined whether "wrong" could take c(1,2), and therefore test for type I error
#' rates if expecting not significant. Note that it is possible to have a type I
#' error irrespective of power of test, so a row can have more than one 1 in it. NEED TO MODIFY THIS FUNCTION TO BETTER DEAL WITH IAC. CURRENTLY, IF EXPECTING 0, EVEN IF ALL ZEROS, DOES NOT BEHAVE AS EXPECTED
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
#' system.time(allMetricsNull(tree=tree, orig.matrix=cdm, null.method="richness", no.randomizations=10, temp.file="output.csv"))
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
#' oneMetric <- sigTest(results, "PSV")
#'
#' #example of how to loop it over a table of results
#' metric.names <- names(observed)[3:21]
#'
#' sig.results <- list()
#'
#' for(i in 1:length(metric.names))
#' {
#'	sig.results[[i]] <- sigTest(results, metric.names[i])
#' }
#'
#' sig.results <- as.data.frame(sig.results)
#'
#' names(sig.results) <- metric.names
#'
#' error.summ <- typeI(sig.results, expectation=1, wrong=2)

typeI <- function(significance.results, expectation, wrong)
{
	#flip the values of IAC, because large values of this metric (2) correspond to
	#clustering and small (1) correspond to overdispersion. flip the 1s to 3s first so
	#you don't screw anything up
	significance.results$IAC[significance.results$IAC==1] <- 3
	significance.results$IAC[significance.results$IAC==2] <- 1
	significance.results$IAC[significance.results$IAC==3] <- 2
	
	#set up a blank matrix to save type I and II results into
	typeI.results <- matrix(nrow=dim(significance.results)[2], ncol=3)
	
	#name the blank matrix columns
	dimnames(typeI.results)[[2]] <- c("TypeI","NoSignal","Good")

	#go through each column of the normal style results (each metric) and see if any plots
	#deviate from expectation (=typeI), if fewer than half of plots show expectation,
	#or if at least half show the expectation
	for(i in 1:dim(significance.results)[2])
	{
		#give typeI errors to those for which this is true
		if(sum(significance.results[,i]==wrong) > 0)
		{
			typeI.results[i,1] <- 1
		}
		#give typeII errors to those for which this is true
		if(sum(significance.results[,i]==expectation)/dim(significance.results)[1] < 0.5)
		{
			typeI.results[i,2] <- 1
		}
		#give "good" to those for which this is true
		if(sum(significance.results[,i]==expectation)/dim(significance.results)[1] >= 0.5)
		{
			typeI.results[i,3] <- 1
		}
	}

	#bind the normal names
	dimnames(typeI.results)[[1]] <- names(significance.results)

	#set any comparisons that didn't work to 0
	typeI.results[is.na(typeI.results)]=0
	
	#will ultimately be able to delete this rearrangement of the output, but in order to
	#maintain easy backward compatibility with some cluster runs still running, pull the
	#IAC results out and bind them in at the end
	
	IAC <- typeI.results[rownames(typeI.results)=="IAC",]
	
	typeI.results <- typeI.results[rownames(typeI.results)!="IAC",]
	
	typeI.results <- rbind(typeI.results, IAC)
	
	return(typeI.results)
}
