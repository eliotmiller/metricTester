#' Return average and CIs of input vector
#'
#' Given a vector of numbers, such as a column from a data frame of null expectations,
#' returns the average and 95 percent CIs of that vector
#'
#' @param null.output Vector of numbers
#' 
#' @details Took out the call to iterations, but if you want that back it's just 
#' the length of v. Note that it's very important when you run this to have your null 
#' output be a file that has one column called richness, and all others be various metrics
#' you want confidence intervals returned for. Note also that this function must be used 
#' from within a ddply statement in order to work as desired, see example.
#'
#' @return temp
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
#' system.time(allMetricsNull(tree=tree, orig.matrix=cdm, null.method="richness", 
#' no.randomizations=10, temp.file="output.csv"))
#'
#' possibilities <- read.csv("output.csv")
#'
#' #call the summaries function from within a ddply statement
#' expectations <- ddply(possibilities, .(richness), summaries)

summaries <- function(null.output)
{
    # Create the output variable names
	metricNames <- names(null.output)[names(null.output)!="richness" & names(null.output)!="quadratNames"]
	summary.names <- c("average", "sd", "lower", "upper")
	combo.names <- paste(rep(metricNames, each = length(summary.names)), rep(summary.names, length(metricNames)), sep = ".")

	# Write a confidence interval function to be able to each variable
	CI <- function(x)
	{
		c(mean(x, na.rm = TRUE), sd(x, na.rm=TRUE), quantile(x, c(0.025, 0.975), na.rm = TRUE))
	}

	# Convert the numeric part of the data frame to matrix
    # and then apply the function g to each column
    L <- apply(data.matrix(null.output[, metricNames]), 2, CI)
    
    # String the matrix in column order to a vector (that means basically run through the whole matrix creating a long vector as you go)
    # add combo names to it, then return. 
    v <- as.vector(L)
    names(v) <- combo.names
    return(v)
}
