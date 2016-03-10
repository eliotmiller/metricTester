#' Calculate alpha or beta metrics across a set of parameters, varying matrix richness
#'
#' Takes a specified set of tree size, shape, and vector of abundances, and takes a list
#' of richness vectors, and calculates the community structure metrics.
#'
#' @param alpha Whether to calculate alpha or beta phylogenetic community structure
#' metrics. Default is TRUE. Set to FALSE for beta metrics.
#' @param tree.size Number of species desired in the total tree.
#' @param richness.vectors List of vectors of/ Number of species to be placed in each
#' plot. See details.
#' @param delta A value for the delta transformation (Pagel 1999). Values greater than 1
#' push the branching events towards the root, while values less than 1 push the branching
#' events closer to the tips. See details for particularly low delta values.
#' @param abundances A vector of abundances. See examples.
#' @param beta.iterations Because the type of beta-level phylogenetic community structure
#' metrics used here return a single value per community data matrix, it is not possible
#' to look for inter-metric correlations with only a single matrix and tree. To deal with
#' this, the same tree can be used with different community data matrices. This argument
#' specifies the number of matrices to be used per tree. Not needed if alpha=TRUE.
#' @param iterations How many times to simulate the given set of parameters. For instance,
#' with a single tree size, richness.vector, delta, and two sets of abundances, and 10
#' iterations, the result will be a list with 10 elements. Each of those 10 elements will
#' be a list of two elements, each of which will be the calculated metrics for a given
#' set of parameters (one for each abundance vector).
#' @param cores Number of cores to be used for parallel processing. The iteration aspect
#' of the function is parallelized, so for efficiency purposes, it is best to run this
#' over numerous iterations rather than repeating the same parameter numerous times (e.g.,
#' rather than setting deltas to rep(1, 10), set delta to 1 and iterations to 10). 
#' 
#' @details richness.vectors is a list of vectors. See details in alphaMetricSims for
#' examples of the ways in which one of these vectors can be formatted. String these
#' vectors into a list for this function. If given a small value, e.g. 0.1, the delta
#' parameter
#' (tree shape) can occasionally result in oddly formatted trees that would cause errors.
#' To deal with this, there is an internal check that will recreate a new tree and
#' re-scale it with the desired delta. This has not been tested at delta < 0.1, and is
#' currently programmed with a while loop. Care should be taken not to get R stuck in an
#' indefinite loop at delta values even lower than 0.1
#'
#' @return A list of lists of data frames.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' system.time(vRichness <- varyRichness(alpha=TRUE, tree.size=50,
#'	richness.vectors=list(30:39, 40:49), delta=1,
#'	abundances=round(rlnorm(5000, meanlog=2, sdlog=1)) + 1, iterations=2, cores=1))

varyRichness <- function(alpha=TRUE, tree.size, richness.vectors, delta, abundances,
	beta.iterations, iterations, cores)
{
	if(!is.list(richness.vectors))
	{
		richness.vectors <- as.list(richness.vectors)
	}

	registerDoParallel(cores)

	if(alpha==TRUE)
	{
		results <- foreach(i = 1:iterations) %dopar%
		{
			lapply(seq_along(richness.vectors), function(x)
				alphaMetricSims(tree.size=tree.size, 
				richness.vector=richness.vectors[[x]], delta=delta, abundances))
		}
		for(i in 1:length(results))
		{
			names(results[[i]]) <- paste("richness.vector", 1:length(richness.vectors),
				sep="")
		}
	}
	else if(alpha==FALSE)
	{
		results <- foreach(i = 1:iterations) %dopar%
		{
			lapply(seq_along(richness.vectors), function(x)
				betaMetricSims(tree.size=tree.size, 
				richness.vector=richness.vectors[[x]], delta=delta, abundances,
				beta.iterations))
		}
		for(i in 1:length(results))
		{
			names(results[[i]]) <- paste("richness.vector", 1:length(richness.vectors),
				sep="")
		}
	}
	
	names(results) <- paste("iteration", 1:iterations, sep="")
	
	results
}
