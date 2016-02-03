alphaMetricSims <- function(tree.size, richness.vector, delta, abundances)
{
	#simulate tree with birth-death process
	tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=tree.size)

	newTree <- rescale(tree, "delta", delta)

	ok <- is.ultrametric(newTree)

	#was having trouble with some trees and delta parameters making trees that were not
	#ultrametric and would throw errors when trying to use. this will make a new tree
	#if the first is not ultrametric
	while(ok==FALSE)
	{
		tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=tree.size)
		newTree <- rescale(tree, "delta", delta)
		ok <- is.ultrametric(newTree)
	}

	cdm <- simulateComm(newTree, richness.vector=richness.vector,
		abundances=abundances)

	prepped <- prepData(newTree, cdm)
	
	results <- calcMetrics(prepped)
	
	results
}

betaMetricSims <- function(tree.size, richness.vector, delta, abundances, beta.iterations)
{
	#simulate tree with birth-death process
	tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=tree.size)

	newTree <- rescale(tree, "delta", delta)

	ok <- is.ultrametric(newTree)

	#was having trouble with some trees and delta parameters making trees that were not
	#ultrametric and would throw errors when trying to use. this will make a new tree
	#if the first is not ultrametric
	while(ok==FALSE)
	{
		tree <- sim.bdtree(b=0.1, d=0, stop="taxa", n=tree.size)
		newTree <- rescale(tree, "delta", delta)
		ok <- is.ultrametric(newTree)
	}

	#results <- list()
	results <- matrix(nrow=beta.iterations, ncol=length(defineBetaMetrics()))
	
	for(i in 1:beta.iterations)
	{
		cdm <- simulateComm(newTree, richness.vector=richness.vector,
			abundances=abundances)
		prepped <- prepData(newTree, cdm)
		results[i,] <- as.matrix(calcBetaMetrics(prepped)[1,])
	}
	
	colnames(results) <- names(defineBetaMetrics())
	rownames(results) <- paste("beta.iteration", 1:beta.iterations, sep="")
	
	results
}

varyTreeSize <- function(alpha=TRUE, tree.sizes, richness.vector, delta, abundances,
	beta.iterations, iterations, cores)
{
	registerDoParallel(cores)

	if(alpha==TRUE)
	{
		results <- foreach(i = 1:iterations) %dopar%
		{
			lapply(seq_along(tree.sizes), function(x)
				alphaMetricSims(tree.size=tree.sizes[[x]], 
				richness.vector=richness.vector, delta=delta, abundances))
		}
		for(i in 1:length(results))
		{
			names(results[[i]]) <- paste("tree.size", 1:length(tree.sizes), sep="")
		}
	}
	else if(alpha==FALSE)
	{
		results <- foreach(i = 1:iterations) %dopar%
		{
			lapply(seq_along(tree.sizes), function(x)
				betaMetricSims(tree.size=tree.sizes[[x]], 
				richness.vector=richness.vector, delta=delta, abundances,
				beta.iterations))
		}
		for(i in 1:length(results))
		{
			names(results[[i]]) <- paste("tree.size", 1:length(tree.sizes), sep="")
		}
	}
	
	names(results) <- paste("iteration", 1:iterations, sep="")
	
	results
}

varyTreeShape <- function(alpha=TRUE, tree.size, richness.vector, deltas, abundances,
	beta.iterations)
{
	if(alpha)
	{
		results <- lapply(seq_along(deltas), function(x)
			alphaMetricSims(tree.size=tree.size, richness.vector=richness.vector,
			delta=deltas[[x]], abundances))
	}
	
	results
}

varyRichness <- function(alpha=TRUE, tree.size, richness.vectors, delta, abundances,
	beta.iterations)
{
	if(alpha)
	{
		results <- lapply(seq_along(richness.vectors), function(x)
			alphaMetricSims(tree.size=tree.size, richness.vector=richness.vectors[[x]],
			delta=delta, abundances))
	}
	
	results
}

varyAbundance <- function(alpha=TRUE, tree.size, richness.vector, delta, multi.abundances,
	beta.iterations)
{
	if(alpha)
	{
		results <- lapply(seq_along(multi.abundances), function(x)
			alphaMetricSims(tree.size=tree.size, richness.vector=richness.vector,
			delta=delta, abundances=multi.abundances[[x]]))
	}
	
	results
}
