nullPerformance <- function(summarized.results, simulations="all", metrics="all",
	concat.by="both")
{
	if(simulations=="all")
	{
		simulations <- unique(summarized.results$simulation)
	}
	#complicated if statement here. it says if the specified simulations do not contain
	#an entry of character "all" and there is any difference between the specified sims
	#and the unique simulations in the summarized.results table, throw an error
	else if(all(!(simulations %in% "all")) &
		length(setdiff(simulations, unique(summarized.results$simulation))) > 0)
	{
		stop("Specified simulations do not match those in the results table")
	}
	else
	{
		simulations <- simulations
	}

	if(concat.by=="both")
	{
		concat.by <- unique(summarized.results$concat.by)
	}
	else if(all(!(concat.by %in% "both")) &
		length(setdiff(concat.by, unique(summarized.results$concat.by))) > 0)
	{
		stop("concat.by must be set to quadrat, richness, or both")
	}
	else
	{
		concat.by <- concat.by
	}

	if(metrics=="all")
	{
		metrics <- unique(summarized.results$metric)
	}
	else if(all(!(metrics %in% "all")) &
		length(setdiff(metrics, unique(summarized.results$metric))) > 0)
	{
		stop("Specified metrics do not match those in the results table")
	}
	else
	{
		metrics <- metrics
	}

	nulls <- unique(summarized.results$null.model)
	typeI <- c()
	typeII <- c()
	for(i in 1:length(nulls))
	{
		temp <- summarized.results[summarized.results$null.model %in% nulls[i]
			& summarized.results$simulation %in% simulations
			& summarized.results$concat.by %in% concat.by
			& summarized.results$metric %in% metrics,]
		typeI[i] <- mean(temp$typeIrate, na.rm=TRUE)
		typeII[i] <- mean(temp$typeIIrate, na.rm=TRUE)
	}
	results <- data.frame(nulls, typeI, typeII)
	results
}
