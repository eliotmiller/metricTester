overallSES <- function(summarized.results)
{
	#define your metrics you are interested (exclude richness)
	toTest <- names(defineMetrics())
	toTest <- toTest[toTest != "richness"]

	#set up a blank matrix to save results into, and convert into data frame for ease
	output <- matrix(ncol=5, nrow=(length(defineMetrics())-1) * 
		length(defineNulls()) * length(defineSimulations()))
	
	output <- as.data.frame(output)
	
	#give the data frame appropriate column names
	names(output) <- c("simulation", "null", "metric", "mean.ses", "p.value")
	
	#one level of i for each spatial simulation
	for(i in 1:length(summarized.results$ses))
	{
		#one level of j for each null model
		for(j in 1:length(summarized.results$ses[[1]]))
		{
			#one level of k for each metric
			for(k in 1:length(toTest))
			{
				print((i * j) + (k * 19) - 18)
			}
		}
	}

}
