tWrapApply <- function(dataframe)
{
	#apply tWrap over a data frame of metric SES scores for a given null and spatial sim
	output <- apply(dataframe, 2, tWrap)
	
	#transform the table, convert to a data frame, save the row names as an actual column,
	# exclude "richness" as a metric. output a data frame with three columns
	output <- t(output)

	#convert to data frame
	output <- as.data.frame(output)
	
	#add column names
	names(output) <- c("estimate", "p.value")
	
	#get rid of row names
	output$metric <- row.names(output)
	
	row.names(output) <- NULL
	
	output <- output[output$metric != "richness",]
	
	output
}
