#' Summary statistics of SES results
#'
#' Summarizes individual iteration performance of each sim/null/metric combination across
#' all iterations
#'
#' @param raw.results A list of lists of lists of dataframes; the results of a call to
#' readIn.
#'
#' @details This function takes a raw list of results from multiple iterations from
#' multiLinker, and runs sesSingle across each one. It then summarizes the results of
#' those single runs as the number of sim/null/metrics that deviated beyond expectations
#' and the number that were within expectations.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- sesIndiv(results)

sesIndiv <- function(raw.results)
{
	#lapply sesSingle across the list of results (one element per iteration)
	temp <- lapply(raw.results, sesSingle)
	
	#reduce the list of results to one huge data frame
	temp <- Reduce(rbind, temp)
	
	#find the unique sim/null/metric combinations and turn into a temporary data frame
	#to loop over
	output <- unique(temp[,1:3])
	
	#set up an empty vector of Type I errors. as currently implemented, this will only
	#be triggered by runs where the random was considered significant. there are no TypeI
	#errors (currently) for filtering or competition. just TypeII. also set up an empty
	#vector to keep track of how many iterations there were for the given sim/null/metric
	
	totalRuns <- c()
	typeI <- c()
	typeII <- c()
	
	for(i in 1:dim(output)[1])
	{
		temp2 <- temp[temp$simulation %in% output$simulation[i] & 
			temp$null %in% output$null[i] &
			temp$metric %in% output$metric[i], ]
		totalRuns[i] <- dim(temp2)[1]
		if(output$simulation[i] == "random")
		{
			typeII[i] <- NA
			typeI[i] <- dim(temp2[temp2$p.value <= 0.05,])[1]
		}
		else
		{
			typeI[i] <- NA
			typeII[i] <- dim(temp2[temp2$p.value > 0.05,])[1]
		}
	}
	
	#bind the three vectors into the output and send out
	output <- data.frame(output, totalRuns, typeI, typeII)
	
	output
}
