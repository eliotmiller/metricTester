#' Batch read multiple csv files to list
#'
#' Read in all the files from a given directory and save each to a different element of a 
#' list.
#'
#' @param path The path of the directory containing the files to be read
#' 
#' @details This function reads in all the files from a given directory and stores each as
#' a separate element in a list. The names of the original files do not matter, but the
#' function assumes all to be comma-delimited files with the row names stored in the first
#' column and with each column having a name. Can modify this in the future if others find
#' it useful.
#'
#' @return A list with each file stored as a separate element.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' #path <- "/Users/eliotmiller/Desktop/delete"
#' 
#' #test <- readIn(path)
#' 
#' #output <- matrix(0, nrow=19, ncol=3)
#'
#' #for(i in 1:length(test))
#' #{
#' #	output <- output + test[[i]]
#' #}
#'
#' #output <- t(output)
#'
#' #barplot(output, beside=TRUE, las=2, cex.names=0.7, col=c("red","gray","blue"), xlab="Metric", ylab="Count")
#'
#' #legend(x=1, y=98, c("Type1","No signal","Good"), fill=c("red","gray","blue"))

readIn <- function(path)
{
	results <- list()
	files <- list.files(path)
	setwd(path)
	for(i in 1:length(files))
	{
		results[[i]] <- read.table(files[i], header=TRUE, row.names=1, sep=",")
	}
	return(results)
}
