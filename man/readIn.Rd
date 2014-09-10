\name{readIn}
\alias{readIn}
\title{Batch read multiple csv files to list}
\usage{
readIn(path, row.names = TRUE)
}
\arguments{
  \item{path}{The path of the directory containing the
  files to be read}

  \item{row.names}{Do the files to be read in have row
  names? Default is yes. If not, set this argument to
  FALSE.}
}
\value{
A list with each file stored as a separate element.
}
\description{
Read in all the files from a given directory and save each
to a different element of a list.
}
\details{
This function reads in all the files from a given directory
and stores each as a separate element in a list. The names
of the original files do not matter, but the function
assumes all to be comma-delimited files with the row names
stored in the first column and with each column having a
name. Can modify this in the future if others find it
useful.
}
\examples{
#path <- "/Users/eliotmiller/Desktop/delete"

#test <- readIn(path)

#output <- matrix(0, nrow=19, ncol=3)

#for(i in 1:length(test))
#{
#	output <- output + test[[i]]
#}

#output <- t(output)

#ordr <- c("IAC","Haed","PD","PD_Cadotte","Hed","Eed","AW_MNTD","NAW_MNTD","PSC","PAE",
"Eaed","NAW_MPD","PSV","inter_MPD","PSE","intra_MPD","QE","complete_MPD","SimpsonsPhy")

#output <- output[,ordr]

#dimnames(output)[[2]] <- gsub("_", " ", dimnames(output)[[2]])

#quartz(height=6, width=12)

#par(mar=c(6.6,4.1,4.1,2.1))

#barplot(output, beside=TRUE, las=2, cex.names=1, col=c("red","gray","blue"),
xlab="Metric", ylab="Count")

#legend(x=65, y=85, c("Type I error","Type II error","Successful"), fill=c("red","gray","blue"))
}
\references{
Miller, Trisos and Farine.
}

