compareMins <- function(x)
{
	output <- x[1:(length(x)-1)] == x[length(x)]
	return(output)
}
