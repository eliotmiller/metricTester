tWrap <- function(vect, mu=0)
{
	#set up a blank matrix to save results into
	output <- matrix(nrow=1, ncol=2)
	
	#run a quick t.test on the vector
	temp <- t.test(x=vect, mu=mu)
	
	#pull out the observed mean and p.value from temp and retain these
	output[1,1] <- temp$estimate
	output[1,2] <- temp$p.value
	
	output
}
