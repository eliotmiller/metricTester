betaErrorWrapper <- function(working.directory)
{
	temp <- readIn(working.directory)
	checkedList <- lapply(temp, betaErrorChecker)
	results <- Reduce(function(y, z) mapply(rbind, y, z, SIMPLIFY=F),
		checkedList, accumulate=F)
	checkedList
}

#temp2 <- Reduce(function(y, z) mapply(rbind, y, z, SIMPLIFY=F), lapply(temp, unlist, recursive=F), accumulate=F)

#temp3 <- lapply(seq_along(temp2), function(x) apply(temp2[[x]], 2, sum))

#names(temp3) <- names(temp2)

#t(as.data.frame(temp3))
