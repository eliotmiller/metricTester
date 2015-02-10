#this is a simple R script that will ultimately make an .RDS file

prefix1 <- Sys.time()
prefix1 <- gsub(" ", "_", prefix1)
prefix1 <- gsub("[:]", "", prefix1)
prefix2 <- sample(round(runif(100, 1, 100)), size=1)
prefixBoth <- paste(prefix1, prefix2, sep="_")

temp <- rnorm(100)

filename <- paste(prefixBoth, ".RDS", sep="")

saveRDS(temp, file=filename)
