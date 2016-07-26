#' Summary statistics of SES results
#'
#' Summarizes individual iteration performance of each sim/null/metric combination across
#' all iterations.
#'
#' @param raw.results A list of lists of lists of dataframes; the results of a call to
#' readIn.
#' @param direction Character vector that needs to be provided if spatial simulations
#' beyond the standard "random", "filtering", and "competition" simulations are run.
#' The character vector must be the same length as the number of spatial simulations that
#' were run, and can take the possible values of "two.sided" (for a two-tailed test when
#' the SES scores are expected to be centered around 0), "less" (for when the observed SES
#' scores are expected to be less than 0), and "greater" (for when the observed SES scores
#' are expected to be greater than 0). For instance, habitat filtering would be set to
#' "less". The relevant simulation to which these directional tests will be applied can be
#' determined by calling names(raw.results[[1]]). Note that currently, even if a direction
#' vector is supplied, if the standard simulations were run then the supplied direction
#' vector will be replaced with the standard expectations.
#'
#' @details This function takes a raw list of results from multiple iterations from
#' multiLinker, and runs a non-exported function, sesSingle, over each element (iteration)
#' in that list. This sesSingle function runs a Wilcoxon signed rank test over each
#' iteration to determine whether the
#' plots from a spatial simulation/null/metric differ from
#' expectations. Assuming there are three spatial simulations named random, filtering, and
#' competition, this function will use two.sided, lesser and greater Wilcoxon tests,
#' respectively. If additional (or a limited set of)
#' spatial simulations are included, requiring modified expectations, these can be passed
#' along with the "direction" argument. It then summarizes the results of
#' those single run tests as the number of sim/null/metrics that deviated beyond
#' expectations and the number that were within expectations. A single run from a given
#' unique metric + null approach is considered as throwing a type I error only if p is
#' less than or equal to 0.05 for the random spatial simulation. It would be possible to
#' also assess whether such unique combinations throw the opposite signal than expected
#' for habitat filtering and competitive exclusion. A unique combination iteration is
#' considered to throw a type II error if the p value from  either the filtering or the
#' exclusion simulation is greater than 0.05. 
#'
#' @return A data frame summarizing the total number of runs per spatial simulation, null,
#' metric, concat.by combination, as well as the type I and II error rates of each such
#' unique combination. 
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' #not run
#' #results <- readIn()
#' #summ <- sesIndiv(results)

sesIndiv <- function(raw.results, direction)
{
	#lapply sesSingle across the list of results (one element per iteration)
	temp <- lapply(raw.results, sesSingle, direction)
	
	#reduce the list of results to one huge data frame
	temp <- Reduce(rbind, temp)
	
	#find the unique sim/null/metric combinations and turn into a temporary data frame
	#to loop over
	output <- unique(temp[,1:4])
	
	#set up an empty vector of Type I errors. as currently implemented, this will only
	#be triggered by runs where the random was considered significant. there are no TypeI
	#errors (currently) for filtering or competition. just TypeII. also set up an empty
	#vector to keep track of how many iterations there were for the given sim/null/metric
	
	totalRuns <- c()
	typeI <- c()
	typeII <- c()
	
	for(i in 1:dim(output)[1])
	{
		#this is a complicated subsetting operation. subset the reduced DF of results
		#(temp) down to instances where the sim, null, concat.by & metric match one
		#of the unique rows (i) from output
		temp2 <- temp[temp$simulation %in% output$simulation[i] & 
			temp$null.model %in% output$null[i] &
			temp$concat.by %in% output$concat.by[i] &
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
	output <- data.frame(output, total.runs=totalRuns, typeI, typeII)
	
	output
}

# Summary of results from a single iteration
#
# Use Wilcoxon signed rank test to determine whether plots from a SINGLE iteration 
# differ from expectations
#
# @param single.iteration The results of a single iteration from multiLinker.
# @param direction Character vector that needs to be provided if spatial simulations
# beyond the standard "random", "filtering", and "competition" simulations are run.
# The character vector must be the same length as the number of spatial simulations that
# were run, and can take the possible values of "two.sided" (for a two-tailed test when
# the SES scores are expected to be centered around 0), "less" (for when the observed SES
# scores are expected to be less than 0), and "greater" (for when the observed SES scores
# are expected to be greater than 0). For instance, habitat filtering would be set to
# "less". The relevant simulation to which these directional tests will be applied can be
# determined by calling names(single.iteration).
#
# @details This function uses a Wilcoxon signed rank test to determine whether the
# plots from a spatial simulation/null/metric from a SINGLE iteration differ from
# expectations. Assuming there are three spatial simulations named random, filtering, and
# competition, this function will use two.sided, lesser and greater Wilcoxon tests,
# respectively. If additional (or a limited set of)
# spatial simulations are included, requiring modified expectations, these can be passed
# along with the "direction" argument.
#
# @return A data frame summarizing the mean of standardized effect sizes and the
# significance of those devations from expectations for a given iteration (e.g. the 
# plots from a given arena). It does consider all spatial simulations, nulls and 
# metrics from that iteration. This test works across a single iteration, and will
# generally not be used by itself; it is called by sesIndiv.
#
# @examples
# #not run
# #results <- readIn()
# #summ <- sesSingle(results[[1]])

sesSingle <- function(single.iteration, direction)
{
	#dumb hack to pass R CMD check
	simulation <- "hack"
	null.model <- "hack"
	metric <- "hack"
	estimate <- "hack"
	p.value <- "hack"

	#determine whether the results were concatenated by plot, richness, or both. if by
	#both, then this will return true
	if(class(single.iteration[[1]][[1]][[1]])=="list")
	{
		concat.by <- "both"
	}
	
	#this will be a data frame and the first column will be named "richness" if
	#concatenated by that
	else if(is.data.frame(single.iteration[[1]][[1]][[1]]))
	{
		if(names(single.iteration[[1]][[1]][[1]])[1]=="richness")
		{
			concat.by <- "richness"
		}
		else if(names(single.iteration[[1]][[1]][[1]])[1]=="plot")
		{
			concat.by <- "plot"
		}
	}
	else
	{
		stop("Unexpected function input")
	}

	#identify any null model/spatial sim combos that failed. 
	problems <- failed(single.iteration, concat.by)
	
	#if there were any problems use a for loop to go through each failed run and set that 
	#element to NULL in the single.iteration results
	if(dim(problems)[1] > 0)
	{
		for(i in 1:dim(problems)[1])
		{
			sim <- problems[i,"simulation"]
			null <- problems[i,"null"]
			if(concat.by=="richness" | concat.by=="plot")
			{
				single.iteration[[sim]]$ses[[null]] <- NULL
			}
			else
			{
				single.iteration[[sim]]$ses[[null]]$richness <- NULL
			}
		}
	}

	#if the names of the spatial simulations are "random" "competition" and 
	#"filtering", create a character vector of expected alternative hypotheses to feed
	#into anonymous function below
	if(setequal(names(single.iteration), c("random","filtering","competition")))
	{
		direction <- names(single.iteration)
		direction[direction=="random"] <- "two.sided"
		direction[direction=="filtering"] <- "less"
		direction[direction=="competition"] <- "greater"
	}
	
	#if the names of the spatial simulations do not match the standard, direction needs to
	#be supplied as a character vector of same length as single.iteration
	else
	{
		if(length(direction) != length(single.iteration))
		{
			stop("direction must be a character vector of same length as number of spatial simulations")
		}
		
		if(length(setdiff(direction, c("two.sided", "less", "greater"))) > 0)
		{
			stop("possible directions are limited to two.sided, less, and greater")
		}
	}

	if(concat.by=="richness" | concat.by=="plot")
	{
		temp <- lapply(seq_along(direction), function(x)
			wilcoWrapLApply(single.iteration[[x]]$ses, alternative=direction[x]))

		#reduce the output list into a single data frame
		output <- Reduce(rbind, temp)
	
		#create a data frame of expanded simulation names
		simNames <- expand.grid(temp[[1]]$null.model, names(single.iteration))
	
		#give it names
		names(simNames) <- c("null", "simulation")
	
		#if there were any problems, take these names out of simNames
		if(dim(problems)[1] > 0)
		{
			simNames <- simNames[!(simNames$null %in% problems$null & 
				simNames$simulation %in% problems$simulation),]
		}
	
		#use just the second col from simNames
		output$simulation <- simNames[,2]
		
		#tack on concat names
		output$concat.by <- concat.by
	}
	
	else
	{
		tempAll <- list()
		
		#i refers to spatial simulations (you will only feed in $ses components)
		for(i in 1:length(direction))
		{
			#j refers to null models
			tempNull <- list()
			for(j in 1:length(single.iteration[[1]]$ses))
			{
				#k refers either to concatenating by richness or by plot
				for(k in 1:length(single.iteration[[1]]$ses[[1]]))
				{
					temp <- wilcoWrapLApply(single.iteration[[i]]$ses[[j]],
						alternative=direction[i])
					#this function was designed for use over a list of null models, so
					#change name here to reflect what it is actually being used for
					names(temp)[4] <- "concat.by"
				}
				#set the jth element of tempNull equal to the dataframe temp
				tempNull[[j]] <- temp
				#repeat the name of the null the correct length of the null model and bind
				tempNull[[j]]$null.model <- rep(names(single.iteration[[1]]$ses)[j],
					dim(tempNull[[j]])[1])
			}
			#set the ith element of tempAll equal to the list tempNull
			tempAll[[i]] <- tempNull
			#reduce the list down
			tempAll[[i]] <- Reduce(rbind, tempAll[[i]])
			#repeat the sim name the correct length and bind
			tempAll[[i]]$simulation <- rep(names(single.iteration)[i],
				dim(tempAll[[i]])[1])
		}
		#reduce the output list into a single data frame
		output <- Reduce(rbind, tempAll)	
	}
	
	#select the columns so they come out in a nice order
	output <- select(output, simulation, null.model, concat.by, metric,
		estimate, p.value)
	
	output
}
