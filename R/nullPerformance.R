#' Summarize null model performance of a series of summarized simulation results
#'
#' Summarizes null performance after reading in and testing
#' simulation results with either sesIndiv or plotOverall.
#'
#' @param summarized.results The results of a call to sesIndiv() or plotOverall(). If from
#' plotOverall, the results must include the standard three spatial simulations (random,
#' filtering, competition), but must contain no other spatial simulations.
#' @param metrics By default, this function summarizes null performance over all tested
#' metrics. Alternatively, user can supply a vector of metric
#' names to summarize the results over.
#' @param concat.by Default is "both". Meaning that if both plot-level and richness-level
#' summary statistics are available, then performance will be average over both types of
#' results. Alternatively (and perhaps preferably), either "plot" or "richness" can be
#' provided and, assuming that concatenation option was specified in the multiLinker runs,
#' performance results will be summarized just over that specified option.
#' 
#' @details If an overall picture of null performance is desired, this function can
#' provide it. It can also be used to summarize null performance over a specific subset
#' of simulations, metrics, and concatenation options. If provided with the results
#' of a call to plotOverall, the options are more limited. Currently, if provided with
#' such a result, the assumption is
#' that there are three spatial simulations, "random", "filtering", and "competition". It
#' then assumes that any clustered or overdispersed plots for the random simulation,
#' or any overdispersed or clustered for the filtering or competition simulations,
#' respectively, count as typeI errors. It assumes that any plots that are not
#' clustered or overdispersed for the filtering or competition simulations, respectively,
#' count as typeII errors. 
#'
#' @return A data frame of summarized results
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
#' #examp <- nullPerformance(summ)

nullPerformance <- function(summarized.results, metrics, concat.by="both")
{
	#detect the names of the simulations that were run
	simulations <- unique(summarized.results$simulation)

	#if simulations is not equal to the standard filtering, random, competition options,
	#code is not currently able to accommodate results from plotOverall. throw an error.
	#note the names(summarized.results)[5] is just a quick way to figure out whether
	#the summarized.results object came from sesIndiv or plotOverall
	if(!setequal(simulations, c("random","filtering","competition")) &
		names(summarized.results)[5] == "clustered")
	{
		stop("nullPerformance not currently able to accommodate results from plotOverall that contain non-standard spatial simulations")
	}

	#if no metrics are specified, take the names of those to use from the results
	if(missing(metrics))
	{
		metrics <- unique(summarized.results$metric)
	}

	#confirm that the specified metrics match those in the results
	if(length(setdiff(metrics, unique(summarized.results$metric)) > 0))
	{
		stop("Some of the specified metrics do not match those in summarized.results")
	}

	#confirm that concat.by is set to a valid type. if both, just convert concat.by to
	#become c(richness, plot)
	if(concat.by=="both")
	{
		concat.by <- unique(summarized.results$concat.by)
	}
	#if set to richness, and if summarized.results contains a richness element, proceed
	else if(concat.by=="richness" & sum(summarized.results$concat.by=="richness") > 0)
	{
		concat.by <- concat.by
	}
	else if(concat.by=="plot" & sum(summarized.results$concat.by=="plot") > 0)
	{
		concat.by <- concat.by
	}
	else
	{
		stop("concat.by must be set to plot, richness, or both, and summarized.results must contain the specified type of summary")
	}

	#determine the null models that were used in summarized results
	nulls <- unique(summarized.results$null.model)

	#set up blank type i and ii vectors to save info into
	typeI <- c()
	typeII <- c()

	#determine whether the results came from sesIndiv (quick way to figure it out in the
	#if statement below), and, if so, calculate type I and II error rates in this way
	if(names(summarized.results)[5] == "total.runs")
	{
		for(i in 1:length(nulls))
		{
			temp <- summarized.results[summarized.results$null.model %in% nulls[i]
				& summarized.results$simulation %in% simulations
				& summarized.results$concat.by %in% concat.by
				& summarized.results$metric %in% metrics,]
			temp$typeIrate <- 100 * temp$typeI/temp$total.runs
			temp$typeIIrate <- 100 * temp$typeII/temp$total.runs
			typeI[i] <- mean(temp$typeIrate, na.rm=TRUE)
			typeII[i] <- mean(temp$typeIIrate, na.rm=TRUE)
		}
	}

	#determine whether the results came from plotOverall (quick way to figure it out in
	#if statement below), and, if so, calculate type I and II error rates in this way
	else if(names(summarized.results)[5] == "clustered")
	{
		#generate some simulation-specific data frames
		random <- summarized.results[summarized.results$metric %in% metrics
			& summarized.results$simulation == "random"
			& summarized.results$concat.by %in% concat.by
			& summarized.results$null.model %in% nulls,]
		filtering <- summarized.results[summarized.results$metric %in% metrics
			& summarized.results$simulation == "filtering"
			& summarized.results$concat.by %in% concat.by
			& summarized.results$null.model %in% nulls,]
		competition <- summarized.results[summarized.results$metric %in% metrics
			& summarized.results$simulation == "competition"
			& summarized.results$concat.by %in% concat.by
			& summarized.results$null.model %in% nulls,]

		#define typeI & II error rates for each of these
		random$typeIrate <- 100 *
			(random$clustered + random$overdispersed)/random$total.plots
		random$typeIIrate <- NA

		filtering$typeIrate <- 100 * filtering$overdispersed/filtering$total.plots
		#notice this important revision. the typeII rate is not simply the number of
		#plots that were not clustered. if the plot was overdispersed than it
		#already counted as a typeI error. revising here and below to incorporate that 
		filtering$typeIIrate <- 100 * 
			(filtering$total.plots - filtering$clustered -
			filtering$overdispersed)/filtering$total.plots

		competition$typeIrate <- 100 *
			competition$clustered/competition$total.plots
		competition$typeIIrate <- 100 * 
			(competition$total.plots -
			competition$overdispersed - competition$clustered)/competition$total.plots

		#redefine summarized.results
		summarized.results <- rbind(random, filtering, competition)

		for(i in 1:length(nulls))
		{
			temp <- summarized.results[summarized.results$null.model %in% nulls[i]
				& summarized.results$simulation %in% simulations
				& summarized.results$concat.by %in% concat.by
				& summarized.results$metric %in% metrics,]
			typeI[i] <- mean(temp$typeIrate, na.rm=TRUE)
			typeII[i] <- mean(temp$typeIIrate, na.rm=TRUE)
		}
	}

	results <- data.frame(nulls, typeI, typeII)
	results
}
