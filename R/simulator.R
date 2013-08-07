#' Test metrics across multiple arenas
#'
#' Sloppy function that needs work. Intended to be a large function tying many previous
#' functions together into a single simulator function generating spatial arenas, sampling
#' quadrats, generating null expectations, testing for significance of observed metrics,
#' and summarizing results as a matrix of type I and type II error rates.
#'
#' @param no.species Number of species in each arena
#' @param x_min Minimum X coordinate of arena, e.g. 0
#' @param x_max Maximum X coordinate of arena
#' @param y_min Minimum Y coordinate of arena, e.g. 0
#' @param y_max Maximum Y coordinate of arena
#' @param no.quadrats Number of quadrats to sample
#' @param quadrat_size Size of an individual quadrat
#' @param mean.log.individuals Mean log of abundance vector from which species abundances
#' will be drawn
#' @param length.parameter Length of vector from which species' locations are drawn. Large
#' values of this parameter dramatically decrease the speed of the function but result in
#' nicer looking communities
#' @param sd.parameter Standard deviation of vector from which species' locations are 
#' drawn
#' @param null.method A picante-style null, e.g. "richness" or "frequency"
#' @param no.randomizations Number of iterations the function should run, i.e. the
#' number of times the orig.matrix will be shuffled and the metrics calculated on it
#' @param expectation Expected value: 0=not significant, 1=clustered, 2=overdispersed
#' @param wrong Value of a typeI error rate, e.g. 2 if expecting 1.
#' @param no.metrics Need to specify how many metrics are being tested
#' @param iterations Number of arenas to simulate and test
#' @param temp.file File name of output file where null metric values are saved to.
#' Re-written each iteration
#' @param output.file File name of results file
#' 
#' @details Could easily modify this function to save more information than it currently
#' does, though obviously beware the additional space such an operation might require.
#' A single null.csv file for 19 metrics by 1000 iterations is about ~50 megabytes. The
#' results matrix is also written to csv in case the function crashes part-way through.
#'
#' @return Two csvs and a matrix of results summarizing the type I and type II errors
#' across all metrics and spatial simulations. One csv is just a temporary file storing
#' the null expectations, the other is a csv of the same thing as the output matrix.
#'
#' @export
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(ape)
#' library(geiger)
#' library(colorRamps)
#' library(plyr)
#' library(picante)
#'
#' simulator(no.species=50, x_min=0, x_max=300, y_min=0, y_max=300, no.quadrats=15, 
#' quadrat_size=50, mean.log.individuals=4, length.parameter=5000, sd.parameter=50, 
#' null.method="richness", no.randomizations=2, expectation=1, wrong=2, no.metrics=19, 
#' iterations=3, temp.file="deleteme.csv", output.file="confused.csv")

simulator <- function(no.species, x_min, x_max, y_min, y_max, no.quadrats, quadrat_size, mean.log.individuals, length.parameter, sd.parameter, null.method, no.randomizations, expectation, wrong, no.metrics, iterations, temp.file, output.file)
{
	output <- matrix(0, nrow=no.metrics, ncol=3)

	for(i in 1:iterations)
	{
		print(paste("iteration",i))

		temp <- phyloNtraits(no.species)

		tree <- temp[[1]]

		scaled <- scaler(temp[[2]], x_min, x_max)

		##with mean.log.individuals=4, length.parameter=5000, and sd.parameter=50, we're somewhere
		##near 3800 individuals

		positions <- locationSampler(temp, mean.log.individuals=mean.log.individuals, scaled, length.parameter=length.parameter, sd.parameter=sd.parameter)

		phydistmatrix <- cophenetic(tree)

		##define a color for each species

		cols <- blue2green2red(nrow(phydistmatrix))

		##plot the arena. don't close the window. currently hashed out

		plot(positions$X, positions$Y, pch=20, cex=0.5, xlim=c(0,x_max), ylim=c(0,y_max), col=cols[positions$individuals])

		bounds <- quadratPlacer(no.quadrats, x_max=x_max, y_max=y_max, quadrat_size=quadrat_size)

		quadratPlotter(bounds)

		com.results <- quadratContents(positions, bounds)
		
		if(com.results[1]==FALSE)
		{
			next
		}

		cdm <- t(com.results)

		##the ecoPD functions shit the bed if there isn't a name for each quadrat

		quadratNames <- paste("quadrat",1:no.quadrats, sep="")

		dimnames(cdm)[[1]] <- quadratNames

		##call the allMetricsNull.csv() function

		allMetricsNull.csv(orig.matrix=cdm, tree=tree, null.method=null.method, iterations=no.randomizations, file.name=temp.file)

		##read the simulations in 

		simulations <- read.csv("deleteme.csv")

		##call the summaries function from within a ddply statement

		summ.results <- ddply(simulations, .(richness), summaries)

		##calculate the observed metrics

		observed <- allMetrics(tree, cdm)

		results <- merge(observed, summ.results, sort=FALSE)

		metric.names <- names(observed)[2:20]

		sig.results <- list()

		for(j in 1:length(metric.names))
		{
			sig.results[[j]] <- sig.test(results, metric.names[j])
		}

		sig.results <- as.data.frame(sig.results)

		names(sig.results) <- metric.names

		output <- output + typeI(sig.results, expectation=expectation, wrong=wrong)

		write.csv(output, file=output.file)
	}
	
	return(output)
}
