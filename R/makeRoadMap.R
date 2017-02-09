#' Create a road map for use in FDis-related functions
#'
#' Based on an ordination space, convert into the road.map required by the FDis-related
#' functions, and by the dbFD function in the FDis package.
#'
#' @param ordination.results data.frame summarizing the location of the points in the
#' ordination over which to calculate FDis. For example, the $x object of a call to
#' prcomp. Formatting of this ordination.results object is currently strict. It must be a
#' data frame, it must not have row.names, and it must have a column called 'species'.
#'
#' @details This is currently a basic utility function with very little flexibility. It
#' assumes you have a data.frame without row.names and a column called species. If you
#' would like to calculate plot-level FDis, sensu Laliberte & Legendre, you will still
#' need to have a column named species. See examples for more details.
#' This function will create the object called 'a'
#' used in the dbFD function of the FDis package. However, it gives all points equal
#' weighting, i.e., the returned object will only contain 0s and 1s.
#'
#' @return A data.frame summarizing which points in the ordination space "belong" to
#' which species or plots over which to calculate FDis.
#'
#' @export
#'
#' @references Miller, E. T. 2016. Random thoughts, though please cite metricTester via
#' our 2016 Ecography paper.
#'
#' @examples
#' #simulate trait data for a series of individuals. to illustrate the point, simulate
#' #varying numbers of individuals per
#' #species, and where there are varying degrees of variance in traits per species. 
#' traits <- data.frame(trait1=c(rnorm(n=30, sd=1), rnorm(n=60, sd=2), rnorm(n=120,sd=4)),
#' trait2=c(rnorm(n=30, sd=1), rnorm(n=60, sd=2), rnorm(n=120, sd=4)))
#'
#' #ordinate those trait data
#' ord <- prcomp(traits)
#'
#' #pull out the positions of the points in the ordination space, and specify which
#' #species those individuals belong to
#' ex <- data.frame(ord$x)
#' ex$species <- c(rep("species1", 30), rep("species2",30), rep("species3",150))
#'
#' test <- makeRoadMap(ex)

makeRoadMap <- function(ordination.results)
{
	#check whether the row.names of ordination space go from 1 to the number of rows. if
	#not, throw an error
	if(!all(row.names(ordination.results)==1:dim(ordination.results)[1]))
	{
		stop("Your ordination.results has row.names. Remove these before proceeding.")
	}
	
	#define the species in the ordination space
	spp <- unique(ordination.results$species)
	
	#set up a blank road map
	roadMap <- matrix(nrow=length(spp), ncol=dim(ordination.results)[1], 0)
	
	#give it row and column names
	row.names(roadMap) <- spp
	colnames(roadMap) <- 1:dim(ordination.results)[1]
	
	for(i in 1:length(spp))
	{
		roadMap[i,][row.names(ordination.results)[ordination.results$species==spp[i]]] <- 1
	}
	
	roadMap <- as.data.frame(roadMap)
	
	roadMap
}
