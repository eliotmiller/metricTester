#' Output all beta metrics as a list of named functions
#'
#' Creates a list of named functions, each of which accept a metrics.input object
#'
#' @details All of the beta metrics we calculated for our manuscript are included in this
#' function. To add additional functions, they can either be defined on the fly or, to 
#' permanently include a new metric in all downstream simulations, it can be included
#' here. The function needs to be included with a name, and it must accept a metrics.input
#' as input. If the function needs additional elements not included in that input, then
#' the prepData function must also be revised.
#'
#' @return A list of named functions
#'
#' @export
#'
#' @import phylobase grid ecoPDcorr
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2015. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' bioRxiv 025726.
#'
#' @examples
#' defineBetaMetrics()

defineBetaMetrics <- function()
{
	list("richness"=my_betaRichness,
	"total_abundance"=my_totalAbundance,
	"Ist"=my_Ist,
	"Pst"=my_Pst,
	"Bst"=my_Bst,
	"PIst"=my_PIst,
	"Ist_unpruned"=my_Ist_unpruned,
	"Pst_unpruned"=my_Pst_unpruned,
	"Bst_unpruned"=my_Bst_unpruned,
	"PIst_unpruned"=my_PIst_unpruned,
	"meanNAW_MPD"=mean_naw_mpd,
	"meanInter_MPD"=mean_inter_mpd,
	"meanIntra_MPD"=mean_intra_mpd,
	"meanComplete_MPD"=mean_complete_mpd,
	"meanNAW_MNTD"=mean_naw_mntd,
	"meanAW_MNTD"=mean_aw_mntd
	)
}

my_betaRichness <- function(metrics.input)
	sum(apply(metrics.input$picante.cdm, 2, lengthNonZeros) != 0)

my_totalAbundance <- function(metrics.input)
	sum(metrics.input$picante.cdm)

#should consider adding a skewness function here. need to check, but I believe the formula
#may just be (3(mean-median))/standard deviation

my_Ist <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree)$Ist

my_Pst <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree)$Pst

my_Bst <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree)$Bst

my_PIst <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree)$PIst

my_Ist_unpruned <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree,
	prune=FALSE)$Ist

my_Pst_unpruned <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree,
	prune=FALSE)$Pst

my_Bst_unpruned <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree,
	prune=FALSE)$Bst

my_PIst_unpruned <- function(metrics.input)
	spacodi.calc(sp.plot=t(metrics.input$picante.cdm), phy=metrics.input$tree,
	prune=FALSE)$PIst

mean_naw_mpd <- function(metrics.input)
	mean(modifiedMPD(metrics.input$picante.cdm, metrics.input$dists,
	abundance.weighted=FALSE))

mean_inter_mpd <- function(metrics.input)
	mean(modifiedMPD(metrics.input$picante.cdm, metrics.input$dists,
	abundance.weighted="interspecific"))

mean_intra_mpd <- function(metrics.input)
	mean(modifiedMPD(metrics.input$picante.cdm, metrics.input$dists,
	abundance.weighted="intraspecific"))

mean_complete_mpd <- function(metrics.input)
	mean(modifiedMPD(metrics.input$picante.cdm, metrics.input$dists,
	abundance.weighted="complete"))

mean_naw_mntd <- function(metrics.input)
	mean(mntd(metrics.input$picante.cdm, metrics.input$dists,
	abundance.weighted=FALSE))

mean_aw_mntd <- function(metrics.input)
	mean(mntd(metrics.input$picante.cdm, metrics.input$dists,
	abundance.weighted=TRUE))
