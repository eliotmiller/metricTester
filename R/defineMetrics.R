#' Output all metrics as a list of named functions
#'
#' Creates a list of named functions, each of which accept a metrics.input object
#'
#' @details All of the functions we calculated for our manuscript are included in this
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
#' defineMetrics()

defineMetrics <- function()
{
	list("richness"=my_richness,
	"NAW_MPD"=naw_mpd,
	"inter_MPD"=inter_mpd,
	"intra_MPD"=intra_mpd,
	"complete_MPD"=complete_mpd,
	"NAW_MNTD"=naw_mntd,
	"AW_MNTD"=aw_mntd,
	"PSV"=my_psv,
	"PSC"=my_psc,
	"PSE"=my_pse,
	"PAE"=PAE,
	"IAC"=IAC,
	"Haed"=Haed,
	"Eaed"=Eaed,
	"Eed"=Eed,
	"Hed"=Hed,
	"SimpsonsPhy"=SimpsonsPhy,
	"PD"=my_PD,
	"PD_Cadotte"=my_PD_Cadotte,
	"QE"=my_QE
	)
}

my_richness <- function(metrics.input)
	apply(metrics.input$picante.cdm, 1, lengthNonZeros)

naw_mpd <- function(metrics.input)
	modifiedMPD(metrics.input$picante.cdm, metrics.input$dists, abundance.weighted=FALSE)

inter_mpd <- function(metrics.input)
	modifiedMPD(metrics.input$picante.cdm, metrics.input$dists, abundance.weighted="interspecific")

intra_mpd <- function(metrics.input)
	modifiedMPD(metrics.input$picante.cdm, metrics.input$dists, abundance.weighted="intraspecific")

complete_mpd <- function(metrics.input)
	modifiedMPD(metrics.input$picante.cdm, metrics.input$dists, abundance.weighted="complete")

naw_mntd <- function(metrics.input)
	mntd(metrics.input$picante.cdm, metrics.input$dists, abundance.weighted=FALSE)

aw_mntd <- function(metrics.input)
	mntd(metrics.input$picante.cdm, metrics.input$dists, abundance.weighted=TRUE)

my_psv <- function(metrics.input)
{
	PSV <- psv(metrics.input$picante.cdm, metrics.input$tree)
	PSV <- PSV$PSVs
	PSV
}

my_psc <- function(metrics.input)
{
	PSC <- pscCorr(metrics.input$picante.cdm, metrics.input$tree)
	PSC <- PSC$PSCs
	PSC
}

my_pse <- function(metrics.input)
{
	PSE <- pse(metrics.input$picante.cdm, metrics.input$tree)
	PSE <- PSE$PSEs
	PSE
}

PAE <- function(metrics.input)
	pae(metrics.input$ecoPD.cdm)

#CRITICAL STEP RIGHT HERE. NOTE THAT YOU ARE REVERSING THE DIRECTIONALITY OF IAC FROM WHAT
#IT IS DEFINED AS, AND ALSO DIFFERENTLY THAN YOUR ORIGINAL SUBMISSION. THIS MEANS YOU DONT
#NEED TO DO ANY SWITCHING OF EXPECTATIONS DOWNSTREAM, BUT REMEMBER TO REMOVE PRE-EXISTING
#CODE THAT ACCOUNTS FOR WHAT YOU FIXED HERE
IAC <- function(metrics.input)
{
	iac <- iac(metrics.input$ecoPD.cdm) * -1
	iac
}

Haed <- function(metrics.input)
	haed(metrics.input$ecoPD.cdm)

Eaed <- function(metrics.input)
	eaed(metrics.input$ecoPD.cdm)

Eed <- function(metrics.input)
	eed(metrics.input$ecoPD.cdm)

Hed <- function(metrics.input)
	hed(metrics.input$ecoPD.cdm)

SimpsonsPhy <- function(metrics.input)
	simpson(metrics.input$ecoPD.cdm, method="phylogenetic")

my_PD <- function(metrics.input)
{
	PD <- pd(metrics.input$picante.cdm, metrics.input$tree, include.root=TRUE)
	PD <- PD$PD
	PD
}

my_PD_Cadotte <- function(metrics.input)
{
	PD <- pd(metrics.input$picante.cdm, metrics.input$tree, include.root=FALSE)
	PD <- PD$PD
	PD
}

my_QE <- function(metrics.input)
{
	QE <- raoD(metrics.input$picante.cdm, metrics.input$tree)
	QE <- QE$Dkk
	QE
}
