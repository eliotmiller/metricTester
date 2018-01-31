#' Output all metrics as a list of named functions
#'
#' Creates a list of named functions, each of which accept a metrics.input object
#'
#' @details All of the functions we calculated for our manuscript are included in this
#' function. To add additional functions, they can either be defined on the fly or, to 
#' permanently include a new metric in all downstream simulations, it can be included
#' here. The function needs to be included with a name, and it must accept a metrics.input
#' as input. If the function needs additional elements not included in that input, then
#' the prepData function must also be revised. Currently MRD references the
#' non-abundance-weighted version of the function. This is simply so calcField knows
#' how to handle the results downstream. Reminder to modify that function if 
#' abundance-weighted MRD is to be implemented here.
#'
#' @return A list of named functions
#'
#' @export
#'
#' @importFrom picante mntd psv pse pd raoD
#' @importFrom vegan taxondive
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
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
	"VPD"=my_VPD,
	"NAW_MNTD"=naw_mntd,
	"AW_MNTD"=aw_mntd,
	"PSV"=my_psv,
	"PSC"=my_psc,
	"PSE"=my_pse,
	"PD"=my_PD,
	"PD_Cadotte"=my_PD_Cadotte,
	"QE"=my_QE,
	"MRD"=my_MRD
	)
}
