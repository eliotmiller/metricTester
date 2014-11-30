#' Output all null models as a list of named functions
#'
#' Creates a list of named functions, each of which accept a nulls.input object
#'
#' @details All of the null models we calculated for our manuscript are included in this
#' function. To add additional functions, they need to be defined and included
#' here. The function needs to be included with a name, and it must accept a nulls.input
#' as input. If the function needs additional elements not included in that input, then
#' the prepNulls function must also be revised.
#'
#' @return A list of named functions
#'
#' @export
#'
#' @import phylobase grid ecoPDcorr
#'
#' @references Miller, Trisos and Farine.
#'
#' @examples
#' library(geiger)
#' library(picante)
#'
#' defineNulls()

defineNulls <- function()
{
	list("twoX"=my_2x,
	"threeX"=my_3x,
	"oneS"=my_1s,
	"twoS"=my_2s,
	"regional"=my_regional,
	"richness"=my_richnessNull,
	"frequency"=my_frequency,
	"independent_swap"=my_IS,
	"trial_swap"=my_TS
	)
}

my_2x <- function(nulls.input)
{
	temp.matrix <- resamp.2x(nulls.input$spacodi.cdm)
	new.matrix <- suppressMessages(as.picante(temp.matrix))
}

my_3x <- function(nulls.input)
{
	temp.matrix <- resamp.3x(nulls.input$spacodi.cdm)
	new.matrix <- suppressMessages(as.picante(temp.matrix))
}

my_1s <- function(nulls.input)
{
	temp.matrix <- resamp.1s(nulls.input$spacodi.cdm)
	new.matrix <- suppressMessages(as.picante(temp.matrix))
}

my_2s <- function(nulls.input)
{
	temp.matrix <- resamp.2s(nulls.input$spacodi.cdm)
	new.matrix <- suppressMessages(as.picante(temp.matrix))
}

my_regional <- function(nulls.input)
{
	new.matrix <- regionalNull(cdm=nulls.input$picante.cdm, tree=nulls.input$tree, 
		regional.abundance=nulls.input$regional.abundance)
}

my_richnessNull <- function(nulls.input)
{
	new.matrix <- randomizeMatrix(nulls.input$picante.cdm, "richness")
}

my_frequency <- function(nulls.input)
{
	new.matrix <- randomizeMatrix(nulls.input$picante.cdm, "richness")
}

my_IS <- function(nulls.input)
{
	new.matrix <- randomizeMatrix(nulls.input$picante.cdm, "independentswap", 
		iterations=100000)
}

my_TS <- function(nulls.input)
{
	new.matrix <- randomizeMatrix(nulls.input$picante.cdm, "trialswap", 
		iterations=100000)
}
