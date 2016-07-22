#' Confirm that the null model functions are in suitable format
#'
#' Utility function. Creates a list of null models, either those defined in defineNulls
#' or a named list of null model functions.
#'
#' @param x Optional. If not provided, defines the nulls as those in defineNulls. Else
#' either a character vector or a named list of functions, depending on whether new_ is
#' set to TRUE or FALSE. See runNulls.
#' @param new_ Whether or not new nulls are being defined on the fly. Default is FALSE.
#' Set to TRUE if a new null is being used.
#' 
#' @details A few quick checks to confirm the null model functions are input in suitable
#' format.
#'
#' @return A list of functions.
#'
#' @export
#'
#' @references Miller, E. T., D. R. Farine, and C. H. Trisos. 2016. Phylogenetic community
#' structure metrics and null models: a review with new methods and software.
#' Ecography DOI: 10.1111/ecog.02070
#'
#' @examples
#' checkNulls(names(defineNulls()))

checkNulls <- function(x, new_=FALSE)
{
	#if nothing is passed to checkNulls, then just calculate all nulls
	if (is.null(x))
	{
		nulls <- defineNulls()
	}

	#if a character vector is passed to checkNulls, and new_ remains FALSE, run the
	#unexported function nullNameMatcher. this checks whether all the names in the
	#character vector match named functions from defineNulls, then outputs a named list
	else if(!is.null(x) & new_==FALSE)
	{
		if (!inherits(x, "character"))
		{
			stop("The nulls need to be input as a character vector of named functions")
		}
					
		nulls <- nullNameMatcher(null.name.vector=x)
	}
	
	#if a new null is passed to checkNulls, then new_ needs to be set to TRUE. 
	#in this case, it needs to be passed as a named list
	else if(!is.null(x) & new_==TRUE)
	{
		if (!inherits(x, "list"))
		{
			stop("New nulls need to be input as a list of named functions")
		}
		if (is.null(names(x)))
		{
			stop("New nulls need to be input as a list of named functions")
		}
		
		nulls <- x
	}					
	nulls
}

nullNameMatcher <- function(null.name.vector)
{
	#define all nulls here
	allPossible	<- defineNulls()
	
	#throw an error if the null.name.vector does not perfectly match named nulls
	if(length(setdiff(null.name.vector, names(allPossible))) > 0)
	{
		stop("Not all of your specified nulls match those named in defineNulls()")
	}
	
	#now subset them to just those defined in null.name.vector
	output <- allPossible[null.name.vector]
	
	output
}
