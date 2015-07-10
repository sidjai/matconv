#' mat2r
#'
#'The top level driver function to call the converting functions and handle the
#'input and output.
#' @param pathInMat A string file path with the input MatLab / Octave code to be converted
#' @param pathOutR A string file path with the desired output file location
#' @param funcConverters a list of function converters that the user wants ot use this conversion
#' @param verbose A number indicating the amount of output desired. (0-2)
#'
#' @return A list containing the report file and the converted code.
#' @export
mat2r <- function(pathInMat, pathOutR, funcConverters = NULL, verbose = 1){

	rawMat <- readLines(pathInMat)
	linesDes <- linesOrg <- gsub("\\s+$", "",rawMat)
	isScr <- !grepl("function", linesOrg[1])

	#Get rid of semi colons
	# gsub([=], [<-]) #also make sure there are spaces surrounding
	#

	commentSet <- grepl("^%", linesDes)

	codeDes <- linesDes[!commentSet]

	codeDes <- convEasySyntax(codeDes)
	if(!is.null(funcConverters)){
		codeDes <- vapply(funcConverters, function(conv){
			useFuncConv(codeDes, conv)
		}, rep(5,1))
	}

	

	linesDes[!commentSet] <- codeDes
	linesDes[commentSet] <- gsub("%", "#", linesDes[commentSet])

	if(verbose == 2 ){
		cat(codeDes)
		cat(sprintf("The previous code had %f lines and the R code has %f lines", length(linesOrg), length(linesDes)))
	} else if (verbose == 1) {
		cat(codeDes)
	}

	writeLines(linesDes, pathOutR)

	return(list(matCode = linesOrg,
							rCode = linesDes)
	)
}
