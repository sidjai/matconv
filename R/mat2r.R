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
mat2r <- function(pathInMat, pathOutR, funcConverters, verbose = 1){

	rawMat <- readLines(pathInMat)
	linesDes <- linesOrg <- gsub("\\s+$", "",rawMat)
	isScr <- !grepl("function", linesOrg[1])

	#Get rid of semi colons
	# gsub([=], [<-]) #also make sure there are spaces surrounding
	#

	commentSet <- grepl("^%", linesDes)

	codeDes <- linesDes[!commentSet]

	codeDes <- convEasySyntax(codeDes)

	results <- vapply(funcConverters, function(conv){
		useFuncConv(codeDes, conv)
	}, rep(5,1)) #Converter results

	linesDes[!commentSet] <- results
	linesDes[commentSet] <- gsub("%", "#", linesDes[commentSet])

	if(verbose == 2 ){
		cat(results)
		cat(sprintf("The previous code had %f lines and the R code has %f lines", length(linesOrg), length(linesDes)))
	} else if (verbose == 1) {
		cat(results)
	}

	writeLines(linesDes, pathOutR)

	return(list(report = results,
							matCode = linesOrg,
							rCode = linesDes)
	)
}
