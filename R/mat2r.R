#' mat2r
#'
#'The top level driver function to call the converting functions and handle the
#'input and output.
#' @param inMat A file path with the input MatLab / Octave code to be converted
#' or a character vector of the code that needs to be converted
#' @param pathOutR A file path with the desired output file location
#' @param funcConverters A list of function converters that the user wants to use in this conversion
#' @param verbose A number indicating the amount of messages that should be outputed.
#' \describe{
#'   \item{0}{No messages}
#'   \item{1}{A summary report of what happened in the conversion}
#'   \item{2}{The final code as a message as well as the summary report}
#' } 
#'
#' @return A list containing the original code (named matCode) and the converted code (named rCode).
#' @export
mat2r <- function(inMat, pathOutR ='', funcConverters = NULL, verbose = 1){

	if (length(inMat) ==1 && file.exists(inMat){
		if(grepl(".m", inMat)) stop("Please supply a '.m' file")
		rawMat <- readLines(inMat)
	} else {
		rawMat <- inMat
	}

	linesDes <- linesOrg <- trimWhite(rawMat, "end")
	isScr <- !grepl("function", linesOrg[1])
	


	commentSet <- grepl("^%", linesDes) | grepl("^\\s+%", linesDes)

	codeDes <- linesDes[!commentSet]
	codeDes <- convEasySyntax(codeDes)
	
	linesDes[!commentSet] <- codeDes
	linesDes[commentSet] <- gsub("%", "#", linesDes[commentSet])
	
	if (!isScr) linesDes <- convUserFunctions(linesDes)

	
	if(!is.null(funcConverters)){
		codeDes <- vapply(funcConverters, function(conv){
			useFuncConv(linesDes, conv)
		}, rep(5,1))
	}

	

	report <- sprintf("The previous code had %f lines and the R code has %f lines", length(linesOrg), length(linesDes))

	if(verbose == 2 ){
		message(report)
		message(linesDes)
	} else if (verbose == 1) {
		message(report)
	}
	
	if(nzchar(pathOutR)) writeLines(linesDes, pathOutR)

	return(list(matCode = linesOrg, rCode = linesDes))

}
