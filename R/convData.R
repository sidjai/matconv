convData <- function(linesMat, maps){




}

#' Make the maps for the data
#'
#' @param leftSym The left symbol that contains the matlab data
#' @param rightSym the right symbol that contains the matlab data
#' @param rClass The formal r class name that defines what the R data is
#'   outputted as
#'
#' @return A function that takes in a matlab lines and changes the data into R
#'   data lines
#' @export
makeDataMap <- function(leftSym, rightSym, rClass){

	if(!isClassName(rClass)){
		stop(sprintf("'%s' is not a valid R class", rClass))
	}




	return(function(lin){

		guts <- getBetween(lin, leftSym, rightSym)
		rout <- switch(rClass,
			vector = sprintf("c(%s)", paste(splitMatVec(guts), collapse = ", ")),
			data.frame = as.data.frame(matrixify(guts)),
			list = as.list(matrixify(guts)),
			matrix = matrixify(guts),
		)



		return(
			getBetween(lin,leftSym, rightSym, insertChar = rout, shInclude = TRUE))


	})
}

matrixify <- function(lin){
	noNums <- gsub("\\d+(\\.\\d+)?", "|", lin)
	numVec <- splitMatVec(lin)
	numVec <- as.numeric(numVec)

	refMat <- lapply(strsplit(noNums, "[|]"), function(vec){

		rows <- grep("[;]", vec)
		cols <- c(1, grep("\\s|[,]", vec))

		rowInd <- findInterval(1:length(vec), rows) + 1
		tem <- c(rows[1] - 1, diff(c(rows, length(vec)+1)))
		colInd <- c(lapply(tem, function(x){1:x}), recursive = TRUE)
		if(length(rowInd) - length(colInd) != 0) stop("non equal in matrixfy")
		return(cbind(rowInd, colInd))
	})

	rMat <- simplify2array(refMat)
	maxes <- vapply(1:dim(rMat)[3], function(x){
		c(max(rMat[,1,x]), max(rMat[,2,x]))
	}, rep(1,2))

	outMat <- sprintf(
		"matrix(c(%s), nrow = %d, ncol = %d)",
		paste(numVec, collapse = ", "),
		maxes[1,],
		maxes[2,])

	return(outMat)

}

splitMatVec <- function(sin){

	allNums <- gsub(",", " ", gsub(";", " ", sin))
	thingVec <- trimWhite(c(strsplit(allNums, " "), recursive = TRUE))
	return(thingVec[nzchar(thingVec)])

}
