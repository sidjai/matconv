convData <- function(linesMat, maps){
	
	linesDes <- vapply(linesMat, function(lin){
		goodLin <- lin
		for(mp in maps){
			goodLin <- mp(goodLin)
		}
		return(goodLin)
		
	}, "e")
	
	return(linesDes)
}

#' Make the maps for converting slice notation
#'
#' @inheritParams makeDataMap
#' @return A function that takes in a string and converts all the given slice
#'   notation
#' @details Slice notation for matrices are tricky because they can easily be confused withThe requirements for conversion are the bounds given by both left
#'   and right symbols or the matLab class. The matlab class allows for the
#'   conversion of structures but is really just a dictionary for the different
#'   bounds. 
#' @export
makeSliceMap <- function(leftSym, rightSym, rClass, matClass = ""){
	
	if(!isClassName(rClass)){
		stop(sprintf("'%s' is not a valid R class", rClass))
	}
	
	if(!nzchar(matClass)){
		if(missing(leftSym) || missing(rightSym)){
			stop("Please provide either the bounds of the data or the matlab class")
		}
	} else {
		syms <- getMatLabClassBounds(matClass)
		leftSym <- syms[1]
		rightSym <- syms[2]
	}
	
	if(matClass == "matrix"){
		stop(paste(
			"Matrix slicing will not be converted because it has the same",
			"notation as function use. Converting these would be destructive."))
	}
	
	rBounds <- switch(rClass,
		vector = c("[", "]"),
		data.frame = ,
		list = c("[[", "]]")
	)
	
	return(function(lin){
		
		goodLin <- lin
		
		guts <- shExtractData(lin, leftSym, rightSym, type = "slice")
		while(nzchar(guts)){
			bef <- goodLin
			
			goodLin <- if(matClass == "structure"){
				rout <- sprintf("%s'%s'%s", rBounds[1], guts, rBounds[2])
				bas <- getBetween(goodLin, leftSym, rightSym, insertChar = rout)
				sub("[.]", "", bas)
			} else {
				rout <- paste0(rBounds[1], guts, rBounds[2])
				getBetween(goodLin, leftSym, rightSym, insertChar = rout, shInclude = TRUE)
			}
			guts <- getBetween(removeStrings(goodLin), leftSym, rightSym)
		}
		return(goodLin)
		
	})
}

removeStrings <- function(lin){
	noStringLin <- getBetween(lin, "'", "'", insertChar = "")
	noStringLin <- getBetween(noStringLin, '"', '"', insertChar = "")
	return(noStringLin)
}

shExtractData <- function(lin, leftSym, rightSym, type = c("inst", "slice")[1]){
	preLin <- lin
	if(type == "slice") preLin <- removeStrings(preLin)
	guts <- getBetween(preLin, leftSym, rightSym)
	
	equ <- regexpr("[<]-|=", preLin)
	st <- regexpr(paste0("\\", leftSym), preLin)
	
	varNamePos <- regexpr(paste0("\\w+\\", leftSym), preLin)
	
	
	if(type == "inst"){
		if(equ > st || varNamePos > 0) guts <- "" 
	} else {
		if(varNamePos < 0) guts <- ""
	}
	
	return(guts)
}

getMatLabClassBounds <- function(matClass){
	
	matDict <- list(
		structure = c(".", "W|$"),
		cell = c("{", "}"),
		matrix = c("[", "[")
		)
	
	bounds <- matDict[[matClass]]
	
	if(is.null(bounds)){
		stop(paste(
			"The class '", matClass,
			"is not supported. supported classes are",
			paste(names(matDict), sep = " | ")))
	}
	return(bounds)
}

#' Make the maps for the data
#'
#' @param leftSym The left symbol that contains the matlab data
#' @param rightSym the right symbol that contains the matlab data
#' @param rClass The formal r class name that defines what the R data is
#'   outputted as
#' @param matClass The name of the matlab class that should be converted
#' @return A function that takes in a matlab lines and changes the data into R
#'   data lines
#' @details The requirements for conversion are the bounds given by both left
#'   and right symbols or the matLab class. The matlab class allows for the
#'   conversion of structures but is really just a dictionary for the different
#'   bounds.
#' @export
makeDataMap <- function(leftSym, rightSym, rClass, matClass = ""){

	if(!isClassName(rClass)){
		stop(sprintf("'%s' is not a valid R class", rClass))
	}
	
	if(!nzchar(matClass)){
		if(missing(leftSym) || missing(rightSym)){
			stop("Please provide either the bounds of the data or the matlab class")
		}
	} else {
		syms <- getMatLabClassBounds(matClass)
		leftSym <- syms[1]
		rightSym <- syms[2]
	}




	return(function(lin){
		guts <- shExtractData(lin, leftSym, rightSym, type = "inst")
		
		if(!nzchar(guts)){
			return(lin)
		} else {
			
			rout <- switch(rClass,
				vector = sprintf("c(%s)", paste(splitMatVec(guts), collapse = ", ")),
				data.frame = as.data.frame(matrixify(guts)),
				list = as.list(matrixify(guts)),
				matrix = matrixify(guts)
			)
			return(
				getBetween(lin,leftSym, rightSym, insertChar = rout, shInclude = TRUE))
		}


	})
}

matrixify <- function(lin){
	noNums <- gsub("\\d+(\\.\\d+)?|NA|NaN", "|", lin)
	numVec <- splitMatVec(lin)
	numVec <- as.numeric(numVec)

	refMat <- lapply(strsplit(noNums, "[|]"), function(vec){

		rows <- c(1, grep("[;]", vec))
		cols <- c(1, grep("\\s|[,]", vec))

		rowInd <- findInterval(1:length(vec), rows, rightmost.closed = FALSE)
		tem <- diff(c(rows, length(vec)+1))
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
