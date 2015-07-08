convEasySyntax  <- function(linesMat){


	linesOut <- convSemiColon(linesMat)
	linesOut <- convIfElse(linesOut)
	for (loopType in c("for", "if", "while")){
		linesOut <- convLoops(linesOut,loopType)
	}

	linesOut <- convEqualsArrow(linesOut)

	return(linesOut)
}

convSemiColon <- function(linesMat){
	return(gsub(";$", "", linesMat))
}

convEqualsArrow <- function(linesMat){

	out <- gsub(" = ", " <- ", linesMat)

	#filter all the other cases matlab uses =
	logStrs <- c("==", "<=", ">=")
	logSet <- vapply(logStrs, function(x){ grepl(x, linesMat) },
		rep(TRUE, length(linesMat)))
	logSet <- (logSet[, 1] | logSet[, 2] | logSet[, 3] )
	out[!logSet] <- gsub("=", " <- ", out[!logSet])
}

convLoops <- function(linesMat, loopStr){
	forSet <- grepl(loopStr, linesMat)
	linesMat[forSet] <- vapply(linesMat[forSet], function(x){
		res <- x
		if( !(grepl("\\(", x) && grepl("\\)$", x)) ){
			res <- gsub(paste0(loopStr, " "), paste(loopStr, "\\(" ), x)
			res <- paste0(res, "\\)\\{")
		} else {
			res <- paste0(res, "\\{")
		}
		if( !grepl("if", loopStr)){
			res <- gsub(" = ", " in ", res)
			res <- gsub("=", " in ", res)
		}
	}, rep(TRUE, length(linesMat[forSet])))

	linesMat <- gsub("end", "\\}", linesMat)
	return(linesMat)

}
