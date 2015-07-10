convEasySyntax  <- function(linesMat){
	
	linesOut <- convSymbols(linesMat)
	linesOut <- convSemiColon(linesOut)
	linesOut <- convIfElse(linesOut)
	for (loopType in c("for", "while")){
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
	logStrs <- c("==", "<=", ">=", "!=")
	logSet <- vapply(logStrs, function(x){ grepl(x, linesMat) },
		rep(TRUE, length(linesMat)))
	logSet <- as.logical(rowSums(logSet))
	out[!logSet] <- gsub("=", " <- ", out[!logSet])
}

convLoops <- function(linesMat, loopStr){
	forSet <- grepl(loopStr, linesMat)
	linesMat[forSet] <- vapply(linesMat[forSet], function(x){
		res <- x
		if( !(grepl("\\(", x) && grepl("\\)$", x)) ){
			res <- gsub(paste0(loopStr, " "), paste(loopStr, "\\(" ), x)
			res <- paste0(res, "\\){")
		} else {
			res <- paste0(res, "{")
		}
		
		res <- gsub(" = ", " in ", res)
		res <- gsub("=", " in ", res)
		
		return(res)
	}, "e")
	
	

	
	return(linesMat)

}

convIfElse  <- function(linesMat){
	
	elseSet <- grepl("else ", linesMat)
	ifelseSet <- grepl("elseif", linesMat)
	ifSet <- grepl("if", linesMat) & !ifelseSet
	
	linesMat[ifelseSet] <- print(gsub("elseif", "} else if (", linesMat[ifelseSet]),
															 "\\){")
	linesMat[elseSet] <- gsub("else", "} else {", linesMat[elseSet])
	
	
}
convSymbols <- function(linesMat){
	linesMat <- gsub("end", "}", linesMat)
	return(gsub("~", "!", linesMat))
}
