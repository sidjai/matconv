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

	linesOut <- gsub(" = ", " <- ", linesMat)
	

	#filter all the other cases matlab uses =
	logStrs <- c("==", "<=", ">=", "!=")
	logSet <- vapply(logStrs, function(x){ grepl(x, linesMat) },
		rep(TRUE, length(linesMat)))
	logSet <- as.logical(rowSums(logSet))
	linesOut[!logSet] <- gsub("=", " <- ", linesOut[!logSet])
	return(linesOut)
}

convLoops <- function(linesMat, loopStr){
	loopSet <- grepl(loopStr, linesMat)
	loopLines <- paraAroundThings(linesMat[loopSet], loopStr)
	loopLines <- gsub(" = ", " in ", loopLines)
	loopLines <- gsub("=", " in ", loopLines)
	
	linesMat[loopSet] <- loopLines
	return(linesMat)

}

convIfElse  <- function(linesMat){
	
	elseSet <- grepl("else$", linesMat)
	ifelseSet <- grepl("elseif", linesMat)
	ifSet <- grepl("if", linesMat)
	
	linesMat[ifelseSet] <- gsub("elseif", "} else if", linesMat[ifelseSet])
	linesMat[elseSet] <- gsub("else", "} else {", linesMat[elseSet])
	
	linesMat[ifSet] <- paraAroundThings(linesMat[ifSet], "if")
	
	return(linesMat)
}
convSymbols <- function(linesMat){
	linesMat <- gsub("end", "}", linesMat)
	return(gsub("~", "!", linesMat))
}

paraAroundThings <- function(sin, type){
	startChar <- regexpr(type, sin) + 2
	lastChar <- nchar(sin)
	aftThing <- substr(sin, startChar, lastChar)
	firstLet <- regexpr("\\w", aftThing)
	firstPara <- regexpr("\\(", aftThing)
	
	badSet <- (firstLet < firstPara | firstPara < 0)
	sin[badSet] <- paste0(
		gsub(paste0(type, ' '), paste0(type, " ("), sin[badSet]),
		"){")
	sin[!badSet] <- gsub(paste0(type, "\\("), paste(type, '('), sin[!badSet])
	sin[!badSet] <- paste0(sin[!badSet], '{')
	return(sin)
	
}
