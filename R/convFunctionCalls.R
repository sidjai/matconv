convFunctionsCalls <- function(linesMat, dict){
	linesDes <- linesMat
	assignInd <- gregexpr("[<]\\-", linesMat)
	leftParInd <- gregexpr("\\(", linesMat)
	potSet <- (assignInd > leftParInd)

	funName <- getBetween(linesMat[potSet], '- ', '(')

	inDictSet <- funName %isKey% dict
	potSet[!inDictSet]<- FALSE

	matArgs <- strsplit(getBetween(linesMat[potSet], '(', ')'), ',')
	argMaps <- dict$funName[inDictSet]

	rArgs <- mapply(function(marg, fun){ fun(marg) }, matArgs, argMaps)

	linesDes[potSet] <- getBetween(linesMat[potSet], '(', ')', rArgs)

	return(linesDes)
}

makeMaps <- function(addDict = NULL, pathDict = ''){
	dictLines <- addDict
	if(nzchar(pathDict)){
		if(!file.exists(pathDict)){
			stop(paste(pathDict, "does not exist, please supply a dictionary file"))
		}

		dictFile <- readLines(pathDict)
		if (length(dictFile) == 0){
			stop(paste(pathDict, "is empty, please fill with matLab functions"))
		}
		dictLines <- c(dictLines, dictFile)
	}

	if(length(dictLines) == 0){
		stop(paste("No dictionaries supplied",
			"either feed in a character vector"
			"or a file with the dictionaries", sep = ", "))
	}

	mapList <- list()
	funNames <- getbetween(dictLines, "", ":")
	mapFuns <- vapply(dictLines, function(lin){
		sargs <- strsplit(strsplit(lin, ":")[[2]], ',')
		sargs <- trimWhite(sargs)

		swiSet <- grepl("^[0-9]+$", sargs)
		literalNumSet <- grepl("^[0-9]+L$", sargs)
		stringSet <- !literalNumSet & !swiSet

		return(function(matArg){
			rargs[swiSet] <- matArg[as.integer(sargs[swiSet])]
			rargs[literalNumSet] <- as.numeric(sargs[literalNumSet])
			rargs[stringSet] <- sargs[stringSet]

			return(paste(rargs, sep = ", "))
			})
		}, function(x)cat(x))

	mapList[[funNames]] <- mapFuns
	return(mapList)

}
