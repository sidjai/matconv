convFunctionsCalls <- function(linesMat, dict){
	linesDes <- linesMat
	assignInd <- regexpr("[<]\\-", linesMat)
	leftParList <- gregexpr("\\(", linesMat)
	leftParInd <- vapply(leftParList, function(x){ rev(x)[1] }, 1)
	potSet <- (assignInd < leftParInd)

	funName <- getBetween(linesMat[potSet], '-\\s', '(')

	inDictSet <- funName %isKey% dict
	potSet[!inDictSet]<- FALSE

	matArgs <- strsplit(getBetween(linesMat[potSet], '(', ')'), ',')
	argMaps <- dict[funName][inDictSet]

	rArgs <- mapply(function(marg, fun){ fun(trimWhite(marg)) }, matArgs, argMaps)

	linesDes[potSet] <- getBetween(linesMat[potSet], '-\\s', ')', rArgs)

	return(linesDes)
}

#' Turn dictionary lines into functions that map matlab to R function calls
#'
#' @param addDict An optional character vector with manufactored lines
#' @param pathDict The path to a text file with the dictionry lines written to it
#'
#' @return a list of functions with names of matlab functions and the function
#' to change the matlab input into an R function call
#' @export
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
			"either feed in a character vector",
			"or a file with the dictionaries", sep = ", "))
	}

	mapList <- list()
	keyVal <- strsplit(dictLines, ":")
	funNames <- vapply(keyVal, function(x){ x[1] }, "e")
	funVals <- vapply(keyVal, function(x){ x[2] }, "e")

	mapFuns <- lapply(dictLines, function(lin){
		sargs <- strsplit(funVals, ',')
		sargs <- trimWhite(sargs[[1]])

		rname <- sargs[1]
		sargs <- sargs[-1]

		swiSet <- grepl("^[0-9]+$", sargs)
		literalNumSet <- grepl("^[0-9]+L$", sargs)
		stringSet <- !literalNumSet & !swiSet

		return(function(matArg){
			rargs <- NULL
			rargs[swiSet] <- matArg[as.integer(sargs[swiSet])]
			rargs[literalNumSet] <- as.numeric(gsub("L", "", sargs[literalNumSet]))
			rargs[stringSet] <- sargs[stringSet]

			return(paste0(rname, '(', paste(rargs, collapse = ", ")))
			})
		})

	names(mapFuns) <- funNames
	return(mapFuns)

}

`%isKey%` <- function(vals, ldict){
	return(is.element(names(ldict), vals))
}
