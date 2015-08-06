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

	rArgs <- mapply(function(marg, fun){ fun(trimWhite(marg))$rargs }, matArgs, argMaps)

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

	maps <- list()

	lout <- parseFlags(dictLines)
	dictLines <- lout$strSansFlags

	keyVal <- strsplit(dictLines, ":")
	allFunNames <- vapply(keyVal, function(x){ x[1] }, "e")
	allDictArgs <- vapply(keyVal, function(x){ x[2] }, "e")
	finFunNames <- unique(allFunNames)

	maps[finFunNames]$flags <- lout$flags

	argFuns <- lapply(rawDictArgs, function(x){ parseArgs(rawDictArgs) })
	finArgFuns <-

	maps[finFunNames]$argMap <- finArgFuns


	return(maps)

}

`%isKey%` <- function(vals, ldict){
	return(is.element(names(ldict), vals))
}

parseArgs <- function(dictArgs){
	sargs <- strsplit(dictArgs, ',')
	sargs <- trimWhite(sargs[[1]])

	rname <- sargs[1]
	sargs <- sargs[-1]

	swiSet <- grepl("^[0-9]+$", sargs)
	literalNumSet <- grepl("^[0-9]+L$", sargs)
	strInsertSet <- grepl("\\%[0-9]", sargs)
	stringSet <- !literalNumSet & !swiSet & !strInsertSet

	return(function(matArg){
		rargs <- NULL
		rargs[swiSet] <- matArg[as.integer(sargs[swiSet])]
		rargs[literalNumSet] <- as.numeric(gsub("L", "", sargs[literalNumSet]))
		for(iar in which(strInsertSet)){
			arg <- sargs[iar]
			test <- TRUE
			while(test){
				ind <- as.numeric(getBetween(arg, '%', ''))
				arg <- sub("\\%[0-9]", matArg[ind], arg)
				test <- grepl("\\%[0-9]", arg)
			}
			rargs[iar] <- arg
		}

		rargs[stringSet] <- sargs[stringSet]

		return(list(
			rargs = paste0(rname, '(', paste(rargs, collapse = ", "))
			))
	})
}

parseFlags <- function(dictLines){
	possFlags <- c("if", "out", "space-sep", "not-req")
	flags <- list()

	#separate flags
	
	for(lin in dictLines){
	div <- regexpr("[:]", lin)

	while(grepl("\\-\\-", sin)){
		pind <- regexpr("\\-\\-", sin)
		left <- pind + 2
		right <- ifelse(pind > div, nchar(sin), div)

		flagStr <- substr(sin, left, right)
		fargs <- strsplit(sin, ' ')[[1]]
		
		sin <- paste0(
			substr(sin, 1 , left),
			substr(sin, right, nchar(sin))
			)
	}


	return(list(
		strSansFlags = sin,
		flags = flags
		))
}

makeFunSwitcher <- function(lFlags){

	return(function(matArgs){

		})
}
