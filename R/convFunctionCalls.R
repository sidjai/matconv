convFunctionsCalls <- function(linesMat, maps){
	linesDes <- linesMat
	assignInd <- regexpr("[<]\\-", linesMat)
	leftParList <- gregexpr("\\(", linesMat)
	leftParInd <- vapply(leftParList, function(x){ rev(x)[1] }, 1)
	potSet <- (assignInd < leftParInd)

	funName <- getBetween(linesMat[potSet], '-\\s', '(')

	inMapsSet <- funName %isKey% maps
	potSet[!inMapsSet]<- FALSE

	matArgs <- strsplit(getBetween(linesMat[potSet], '(', ')'), ',')
	argMaps <- maps[funName][inMapsSet]

	rArgs <- mapply(function(marg, mp){
		marg <- trimWhite(marg)
		if("argMap" %in% names(mp)){
			out <- mp$argMap(marg)$rargs
		} else {
			#Multiple dictionaries per matlab function
			#use fun switcher
			rightDict <- mp$flags$multSwitch(marg)
			out <- mp[[rightDict]]$argMap(marg)$rargs
		}
		return(out)
	}, matArgs, argMaps)

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

	

	argFuns <- lapply(allDictArgs, function(x){ parseArgs(x) })

	dupsMat <- (duplicated(allFunNames) | duplicated(allFunNames, fromLast = TRUE))

	anum <- 1
	while(anum <= length(argFuns)){
		nam <- allFunNames[anum]
		if(dupsMat[anum]){
			lastDup <- which(!dupsMat[anum:length(argFuns)])[1] - 1

			vapply(anum:lastDup, function(x){
				maps[[nam]][[x]]$argMap <- argFuns[[x]]
				TRUE
			}, TRUE)

			anum <- lastDup
		} else {
			maps[[nam]]$argMap <- argFuns[[anum]]
		}

		anum <- anum + 1
	}
	
	
	maps[finFunNames]$flags <- lout$flags

	return(maps)

}

`%isKey%` <- function(vals, ldict){
	return(is.element(names(ldict), vals))
}

parseArgs <- function(dictArg){
	sargs <- strsplit(dictArg, ',')
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


	flags <- lapply(1:length(dictLines), function(x){ list() })
	strSansFlags <- dictLines

	#separate flags
	stFlag <- gregexpr("\\-\\-", dictLines)
	stDiv <- gregexpr("[:]", dictLines)
	flagNums <- which(vapply(stFlag, function(x){ x[1] > 0 }, TRUE))

	for(ind in flagNums){
		left <- stFlag[ind] + 2
		right <- ifelse(stFlag[ind] > stDiv[ind],
			nchar(dictLines[ind]),
			stDiv[ind]
		)

		flagStr[[ind]] <- mapply(function(lt, rt){
			substr(strSansFlags[ind], lt, rt) <- ''
			substr(dictLines[ind], lt, rt)
		}, left, right)

		flagType[[ind]] <- match(flagStr[[ind]], possFlags)
	}

	#make flags and funcSwitchers
	matName <- vapply(strsplit(strSansFlags, ":"), function(x){ x[1] },"e")
	dupsMat <- (duplicated(matName) | duplicated(matName, fromLast = TRUE))

	fnum <- 1
	while(fnum <= length(flagNums)){
		ind <- flagNums(fnum)

		if(dupsMat[ind]){
			lastDup <- which(!dupsMat[ind:length(flagNums)])[1] - 1

			flags[[ind]] <- lapply(ind:lastDup, function(x){
				makeFlag(flagStr[[x]], makeSwitch = FALSE)
			})

			flags[[ind]]$multSwitch <- makeFunSwitcher(flagStr[ind:lastDup])
			fnum <- lastDup
		} else {
			flags[[ind]] <- makeFlag(flagStr[[ind]])
		}



		fnum <- fnum + 1
	}




	return(mget(c("strSansFlags", "flags")))
}

makeFlag <- function(vin, makeSwitch = TRUE){
	flag <- list()
	possFlags <- c("if", "out", "space-sep", "not-req")

	para <- strsplit(vin, " ")
	flagName <- vapply(para, function(x){ x[1] }, "e")


	vapply(vin, function(x){
		para <- strsplit(x, " ")[[1]]
		fladName <- para[1]
		if(flagName == "if" && makeSwitch){
			flag$multSwitch <- makeFunSwitcher(list(x))
		} else if (flagName == "out"){
			flag$varOut <- para[-1]
		} else if (flagName == "space-sep"){
			flag$spaceSepMatArgs <- TRUE
		} else if (flagName == "not-req"){
			flag$argNotReq <- as.integer(para[-1])
		} else {
			stop(paste("The flag:", quote(x), "is indecipherable", sep = "\n"))
		}

		TRUE
	}, TRUE)

	return(flag)
}

makeFunSwitcher <- function(lFlags){

	finallyInd <- NULL
	lengthOutVec <- lengthVec <- rep(NA, length(lFlags))
	matMap <- lapply(1:length(lFlags), function(x){
		list(arg = NULL, val = NULL)
	})

	for(dictNum in 1:length(lFlags)){
		para <- strsplit(lFlags[[dictNum]][1], ' ')[[1]][-1]
		if(length(para) == 1){
			if(para[1] == "finally"){
				finallyInd <- dictNum
			} else {
				lengthVec[dictNum] <- as.integer(para[1])
			}
		} else {
			if(para[1] == "length(out)"){
				lengthOutVec[dictNum] <- as.integer(para[3])
			} else {
				matMap[[dictNum]]$arg <- para[1]
				matMap[[dictNum]]$val <- gsub("L", "", para[3])
			}
		}
	}

	return(function(matArgs, numOut = 1){
		useInd <- NULL
		if(numOut > 1){
			useInd <- which(lengthOutVec == numOut)
		}

		useInd <- c(useInd, which(lengthVec == length(matArgs)))

		test <- vapply(matMap, function(mp){
			check <- matArgs[as.integer(mp$arg)] == mp$val
			if(length(check) == 0) check <- FALSE
			return(check)
		}, TRUE)
		useInd <- c(useInd, which(test))

		if(length(useInd) == 0){
			if(!is.null(finallyInd)){
				useInd <- finallyInd
			} else {
				stop(paste("Do not have rule that supports:" , matArgs))
			}
		}

		return(useInd[1])
	})
}
