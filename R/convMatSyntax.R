convUserFunctions <- function(linesMat){
  funInd <- which(grepl("^function", linesMat))
  linesFun <- linesMat[funInd]
  
  allEndInd <- which(grepl("^}", linesMat))
  retInd <- allEndInd[vapply(funInd[-1], function(x){
    rev(which(allEndInd < x))[1]
  }, 1)] -1
  retInd <- c(retInd, rev(allEndInd)[1] - 1 )
  returnObj <- captureBetween(linesFun, "[", "]")
  args <- captureBetween(linesFun, "(", ")")
  
  funName <- captureBetween(linesFun, "- ", "(")
  
  linesFun <- paste(funName, "<-", sprintf("function(%s){", args))
  linesMat[funInd] <- linesFun
  
  # save the return statement for last cause it throws off all the indices
  for (funi in length(returnObj):1){
  	linesMat <- append(linesMat, sprintf("\treturn(%s)", returnObj[funi]),
                       after = retInd[funi])
  }
  
  
  return(linesMat)
}

captureBetween <- function(sin, left, right){
	leftPos <- regexpr(paste0("\\", left), sin) 
	rightPos <- regexpr(paste0("\\", right), sin)
	cap <- substr(sin, leftPos + 1 , rightPos - 1)
	return(trimWhite(cap))
	
}
