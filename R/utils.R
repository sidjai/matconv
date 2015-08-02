getBetween <- function(sin, left, right, insertChar = NULL){
	
	if(!nzchar(left)){
		rightPos <- regexpr(paste0("\\", right), sin)
		leftPos <- defaultOneChar(rightPos, `-`)
	} else if(!nzchar(right)){
		leftPos <- regexpr(paste0("\\", left), sin)
		rightPos <- defaultOneChar(leftPos, `+`)
	} else {
		rightPos <- regexpr(paste0("\\", right), sin)
		leftPos <- regexpr(paste0("\\", left), sin)
	}




	if(is.null(insertChar)){
		cap <- substr(sin,
			leftPos + attr(leftPos, "match.length"),
			rightPos - 1
		)
		return(trimWhite(cap))
	} else {
		newStr <- paste0(
			substr(sin, 1, leftPos + attr(leftPos, "match.length") - 1 ),
			insertChar,
			substr(sin, rightPos, nchar(sin))
		)
		return(newStr)
	}
}

trimWhite <- function(sin, where = "both"){
	return(switch(where,
                beg = gsub("^\\s+", "", sin),
                end = gsub("\\s+$", "", sin),
                both = gsub("^\\s+|\\s+$", "", sin)
	))
}

asRightMatrix <- function(vin){
	if(!is.matrix(vin)){
		t(as.matrix(vin))
	} else {
		vin
	}
}
defaultOneChar <- function(oppsMatch, func){
	defMatch <- func(oppsMatch, 2)
	attr(defMatch, "match.length") <- 1
	return(defMatch)
}
