getBetween <- function(sin, left, right, insertChar = NULL){
	leftPos <- regexpr(paste0("\\", left), sin)
	rightPos <- regexpr(paste0("\\", right), sin)

	if(is.null(insertChar)){
		cap <- substr(sin, leftPos + 1 , rightPos - 1)
		return(trimWhite(cap))
	} else {
		newStr <- paste0(
			substr(sin, 1, leftPos),
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
