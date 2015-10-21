convData <- function(linesMat, maps){




}

makeDataMap <- function(leftSym, rightSym, rClass){

	if(!isClassName(rClass)){
		stop(sprintf("'%s' is not a valid R class "))
	}




	return(function(lin){

		guts <- getBetween(lin, leftSym, rightSym)
		rout <- switch(rClass,
			vector = sprintf(,guts),
			data.frame = sprintf(,guts),
			list = sprintf(,guts),
			data.frame = sprintf(,guts),
		)

		return(rout)


	})
}
