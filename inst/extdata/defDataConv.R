dataConvs <- list()
dataConvs[[1]] <- makeDataMap("[", "]", "vector")
dataConvs[[2]] <- makeDataMap("{", "}", "matrix")

dataConvs[[3]] <- makeSliceMap("{", "}", "list")
dataConvs[[4]] <- makeSliceMap(matClass = "structure", rClass = "list")
