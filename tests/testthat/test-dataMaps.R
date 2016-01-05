context("Data Conversion")


test_that("Can turn into matrix: Digits",{
	
	exm <- "1, 5,6;2,7,2"
	matRep <- matrixify(exm)
	expect_true(grepl("nrow = 2", matRep))
	expect_true(grepl("ncol = 3", matRep))
	expect_true(grepl("c\\(1, 5, 6, 2, 7, 2\\)", matRep))
})

test_that("Can turn into matrix: Numerics",{
	
	exm <- "1, 5,6;2,7,2.234"
	matRep <- matrixify(exm)
	expect_true(grepl("nrow = 2", matRep))
	expect_true(grepl("ncol = 3", matRep))
	expect_true(grepl("c\\(1, 5, 6, 2, 7, 2.234\\)", matRep))
})

test_that("Integration: data maps", {
	dataMap <- makeDataMap("[", "]", "matrix")
	
	test <- "scrRes <- [23,2, 3.2; 7, 6, 8];"
	res <- dataMap(test)
	eval(parse(text = res))
	
	expect_true(is.matrix(scrRes))
	expect_equal(dim(scrRes)[1], 2)
	expect_equal(dim(scrRes)[2], 3)
	
	dataVecMap <- makeDataMap("{", "}", "vector")
	testCell <- "celRes <- {23,2, 3.2; 7, 6, 8};"
	res <- dataVecMap(testCell)
	eval(parse(text = res))
	
	expect_true(is.vector(celRes))
	expect_equal(length(celRes), 6)
	expect_null(dim(celRes))
	
})

test_that("Data maps doesn't pick up non-instantiations",{
	dataMap <- makeDataMap("{", "}", "matrix")
	negTest <- "stmTable{rr,4} <- 'Feed'"
	res <- dataMap(negTest)
	
	expect_true(!is.na(match(res, negTest)))
	
	
})

test_that("Missing numbers can be inputted as data",{
	dataMap <- makeDataMap("{", "}", "matrix")
	NATest <- "stmTable <- {NaN}"
	res <- dataMap(NATest)
	eval(parse(text = res))
	expect_true(is.nan(stmTable[1]))
})

