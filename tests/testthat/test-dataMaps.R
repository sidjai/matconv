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
