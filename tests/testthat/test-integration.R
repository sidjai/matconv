context("integration")

source(system.file("extdata", "defDataConv.R", package = "matconv"))
hMaps <- makeFuncMaps(
	pathDict = system.file("extdata", "HiebelerDict.txt", package = "matconv")
)
test_that("Dictionaries are loaded", {
	expect_gt(length(hMaps), 0)
	expect_gt(length(dataConvs), 0)
})
