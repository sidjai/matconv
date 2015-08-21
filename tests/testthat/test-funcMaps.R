context("Function Maps")


test_that("Basic argument switching", {
  dict <- "matsort:sort, 2, 1"
  example <- c("asdf", "hjkl")
  map <- makeMaps(dict)
  result <- map$matsort$argMap[[1]](example)$rargs
  expect_match(result, paste(rev(example), collapse = ', '))
})

test_that("Literal numbers in output", {
  dict <- "matsort:sort, 2, 1L"
  example <- c("asdf", "hjkl")
  map <- makeMaps(dict)
  result <- map$matsort$argMap[[1]](example)$rargs
  expect_match(result, paste(example[2], 1, sep = ', '))
})

test_that("Literal arg inserts", {
  dict <- "matsort:sort, %1 * %2, asdf%1"
  example <- c("asdf", "hjkl")
  map <- makeMaps(dict)
  result <- map$matsort$argMap[[1]](example)$rargs
  expect_equal(result,
    paste(
      paste0("sort(", example[1], ' * ', example[2]),
      paste0("asdf", example[1]),
      sep = ', '))
})

test_that("Flag switching integrates", {
	dict <- c(
		"matsort--if 2:sort, 2, 1",
		"matsort--if 3:sort, 3")
	example <- c(
		"thing <- matsort(asdf, hjkl)",
		"thing <- matsort(asdf, hjkl, omg)")
	map <- makeMaps(dict)
	result <- convFunctionsCalls(example, map)
	expect_equal(result[1],
		"thing <- sort(hjkl, asdf)")
	expect_equal(result[2],
		"thing <- sort(omg)")
	
})
