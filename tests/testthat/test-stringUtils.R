context("String manipulation")

test_getBetweenRight <- function(corrChars,
                                 left = '(',
                                 right = ')',
                                 testString = "([1234&5])",
                                 insertSt = NULL){

  mapply(function(rt, outChars){
    parse <- getBetween(testString, left, rt, insertSt)
    expect_equal(nchar(parse), outChars,
      info = paste0(left, ' with ', rt, '->', parse))
  }, right, corrChars)
}

test_getBetweenLeft <- function(corrChars,
                                left = '(',
                                right = ')',
                                testString = "([1234&5])",
                                insertSt = NULL){
  mapply(function(lt, outChars){
    parse <- getBetween(testString, lt, right, insertSt)
    expect_equal(nchar(parse), outChars,
      info = paste0(lt, ' with ', right, '->', parse))
  }, left, corrChars)
}

test_that("getBetween does croping right with 1 symbol", {
  test_getBetweenLeft(c(8, 7, 2), left = c("(", "[", "&"))
  test_getBetweenRight(c(7, 5), right = c("]", "&"))
})

test_that("getBetween does croping right with 2 symbols", {
  test_getBetweenLeft(c(7, 6, 1), left = c("(\\[", "[1", "&5"))
  test_getBetweenRight(c(7, 5), right = c("]\\)", "&5"))
})

test_that("getBetween does insert right with 1 symbol", {
  test_getBetweenLeft(c(6, 7, 12), left = c("(", "[", "&"), insertSt = "four")
  test_getBetweenRight(c(6, 7), right = c(")", "]"), insertSt = "four")
})

test_that("getBetween does insert right with 2 symbols", {
  test_getBetweenLeft(c(7, 8, 13), left = c("(\\[", "[1", "&5"), insertSt = "four")
  test_getBetweenRight(c(7, 9), right = c("]\\)", "&5"), insertSt = "four")
})
