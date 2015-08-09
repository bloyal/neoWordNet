library(testthat)
library(neoWordNet)

test_check("neoWordNet")

#get path to test data folder
testDictPath<-system.file("extdata", "data.adj", package = "neoWordNet")
testDictPath<-substr(testDictPath,1, nchar(testDictPath)-9)

context("WordNet Data Read")
wordNetData <- readPOSdata(testDictPath, verbose=FALSE)
test_that("WordNet data  files are read correctly", {
  expect_equal(nrow(wordNetData$noun), 3)
  expect_equal(nrow(wordNetData$verb), 3)
  expect_equal(nrow(wordNetData$adj), 3)
  expect_equal(nrow(wordNetData$adv), 2)
})

context("WordNet Index Read")
wordFrame <- readPOSWordIndex(testDictPath, verbose=FALSE)
test_that("WordNet index files are read  correctly", {
  expect_equal(nrow(wordFrame), 59)
})

context("Semantic Pointer Data")
pointerFrame <- ldply(lapply(wordNetData, getSynsetPointerFrame))
test_that("Semantic pointer data is processed correctly", {
  expect_equal(nrow(pointerFrame), 115)
})

context("Lexical Pointer Data")
wordFrame2 <- ldply(lapply(wordNetData, getWordFrame))
lexPointerFrame <- getLexicalPointerWords(pointerFrame[pointerFrame$startWordNum !=
                                                         0, ], wordFrame2)
test_that("Lexical pointer data is processed correctly", {
  expect_equal(nrow(pointerFrame), 9)
})

context("Verb Frame Data")
verbFrameFrame <- ldply(apply(wordNetData$verb, 1, transformSynsetDataToFrameMap))
test_that("Verb frame data is processed correctly", {
  expect_equal(nrow(verbFrameFrame), 5)
})
