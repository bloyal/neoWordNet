# test-get-word-info.R
library(neoWordNet)
context("Test Get Word Info")

# create Wordnet graph
graph <- startGraph(url = "http://localhost:7474/db/data/",
                          username = "neo4j", password = "graph")

word <- getWordInfo(graph, "whoop")
test_that("The correct records have been returned ", {
  expect_equal(nrow(word), 1)
  expect_match(word$pointer, "Hypernym")
  expect_match(word$pointerType, "Semantic")
  expect_match(word$pointerWords, "cough")
})

# clear graph
clear(graph, input = FALSE)
