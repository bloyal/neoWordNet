# test-graph-size.R
library(neoWordNet)
context("Test Graph Size")

checkGraph <- function(graph) {
  if (typeof(graph) != "list") {
    skip("Neo4j database not available")
  }
}

# get path to test data folder
testDictPath <- system.file("extdata", "data.adj", package = "neoWordNet")
testDictPath <- substr(testDictPath, 1, nchar(testDictPath) - 9)

# create Wordnet graph
graph <- createNeoWordNet(dictPath = testDictPath, verbose = FALSE, url = "http://localhost:7474/db/data/",
                          username = "neo4j", password = "graph")

test_that("The correct number of nodes have been created", {
  checkGraph(graph)
  expect_equal(countNodesbyLabel(graph, "LexName"), 45)
  expect_equal(countNodesbyLabel(graph, "VerbFrame"), 35)
  expect_equal(countNodesbyLabel(graph, "Synset"), 11)
  expect_equal(countNodesbyLabel(graph, "Word"), 16)
})

test_that("The correct number of relationships have been created", {
  checkGraph(graph)
  expect_equal(countRelationshipsByLabel(graph, "has_lexicographer_file"),
               11)
  expect_equal(countRelationshipsByLabel(graph, "has_synset"), 14)
  expect_equal(countRelationshipsByLabel(graph, "has_pointer"), 9)
  expect_equal(countRelationshipsByLabel(graph, "has_sentence_frame"),
               5)
})

