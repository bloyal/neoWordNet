library(neoWordNet)

context("Test Graph Size")

checkGraph <- function(graph) {
  if (typeof(graph) != "list") {
    skip("Neo4j database not available")
  }
}

countNodesbyLabel <- function(graph, label) {
  nodes <- getLabeledNodes(graph, label)
  return(length(nodes))
}

countRelationshipsByLabel <- function(graph, label) {
  query <- paste("MATCH (a)-[r]->(b) WHERE type(r)='", label, "' RETURN r",
                 sep = "")
  nodes <- getRels(graph, query)
  return(length(nodes))
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
  expect_equal(countRelationshipsByLabel(graph, "HAS_LEX_FILE"),
               11)
  expect_equal(countRelationshipsByLabel(graph, "HAS_SYNSET"), 14)
  expect_equal(countRelationshipsByLabel(graph, "HAS_POINTER"), 9)
  expect_equal(countRelationshipsByLabel(graph, "HAS_SENTENCE_FRAME"),
               5)
})

# test-get-word-info.R
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

