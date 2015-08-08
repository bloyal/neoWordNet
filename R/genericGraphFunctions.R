# genericGraphFunctions.R Generic helper functions for creating a graph
# db in Neo4j

# Start empty Neo4j graph
newGraph <- function(url = "http://localhost:7474/db/data/", username = "neo4j", 
                     password = "graph") {
  graph <- startGraph(url, username, password)
  clear(graph, input = FALSE)
  return(graph)
}

# Helper function to optimize Neo4j transaction sizes May want to try
# and vectorize this
bulkGraphUpdate <- function(graph, data, creationFunction, transactionMax = 1000, 
                            verbose = TRUE) {
  t <- newTransaction(graph)
  transactionCounter <- 0
  for (i in 1:nrow(data)) {
    transactionCounter <- transactionCounter + 1
    creationFunction(t, data[i, ])
    if (transactionCounter == transactionMax) {
      commit(t)
      t <- newTransaction(graph)
      transactionCounter <- 0
    }
    if (verbose) {
      printIteration(i, nrow(data), 1000)
    }
  }
  commit(t)
}

# Helper function to print out how far along we are in a loop
printIteration <- function(currentVal, maxVal, divisor) {
  if (currentVal%%divisor == 0) {
    print(paste(round((currentVal/maxVal) * 100), "% Complete", sep = ""))
  }
}

# This is an example function used with bulkGraphUpdate() to add lots
# of nodes and/or relationships at once
createExampleNodes <- function(transaction, data) {
  query <- "CREATE (a:Node {startNodeId:{startNodeId}})-[b:HAS_REL {relId:{relId}}]->(c:Node {endNodeId:{endNodeId}})"
  print(query)
  appendCypher(transaction, query, startNodeId = data$START_NODE_ID, 
               relId = data$REL_ID, endNodeId = data$END_NODE_ID)
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