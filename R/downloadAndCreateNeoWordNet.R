# downloadAndCreateNeoWordNet.R source('downloadWordNetFiles.R');
# source('createNeoWordNet.R');

downloadAndCreateNeoWordNet <- function(url = "http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz", 
                                        dest = getwd(), username = "neo4j", password = "graph", verbose = TRUE) {
  # Test before creating entire graph!
  runIntegrationTests()
  dictPath <- downloadWordNetFiles(url = url, dest = dest, verbose = verbose)
  createWordNetGraph(dictPath = dictPath, verbose = verbose)
}
