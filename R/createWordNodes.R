# createWordNodes.R source('createWordNetNeo4j.R');

readPOSWordIndex <- function(dictPath, verbose = TRUE) {
  if (verbose) {
    print(paste(Sys.time(), "Reading POS word index files", sep = ": "))
  }
  advData <- readAdvWordData(dictPath, verbose)
  verbData <- readVerbWordData(dictPath, verbose)
  adjData <- readAdjWordData(dictPath, verbose)
  nounData <- readNounWordData(dictPath, verbose)
  list(adv = advData, verb = verbData, adj = adjData, noun = nounData)
}

readAdvWordData <- function(path = "~/Downloads/WordNet-3.0/dict", verbose = TRUE) {
  if (verbose) {
    print(paste(Sys.time(), "Reading noun word data", sep = ": "))
  }
  path <- paste(path, "index.adv", sep = "/")
  readPosWordDataFile(path, "r")
}

readVerbWordData <- function(path = "~/Downloads/WordNet-3.0/dict", verbose = TRUE) {
  if (verbose) {
    print(paste(Sys.time(), "Reading noun word data", sep = ": "))
  }
  path <- paste(path, "index.verb", sep = "/")
  readPosWordDataFile(path, "v")
}

readAdjWordData <- function(path = "~/Downloads/WordNet-3.0/dict", verbose = TRUE) {
  if (verbose) {
    print(paste(Sys.time(), "Reading noun word data", sep = ": "))
  }
  path <- paste(path, "index.adj", sep = "/")
  readPosWordDataFile(path, "a")
}

readNounWordData <- function(path = "~/Downloads/WordNet-3.0/dict", verbose = TRUE) {
  if (verbose) {
    print(paste(Sys.time(), "Reading noun word data", sep = ": "))
  }
  path <- paste(path, "index.noun", sep = "/")
  readPosWordDataFile(path, "n")
}

readPosWordDataFile <- function(path = "~/Downloads/WordNet-3.0/dict/data.verb", 
                                posCode = "n") {
  posData <- readLines(path)
  posData <- posData[30:length(posData)]
  processWordData(posData, posCode)
}

processWordData <- function(wordData, posCode) {
  # print(wordData);
  words <- ldply(str_match_all(wordData, "^(\\S+) ([arnv] .+ \\d) (\\d{8}.*)"))[, 
                                                                                c(2, 4)]
  names(words) = c("name", "synsetOffset")
  # print(words);
  ldply(apply(words, 1, function(line) {
    # print(line['synsetOffset']);
    synsetOffsets <- str_match_all(line["synsetOffset"], "(\\S{8})")[[1]][, 
                                                                          1]
    df <- data.frame(name = str_replace_all(str_to_lower(line["name"]), 
                                            "_", " "), synsetId = calcSynsetId(synsetOffsets, posCode), 
                     stringsAsFactors = FALSE, row.names = NULL)
    cbind(df, wordNum = as.numeric(rownames(df)))
  }))
}

createWordNodes <- function(graph, wordFrame, verbose = TRUE) {
  addIndex(graph, "Word", "name")
  # print(typeof(wordFrame));
  if (verbose) {
    print(paste(Sys.time(), "Creating word nodes", sep = ": "))
  }
  bulkGraphUpdate(graph, wordFrame, createSingleWordNode)
  if (verbose) {
    print(paste(Sys.time(), "Creating synset-word relationships", sep = ": "))
  }
  bulkGraphUpdate(graph, wordFrame, createSingleSynsetWordRelationship)
}

createSingleWordNode <- function(transaction, data) {
  query <- "MERGE (:Word {name:{name}})"
  appendCypher(transaction, query, name = data$name)
}

createSingleSynsetWordRelationship <- function(transaction, data) {
  # print(data) query <- 'MATCH (a:Synset {synsetOffset:{synsetOffset},
  # pos:{pos}}), (b:Word {name:{name}}) CREATE (a)-[:has_word
  # {wordNum:{wordNum}}]->(b)';
  query <- "MATCH (a:Word {name:{name}}), (b:Synset {synsetId:{synsetId}})\n  CREATE (a)-[:has_synset {wordNum:{wordNum}}]->(b)"
  appendCypher(transaction, query, synsetId = data$synsetId, name = data$name, 
               wordNum = data$wordNum)
}