# getWordInfo.R

#' Get word information from a Wordnet graph
#'
#' \code{getWordInfo} queries a WordNet graph for information about a single
#' word. This includes a list of senses and the lexicographer file, pointers,
#' and related words for each.
#' @param graph A graph object created using \code{createNeoWordNet},
#'   \code{downloadAndCreateNeoWordNet}, or by starting an already-created
#'   WordNet graph instance (i.e. using the \code{startGraph} function from the
#'   RNeo4j package).
#' @param word The English word to query for.
#' @export
getWordInfo <- function(graph, word) {

  #these variable assignments are only to get around an annoying devtools note - ignore
#   .<-NULL
   pos<-NULL
   sense<-NULL
   pointer<-NULL
   summarise<-NULL
   pointerWord<-NULL

  # get semantic pointer info
  query <- "match (a:Word {name:{word}})-[r:has_synset]->(b:Synset),
  (b)-[r2:has_pointer]->(c)<-[r3:has_synset]-(d:Word)
  return a.name as word, b.posName as pos, r.wordNum as sense, b.gloss as gloss, b.lexFileName as lexFile, r2.pointerType as pointer, r2.relation as pointerType, d.name as pointerWord
  order by a.name, b.posName, r.wordNum, r2.pointerType, r3.wordNum"
  semanticResults <- unique(cypher(graph, query, word = word))
  if (!is.null(semanticResults)) {
    semanticResults <- merge(semanticResults, plyr::ddply(semanticResults, plyr::.(pos,sense,pointer),
                                                          plyr::summarise, pointerWords = paste0(pointerWord,
                                                          collapse = ", ")), by = c("pos", "sense", "pointer"))
    results <- unique(semanticResults[, c("word", "pos", "sense", "gloss",
                                          "lexFile", "pointer", "pointerType", "pointerWords")])
  }
  # get lexical pointer info
  query <- "match (a:Word {name:{word}})-[r:has_synset]->(b:Synset),\n          (a)-[r2:has_pointer]->(c:Word)\n            return a.name as word, b.posName as pos,r.wordNum as sense, b.gloss as gloss, b.lexFileName as lexFile, r2.pointerType as pointer, r2.relation as pointerType, c.name as pointerWord\n            order by a.name, b.posName,r.wordNum, r2.pointerType"
  lexResults <- unique(cypher(graph, query, word = word))
  if (!is.null(lexResults)) {
    lexResults <- merge(lexResults, plyr::ddply(lexResults, plyr::.(pos, sense,
                                                        pointer), plyr::summarise, pointerWords = paste0(pointerWord, collapse = ", ")),
                        by = c("pos", "sense", "pointer"))
    lexResults <- unique(lexResults[, c("word", "pos", "sense", "gloss",
                                        "lexFile", "pointer", "pointerType", "pointerWords")])
    results <- rbind(results, lexResults)
  }
  if (is.null(semanticResults) & is.null(lexResults)){
    return(NULL)
    }
  else{
    return(results[order(results$word, results$pos, results$sense, results$pointerType,
                       results$pointer), ])
  }
}
