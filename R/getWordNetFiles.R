# getWordNetFiles.R

#' Download WordNet database files
#'
#' \code{getWordNetFiles.R} downloads and processes WordNet index and
#' data files for later load into Neo4j
#' @param wordNetUrl URL for the desired version of the WordNet database files.
#'   Defaults to version 3.1.
#' @param dest Destination path for WordNet files.
#' @param verbose Should the function provide progress details? Defaults to
#'   true.
#' @export
getWordNetFiles <- function(wordNetUrl = "http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz",
                                 dest = getwd(), verbose = TRUE) {
  if (verbose) {
    print(paste("Downloading dictionary files from ", wordNetUrl, sep = ""))
  }
  destFile <- paste(dest, "wn.dict.tar.gz", sep = "/")
  download.file(wordNetUrl, destFile)

  if (verbose) {
    print("Unzipping dictionary files")
  }
  R.utils::gunzip(destFile)
  tarFile <- substr(destFile, 0, nchar(destFile) - 3)
  untar(tarFile)

  if (verbose) {
    print("Cleaning up temporary files")
  }
  unlink(destFile)
  unlink(tarFile)
  dictFolder <- paste(dest, "dict", sep = "/")
  unlink(paste(dictFolder, "sents.vrb", sep = "/"))
  unlink(paste(dictFolder, "sentidx.vrb", sep = "/"))
  unlink(paste(dictFolder, "verb.Framestext", sep = "/"))
  unlink(paste(dictFolder, "cntlist", sep = "/"))
  unlink(paste(dictFolder, "cntlist.rev", sep = "/"))
  unlink(paste(dictFolder, "adv.exc", sep = "/"))
  unlink(paste(dictFolder, "cousin.exc", sep = "/"))
  unlink(paste(dictFolder, "noun.exc", sep = "/"))
  unlink(paste(dictFolder, "verb.exc", sep = "/"))
  unlink(paste(dictFolder, "adj.exc", sep = "/"))
  unlink(paste(dictFolder, "log.grind.3.1", sep = "/"))
  unlink(paste(dictFolder, "index.sense", sep = "/"))
  unlink(paste(dictFolder, "dbfiles", sep = "/"), recursive = TRUE)
  return(dictFolder)
}
