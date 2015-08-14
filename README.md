# neoWordNet

[WordNet](https://wordnet.princeton.edu/wordnet/) is an open-source lexical database of the English language. It structures words together into groups of cognitive synonyms, or "synsets", representing a unique concept. The neoWordNet package allows you to easily create your own WordNet instance in [Neo4j](http://neo4j.com/), a leading graph database.

## Contents

* [Install](#install)
* [Start Neo4j](#start-neo4j)
* [Download Database Files and Create WordNet Graph](#create-graph)
* [Get Word Information](#get-word-info)

##  <a name="#install"></a>Install

```r
install.packages("devtools")
devtools::install_github("bloyal/neoWordNet")
library(neoWordNet)
```

## <a name="#start-neo4j"></a>Start Neo4j

All neoWordNet functions require you to first download, install, and start a Neo4j instance. Please refer to the Neo4j [download page](http://neo4j.com/download/) for more information.

## <a name="#create-graph"></a>Download Database Files and Create WordNet Graph

Use the `getAndCreateNeoWn` function to download the required database files and create a Wordnet graph all at once. The function returns a `graph` object that can be used with Nicole White's [RNeo4j]("https://github.com/nicolewhite/RNeo4j") package (included with neoWordNet) to query the database.

```r
graph <- getAndCreateNeoWn(dest="Users/Brian/Downloads", url = "http://localhost:7474/db/data/", username="neo4j",password="password")
```
## <a name="#get-word-info"></a>Get Word Information

neoWordNet comes with a basic function for returning sense information and related terms for a given word. 

```r
getWordInfo(graph, word="hound")
```
```
##  word  pos   sense gloss ...
##  hound Noun  1     any of several breeds of dog...
##  hound Noun  2     someone who is morally reprehensible...
##  hound Verb  3     pursue or chase relentlessly...
```
