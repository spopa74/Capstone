start <- proc.time()

## Java stuff
Sys.setenv(JAVA_HOME="")
options(java.parameters="-Xmx14g")

## load RWeka, tm libs
library(RWeka)
library(tm)

## create a tokenizer for up to 5-Grams
tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))                                                      
en_texts <- VCorpus(DirSource(directory="clean_test/", encoding="UTF-8"))

## create the TermDocumentMatrix
tdm <- TermDocumentMatrix(en_texts, control=list(tokenizer=tokenizer))

## show the running time
proc.time() - start


