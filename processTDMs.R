## some helpers around TDM processing


## Java stuff
Sys.setenv(JAVA_HOME="")
options(java.parameters="-Xmx14g")

## load RWeka, tm libs
library(RWeka)
library(tm)

## load a previously saved TermDocumentMatrix
load("blog.tdm")
tdmMatrix <- as.matrix(tdm)

## get the sums over rows
aggblogs <- rowSums(tdmMatrix)

## save the vector, has as element names the N-grams (.tf stands for term frequencies)
save(aggblogs, file="aggblogs.tf")

