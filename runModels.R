 ## various functions, run a model over the test data. Measures accuracy (hits/misses)...

library(Matrix)
library(compiler)


## ----------------- Load the test file ---------------------------
## The test file is big enough, don't want to re-load it every time. 
## fileName - the name of the file to be tested with. 
loadTestFile <- function (fileName) {
  connection <- file(description = fileName, open = "rb", encoding = "UTF-8")
  allLines <- readLines(con = connection)
  allWords <- unlist(strsplit(x = allLines, split = " "))
  allWords <- allWords[which(allWords != "")]
  close(connection)
  return(allWords)
}



## ------------------- Find x highest freq ------------------

## Small helper function - returns a list of N indexes for repeating elements in a vector
## FAST TILL ABOUT 200,000 elements, after that the findIndexesLoop underneath is (much)
## faster. 
findIndexes <- function(v, n = NA) {
  uni <- unique(v)
  if(is.na(n))
    ret <- lapply(uni, FUN = function(x) {which(v == x)})
  else
    ret <- lapply(uni, FUN = function(x) {head(which(v == x), n)})
  
  return(list(uni, ret))
}

## same version as above, in a loop. Returns a different structure, though. 
findIndexesLoop <- function(v, n = NA) {
  ## something to return
  indexes <- list()
  lengths <- list()
  start <- proc.time()
  
  ## parse the elements of the given vector, put them in the right places
  for(i in 1:length(v)) {
    ## get current element    
    elem <- v[i]

    ## just a print to see that the code is running
    if(i %% 100000 == 0) {
      print(proc.time() - start)
      print(paste("Elem: ", elem, ", index: ", i))
    }
    
    lElem <- indexes[elem]
    
    ## skip after n
    if(!is.na(n))
      if(length(lElem[[1]]) == n)
        next
    
    if (is.null(lElem[[1]])){
      indexes[elem][[1]] <- vector(mode = "integer")
      indexes[elem][[1]][[1]] <- i
      lengths[elem][[1]] <- 1
    }
    else {
      len <- lengths[elem][[1]]
      ## len <- length(indexes[elem][[1]])
      indexes[elem][[1]][[len + 1]] <- i
      lengths[elem][[1]] <- len + 1
    }
  }
  
  return(indexes)
}


## take a data.table sorted by priors, mark the first 3 for each prior. 
## v is a vector of priors
## n is how many to keep
markFirstN <- function(v, n) {
  start <- proc.time()
  
  result <- as.logical(vector(length = length(v)))
  
  currentPrior <- v[1]
  howManyThisPrior <- 0
  nextLine <- 1
  for(i in 1:length(v)) {

    ## just a print to see that the code is running
    if(i %% 100000 == 0) {
      print(proc.time() - start)
      print(paste(", index: ", i))
    }
    
    rowPrior <- v[i]
    
    if(rowPrior != currentPrior){
      currentPrior <- rowPrior
      howManyThisPrior <- 0
    }
    
    if(howManyThisPrior == n){
      result[i] <- FALSE
      next
    } 
        
    result[i] <- TRUE
    howManyThisPrior <- howManyThisPrior + 1
    nextLine <- nextLine + 1
    
  }
  return(result)
}

## compile above
cmpMarkFirstN <- cmpfun(markFirstN)


## function for finding the high freq lines. Vectorial, fairly slow for big data sets. 
## INPUTS: 
## priorIx - the vectors with indexes to priors, ordered decreasingly by freq
## postIx - the vectors with indexes to posteriors, ordered decreasingly by freq
## n - how many postIx to return in the result, for each priorIx
## OUTPUT: 
## model - list with priors and highest N posteriors, for each unique prior
findHighestNFreq <- function(priorIx, postIx, n) {

  indexes <- findIndexes(priorIx, n)
  uniquePres <- indexes[1][[1]]
  indexesPres <- indexes[2][[1]]
  if (length(uniquePres[[1]]) != length(indexesPres[[1]]))
    print("ERROR! The lengths of priors uniques and indexes are not equal!")
  
  highestPosts <- lapply(indexesPres, FUN = function(x) {postIx[indexesPres[x][[1]]]})
  return(list(uniquePres, highestPosts))
}

## Maps the set of priors to a set of posteriors. 
## INPUTS: 
## indexesPrior - the model returned by findIndexesLoop function above
## postIx - the vectors with indexes to posteriors, ordered decreasingly by freq
## OUTPUT: 
## model - list with priors and highest N posteriors, for each unique prior
findPostFromPriors <- function(indexesPrior, postIx) {
  ## indexesPrior is a list of lists
  ## each element index is the index in priors, each corresponding list is the list of indexes where that prior was
  indexesPost <- list()
  start <- proc.time()
  for(i in 1:length(indexesPrior)) {
    
    ## just a print to see that the code is running
    if(i %% 100000 == 0) {
      print(proc.time() - start)
      print(paste("index: ", i))
    }
    
    indexesPost[i][[1]] <- postIx[indexesPrior[i][[1]]]
  }
  return(indexesPost)
}



## -------------- Simple 1-Gram Model -----------------------
## Have the data loaded, have the "distribution" of 1-Grams 
## (similar to the frequency counts), just go through data and 
## compare the word with the highly probable one (in this case, 
## "the", ..). Count the "hits" and "misses" (and the words not
## found at all in the dictionary). 
## data - the data to be tested with, a vector of words
## n - how many possibilities (1 or 3) to try
## model - the data.table for 1-Grams, ordered by probability
run1GramModel <- function(data, n, model1) {
  
  grams <- model1$grams
  
  start <- proc.time()
  ## data is a vector of words
  ## n is "how many high-freq words to try" (e.g. just the top probability, or the top 3?)
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0   ## words which are not in dictionary
  ## in case of 1-grams, the things to compare are static - the highest probable words
  possibilities <- head(grams, n + 1)
  
  for(i in 1:len){
    currentWord <- data[i]
    
    ## just a print to see that the code is running
    if(i %% 100 == 0) {
      print(paste("Word: ", currentWord, ", index: ", i))
    }
    
    if (currentWord %in% possibilities)
      hits <- hits + 1
    else {
      if (is.na(grams[currentWord]))
        notin <- notin + 1
      else
        misses <- misses + 1
    }
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notin))
}

## compile the above
cmpRun1GramModel <- cmpfun(run1GramModel)



## Return a 1-Gram
get2GramPosts <- function(g1Gram, n, model2, model1) {
  l <- list()
  index <- -1
  ## if param is a word, not an index, make it an index
  if (is.character(g1Gram)) {
    ## find index first
    
  }
  
  ## maybe didn't find it
  if (index == -1)
    return(l)
  
  dt <- model2[index==g1Gram]
  
  return(l)
}

cmpGet2GramPosts <- cmpfun(get2GramPosts)


## Run the 2-Gram Model 
## Parameters: 
## data - vector of words to be tested with
## n - number of possibilities to check
## model - the data.table with the 2-Grams, prior and posterior, 3 options for each prior
run2GramModel <- function(data, n, model2, model1) {
  start <- proc.time()
  
  ## if interested in only 1 possibility, filter the model2
  if (n == 1)
    model2 <- model2[!duplicated(model2$prior),]
  
  ## index model1 by grams
  setkey(model2, grams)
  setkey(model1, grams)
  
  ## data is a vector of words
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0
  
  for(i in 1:len-1) {
    ## just a print to see that the code is running
    if(i %% 100 == 0) {
      print(proc.time() - start)
      print(paste("hits: ", hits, ", misses: ", misses, ", notin: ", notin, ", index: ", i))
    }
    
    ## get the current and next words
    currentWord <- data[i]
    nextWord <- data[i+1]
    
    ## create a bigram
    g2Gram <- paste(currentWord, nextWord)
    
    ## check if we have it in our 2-Gram model
    dt <- model2[grams == g2Gram]
    
    ## interpret
    if (length(dt$index) > 0)
      hits <- hits + 1
    else { ## why wasn't found? Prior, or posterior, not in the models?
      ## find the prior
      mdt <- model1[grams == currentWord]
      if (length(mdt$index) == 0)
        notin <- notin + 1
      else
        misses <- misses + 1
    }
    
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notin))
}

## compile the above
cmpRun2GramModel <- cmpfun(run2GramModel)



## Run the 3-Gram Model 
## Parameters: 
## data - vector of words to be tested with
## n - number of possibilities to check
## model3, 2 - the data.table with the 3-, 2-Grams, prior and posterior, 3 options for each prior, etc
run3GramModel <- function(data, n, model3, model2) {
  start <- proc.time()
  
  ## if interested in only 1 possibility, filter the model3
  if (n == 1)
    model3 <- model3[!duplicated(model3$prior),]
  
  ## index model1 by grams
  setkey(model3, grams)
  setkey(model2, grams)
  
  ## data is a vector of words
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0
  
  for(i in 1:(len-2)) {
    ## just a print to see that the code is running
    if(i %% 100 == 0) {
      print(proc.time() - start)
      print(paste("hits: ", hits, ", misses: ", misses, ", notin: ", notin, ", index: ", i))
    }
    
    ## get the current and next words
    currentWord <- data[i]
    nextWord <- data[i+1]
    nextNextWord <- data[i+2]
    
    ## create a trigram
    g2Gram <- paste(currentWord, nextWord)
    g3Gram <- paste(g2Gram, nextNextWord)
    
    ## check if we have it in our 3-Gram model
    dt <- model3[grams == g3Gram]
    
    ## interpret
    if (length(dt$index) > 0)
      hits <- hits + 1
    else { ## why wasn't found? Prior, or posterior, not in the models?
      ## find the prior
      mdt <- model2[grams == g2Gram]
      if (length(mdt$index) == 0)
        notin <- notin + 1
      else
        misses <- misses + 1
    }
    
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notin))
}

## compile the above
cmpRun3GramModel <- cmpfun(run3GramModel)




## Run the 4-Gram Model 
## Parameters: 
## data - vector of words to be tested with
## n - number of possibilities to check
## model4, 3 - the data.table with the 4-, 3-Grams, prior and posterior, 3 options for each prior, etc
run4GramModel <- function(data, n, model4, model3) {
  start <- proc.time()
  
  ## if interested in only 1 possibility, filter the model4
  if (n == 1)
    model4 <- model4[!duplicated(model4$prior),]
  
  ## index model1 by grams
  setkey(model4, grams)
  setkey(model3, grams)
  
  ## data is a vector of words
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0
  
  for(i in 1:(len-3)) {
    ## just a print to see that the code is running
    if(i %% 100 == 0) {
      print(proc.time() - start)
      print(paste("hits: ", hits, ", misses: ", misses, ", notin: ", notin, ", index: ", i))
    }
    
    ## get the current and next words
    currentWord <- data[i]
    nextWord <- data[i+1]
    nextNextWord <- data[i+2]
    nextNextNextWord <- data[i+3]  ## :)
    
    ## create a trigram, fourgram
    g3Gram <- paste(currentWord, nextWord, nextNextWord)
    g4Gram <- paste(g3Gram, nextNextNextWord)
    
    ## check if we have it in our 4-Gram model
    dt <- model4[grams == g4Gram]
    
    ## interpret
    if (length(dt$index) > 0)
      hits <- hits + 1
    else { ## why wasn't found? Prior, or posterior, not in the models?
      ## find the prior
      mdt <- model3[grams == g3Gram]
      if (length(mdt$index) == 0)
        notin <- notin + 1
      else
        misses <- misses + 1
    }
    
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notin, "percentage" = hits / (hits + misses + notin)))
}

## compile the above
cmpRun4GramModel <- cmpfun(run4GramModel)



## Run the 5-Gram Model 
## Parameters: 
## data - vector of words to be tested with
## n - number of possibilities to check
## model5, 4 - the data.table with the 5-, 4-Grams, prior and posterior, 3 options for each prior, etc
run5GramModel <- function(data, n, model5, model4) {
  start <- proc.time()
  
  ## if interested in only 1 possibility, filter the model4
  if (n == 1)
    model5 <- model5[!duplicated(model5$prior),]
  
  ## index model1 by grams
  setkey(model5, grams)
  setkey(model4, grams)
  
  ## data is a vector of words
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0
  
  for(i in 1:(len-3)) {
    ## just a print to see that the code is running
    if(i %% 100 == 0) {
      print(proc.time() - start)
      print(paste("hits: ", hits, ", misses: ", misses, ", notin: ", notin, ", index: ", i))
    }
    
    ## get the current and next words
    currentWord <- data[i]
    nextWord <- data[i+1]
    nextNextWord <- data[i+2]
    nextNextNextWord <- data[i+3]  ## :)
    nextNextNextNextWord <- data[i+4]  
    
    ## create a four, fivegram
    g4Gram <- paste(currentWord, nextWord, nextNextWord, nextNextNextWord)
    g5Gram <- paste(g4Gram, nextNextNextNextWord)
    
    ## check if we have it in our 4-Gram model
    dt <- model5[grams == g5Gram]
    
    ## interpret
    if (length(dt$index) > 0)
      hits <- hits + 1
    else { ## why wasn't found? Prior, or posterior, not in the models?
      ## find the prior
      mdt <- model4[grams == g4Gram]
      if (length(mdt$index) == 0)
        notin <- notin + 1
      else
        misses <- misses + 1
    }
    
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notin, "percentage" = hits / (hits + misses + notin)))
}

## compile the above
cmpRun5GramModel <- cmpfun(run5GramModel)


## Trivial backoff. Take all 5-grams, check with the 5-Gram model. 
## If the prior not there, take the 4-Gram of the current position, 
## continue to 3,2,1. See the word suggestion, and compare with the 
## next word. 
runTrivialBackoff <- function(data, n, model5, model4, model3, model2, model1) {
  
  ## just to make sure I can search quickly by prior. 
  setkey(model5, grams)
  setkey(model4, grams)
  setkey(model3, grams)
  setkey(model2, grams)
  setkey(model1, grams)
  
  ## small cheat. Will start with the 4-th word in the data, so will 
  ## skip the first 4 comparisons. Considering that the data is 350,000 
  ## long, skipping 4 shouldn't change drastically the result. 
  
  for (i in 4:(length(data) - 1)) {
    wordi <- data[i]
    wordiMin1 < data[i-1]
    wordiMin2 < data[i-2]
    wordiMin3 < data[i-3]
    
    
  }
  
  
}






## ---------------- various trials, not really used -------------

## DO NOT USE! IMPOSSIBLY SLOW
## Based on the sorted counts of 1- and 2-Grams, create the 
## structure for holding the best N possibilities following
## a certain word. Since I'm using it with 1 or 3, the structure
## should contain a vector of vectors. Each name in a vector is
## an index in 1-gram, and each element is a list of 3 possible
## followups, ordered decreasingly by probability. Quick note: 
## since the probability is given by the MLE, then I'll take 
## into consideration only the counts. 
## 
## RETURNS: 
## Sparse matrix of:
## row - index in the 1-gram
## column - 3 possible next words (as indexes in 1-gram)
## value at row + column - probability
## 
## PARAMS:
## counts2Gram - a term frequency model of the 2-Grams, sorted by frequency
## indexes1Gram - a vector with 1-Grams and indexes
## freq1Gram - a vector with indexes and frequencies for 1-Grams
## n - how many possibilities for the "next word" (1 or 3 in this case)
create2GramModelSLOW <- function(counts2Gram, indexes1Gram, freq1Gram, n) {
  start <- proc.time()
  ## loop through the 2-grams
  len2g <- length(counts2Gram)
  len1g <- length(indexes1Gram)
  
  model <- Matrix(0, nrow = len1g, ncol = len1g, sparse = TRUE)
  
  for (i in 1:len2g) {
    ## just a print to see that the code is running
    if(i %% 10000 == 0) {
      print(paste("2-Gram: ", counts2Gram[i], ", index: ", i))
    }
    
    ## get the current 2-gram
    nameGram <- names(counts2Gram[i])
    ## this is a 2-gram, so there is a space
    words <- unlist(strsplit(nameGram, split = " "))
    indexWord1 <- indexes1Gram[words[1]]
    indexWord2 <- indexes1Gram[words[2]]
    
    ## check if we have already 3 values in the respective line
    filled <- slotsFilled[indexWord1, 1]
    if (filled == n)
      next
    
    ## if we're here, slot not full yet
    probability <- counts2Gram[i] / tf1freq[indexWord1]
    model[indexWord1, indexWord2] <- probability
    slotsFilled[indexWord1, 1] <- filled + 1
  }
  
  print(proc.time() - start)
  return(model)
}
