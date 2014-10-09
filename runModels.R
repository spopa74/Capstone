 ## various functions, run a model over the test data. Measures accuracy (hits/misses)...

library(Matrix)


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
findIndexes <- function(v, n) {
  uni <- unique(v)
  ret <- lapply(uni, FUN = function(x) {head(which(v == x), n)})
  return(list(uni, ret))
}

findIndexesLoop <- function(v, n) {

  ## something to return
  indexes <- list()
  
  for(i in 1:length(v)) {
    ## get current element    
    elem <- v[i]

    ## just a print to see that the code is running
    if(i %% 100000 == 0) {
      print(paste("Elem: ", elem, ", index: ", i))
    }
    
    lElem <- indexes[elem]
    
    ## skip after n
    if(length(lElem[[1]]) == n)
      next
    
    if (is.null(lElem[[1]]))
      indexes[elem] <- list(i)
    else
      indexes[elem][[1]] <- append(x = indexes[elem][[1]], values = i)
  }
  
  return(indexes)
}


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


## -------------- Simple 1-Gram Model -----------------------
## Have the data loaded, have the "distribution" of 1-Grams 
## (similar to the frequency counts), just go through data and 
## compare the word with the highly probable one (in this case, 
## "the", ..). Count the "hits" and "misses" (and the words not
## found at all in the dictionary). 
## data - the data to be tested with, a vector of words
## n - how many possibilities (1 or 3) to try
## tf1sorted - the term frequency for 1-grams, ordered by frequency
run1GramModel <- function(data, n, words1Gram) {
  start <- proc.time()
  ## data is a vector of words
  ## n is "how many high-freq words to try" (e.g. just the top probability, or the top 3?)
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0   ## words which are not in dictionary
  ## in case of 1-grams, the things to compare are static - the highest probable words
  possibilities <- head(names(words1Gram), n + 1)
  
  for(i in 1:len){
    currentWord <- data[i]
    
    ## just a print to see that the code is running
    if(i %% 100 == 0) {
      print(paste("Word: ", currentWord, ", index: ", i))
    }
    
    if (currentWord %in% possibilities)
      hits <- hits + 1
    else {
      if (is.na(words1Gram[currentWord]))
        notin <- notin + 1
      else
        misses <- misses + 1
    }
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notin))
}


## -------------- CREATE 2-GRAM model ---------------------

## INPUT: 
## prior2Ix - vector of indexes of 2Grams priors, sorted decreasingly by frequency
## post2Ix - vector of indexes of 2-Grams posts, also sorted...
## OUTPUT: 
## 
create2GramModel <- function(prior2Ix, post2Ix, n) {
  start <- proc.time()

  ## makes use of findHighestFreq function above
  bigramModel <- findHighestNFreq(prior2Ix, post2Ix, n)
  
  print(proc.time() - start)
  return(bigramModel)
  
}



## Run the 2-Gram Model, relies on the model created by the function above. 
## Parameters: 
## data - vector of words to be tested with
## n - number of possibilities to check
## model - the model created above
## tf2sorted - the 2-Grams, sorted by frequency
run2GramModel <- function(data, n, model, tf2sorted) {
  start <- proc.time()
  ## data is a vector of words
  ## n is "how many high-freq words to try" (e.g. just the top probability, or the top 3?)
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0
  
  for(i in 1:len) {
    ## get the current and next words
    currentWord <- data[i]
    if (i<len) 
      nextWord <- data[i+1]
    
    ## find the possibilities for the current word
    possibilities <- model[currentWord]
    if(n == 1)
      possibilities <- c(possibilities[1][1],possibilities[1][2]) 
    
    ## just a print to see that the code is running
    if(i %% 100000 == 0) {
      print(paste("Word: ", currentWord, ", index: ", i))
    }
    
    ## check to see if the next word is in the vector of possibilities
    if (nextWord %in% possibilities)
      hits <- hits + 1
    else
      if (is.na(tf2sorted[paste(currentWord, " ", nextWord)]))
        notin <- notin + 1
    else
      misses <- misses + 1
    
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notin))
}


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
