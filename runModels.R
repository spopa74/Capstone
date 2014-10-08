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




## -------------- Simple 1-Gram Model -----------------------
## Have the data loaded, have the "distribution" of 1-Grams 
## (similar to the frequency counts), just go through data and 
## compare the word with the highly probable one (in this case, 
## "the", ..). Count the "hits" and "misses" (and the words not
## found at all in the dictionary). 
## data - the data to be tested with, a vector of words
## n - how many possibilities (1 or 3) to try
## tf1sorted - the term frequency for 1-grams, ordered by frequency
run1GramModel <- function(data, n, tf1sorted) {
  start <- proc.time()
  ## data is a vector of words
  ## n is "how many high-freq words to try" (e.g. just the top probability, or the top 3?)
  len <- length(data)
  
  hits <- 0
  misses <- 0
  notin <- 0   ## words which are not in dictionary
  ## in case of 1-grams, the things to compare are static - the highest probable words
  possibilities <- head(names(distribution), n + 1)
  
  for(i in 1:len){
    currentWord <- data[i]
    
    ## just a print to see that the code is running
    if(i %% 100000 == 0) {
      print(paste("Word: ", currentWord, ", index: ", i))
    }
    
    if (currentWord %in% possibilities)
      hits <- hits + 1
    else {
      if (is.na(tf1sorted[currentWord]))
        notin <- notin + 1
      else
        misses <- misses + 1
    }
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses, "notin" = notIn))
}


## -------------- CREATE 2-GRAM model ---------------------

## INPUT: 
## tf1Sorted - term frequencies for 1-Grams, sorted decreasingly by frequency
## tf2Sorted - same, for 2-Grams
## OUTPUT: 
## 
create2GramModel <- function(tf1Sorted, tf2Sorted, n) {
  start <- proc.time()
  
  len1 <- length(tf1Sorted)
  len2 <- length(tf2Sorted)
  
  ## create 2 vectors for tf1 (term freq 1-grams)
  ## indexes (1-gram to index) and frequencies (index to frequency)
  print("break 1-grams in indexes and frequencies")
  indexes1Gram <- seq.int(1, length(tf1Sorted))
  names(indexes1Gram) <- names(tf1Sorted)
  tf1Sorted <- unname(tf1Sorted)
  
  ## break the 2-grams vector into 2 vectors, prior and posterior
  print("break 2-grams names in 2 vectors, prior and posterior")
  allWords2Grams <- unlist(strsplit(names(tf2Sorted), split=" "))
  ## now we have a concatenated list of priors and posteriors
  priorWords <- allWords2Grams[seq(1, 2 * len2, 2)]
  posteriorWords <- allWords2Grams[seq(2, 2* len2, 2)]
  remove(allWords2Grams)
  ## get the indexes of above words, from tf1sorted
  priorIx <- indexes1Gram[priorWords]
  posteriorIx <- indexes1Gram[posteriorWords]
  
  ## we have the indexes, frequencies, figure out the best possibilities
  print("create the sparse matrix with the model")
  bigramModel <- Matrix(0, nrow=len1, ncol=len1, sparse = TRUE)  
  bigramModel[priorIx, posteriorIx] <- tf2Sorted / tf1Sorted[priorIx]
  
  print(proc.time() - start)
  return(bigramModel)
  
}

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
## NOTE!!!!!!!
## DO NOT USE, WAY TOO SLOW FOR A REASONABLE SIZE VECTOR (10 million 2-Grams)
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
