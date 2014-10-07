## various functions, run a model over the test data. Measures accuracy (hits/misses)...


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
## Based on the sorted counts of 1- and 2-Grams, create the 
## structure for holding the best N possibilities following
## a certain word. Since I'm using it with 1 or 3, the structure
## should contain a vector of vectors. Each name in a vector is
## an index in 1-gram, and each element is a list of 3 possible
## followups, ordered decreasingly by probability. Quick note: 
## since the probability is given by the MLE, then I'll take 
## into consideration only the counts. 
## Structure returned is a vector of:
## name = index in the 1-gram
## vector = vector of 3 possible next words (ordered by probability)
## Parameters:
## counts2Gram - a term frequency model of the 2-Grams, sorted by frequency
## indexes1Gram - a vector with 1-Grams and indexes
## freq1Gram - a vector with indexes and frequencies for 1-Grams
## n - how many possibilities for the "next word" (1 or 3 in this case)
create2GramModel <- function(counts2Gram, indexes1Gram, freq1Gram, n) {
  start <- proc.time()
  model <- vector()
  
  ## loop through the 2-grams
  len2g <- length(counts2Gram)
  for (i in 1:len2g){
    nameGram <- names(counts2Gram[i])
    ## this is a 2 gram, so there is a space
    words <- unlist(strsplit(nameGram, split = " "))
    word1 <- words[1]
    word2 <- words[2]
    
    ## check if the structure has a vector associated with word1 
    vecWord1 <- model[word1]
    if (is.na(vecWord1)){
      vecWord1 <- vector()
      model[word1] <- vecWord1
    }
    
    ## check if the respective word doesn't have already the slots taken. 
    ## since the model is created based on a decreasingly sorted initial
    ## vectors, if the slots are taken then they are the ones with the
    ## biggest probabilities, so ignore the rest. 
    if(length(vecWord1) == n)
      continue
    
    ## if we're here, slot not full yet
    indexWord1 <- tf1sortedIndexes[word1]
    model[word1] <- c(vecWord1, c(indexes1Gram[word2], counts2Gram[i] / tf1freq[indexWord1])
    
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
  
  for(i in 1:len){
    
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
