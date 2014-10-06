## various functions, run a model over the test data. Measures accuracy (hits/misses)...

loadTestFile <- function (fileName) {
  connection <- file(description = fileName, open = "rb", encoding = "UTF-8")
  allLines <- readLines(con = connection)
  allWords <- unlist(strsplit(x = allLines, split = " "))
  allWords <- allWords[which(allWords != "")]
  close(connection)
  return(allWords)
}

run1GramModel <- function(data, n, distribution) {
  start <- proc.time()
  ## data is a vector of words
  ## n is "how many high-freq words to try" (e.g. just the top probability, or the top 3?)
  len <- length(data)
  
  hits <- 0
  misses <- 0
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
    else
      misses <- misses + 1
    
  }
  
  ## how long it took?
  print(proc.time() - start)
  
  return(c("hits" = hits, "misses" = misses))
}
