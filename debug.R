## debug - something is wrong with the 1-grams: 
## no "a", "i", and so on...

findWords <- function(v, w) {
  
  l <- nchar(w)
  names <- names(v)
  r <- vector()
  
  for (word in names){
    if (substr(word, 1, l) == w){
      r <- append(r, word)
    }
  }
  
  return(r)
}

