## generic function, runs a model over the test data. Measures accuracy...

runModel <- function(model, data) {
  ## data is a file, for simplicity
  connection <- file(description = data, open = "rb", encoding = "UTF-8")
  allLines <- readLines(con = connection)
  allWords <- unlist(strsplit(x = allLines, split = " "))
  
}