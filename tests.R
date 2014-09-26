start <- proc.time()

tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 5))

en_texts <- VCorpus(DirSource(directory="data/en_US/small", encoding="UTF-8"), 
                    readerControl=list(language="en"))

en_texts <- tm_map(x=en_texts, FUN=removeNumbers)
en_texts <- tm_map(x=en_texts, FUN=removePunctuation)
en_texts <- tm_map(x=en_texts, FUN=tolower)

## options(mc.cores=1)
tdm <- TermDocumentMatrix(en_texts, control=list(tokenizer=tokenizer))

## show the running time
proc.time() - start


