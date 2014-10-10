library(shiny)

# in short, the story is: 
# I want to predict the "next" word a user will type. For this: 
# 1. I'll analyze a "big" body of text, in English - a book from Gutenberg, on gardening :)
# 2. I'll extract the list of words, for each will list the words which most often follow (3 
# words, decreasing prob)
# 3. When a user types a word and space, I'll check and present the list of 3 possible words. User can click on
# one of those. 

# 1. prepare the data, get the book
book <- "http://www.gutenberg.org/cache/epub/46052/pg46052.txt"
download.file(url=book, destfile="book.txt", method="curl", cacheOK=TRUE)
lines <- readLines("book.txt")
print(length(lines))
# lines <- c("line one", "line two", "gogo line two", "momo two", "momo one", "momo 3", "line momo two three")

# ----------------------------------  some initializations
hashmap <- list()
prev <- "xoxo"
hashmap[[prev]] <- list()


# 2. analyze the text, build the hashmap of words
for (i in 1:length(lines)){
    line <- lines[i]
    
    if (i%%25 == 0){
        print(paste("Line ", i, " out of ", length(lines)))
    }
    
    if (line==""){
        next
    }
    
    words <- unlist(strsplit(line, split=" "))
    for (j in 1:length(words)){
        word <- words[j]
        # cleanup the word, replace special characters
        word <- gsub("[<>,.()!?/\\:'\"]", "", word)
        word <- tolower(word)
        if (! (word %in% names(hashmap))){
            hashmap[[word]] <- list()
        }
        
        # we find the current word in the previous's list?
        if (! is.null(hashmap[[prev]][[word]])){
            counter <- hashmap[[prev]][[word]]
            hashmap[[prev]][[word]] <- counter + 1
        }
        # current word is not in the previous's list, so add it
        else {
            hashmap[[prev]][[word]] <- 1
        }
        
        prev <- word
    }
}

# re-order the hashmap by the freq/prob of the "following" words
nn <- names(hashmap)
for (i in 1:length(nn)) {
    name <- nn[i]
    print (name)
    elem <- hashmap[[name]]
    if (length(elem) > 0) {
        reord <- elem[order(unlist(elem), decreasing=T)]
        hashmap[[name]] <- reord
    }
}


# -------------------------------------- functions
# this function return the most probable n words coming after the given one. 
# will use hashmap for this (not to forget, hashmap[[]] elements are ordered). 
# returns a vector with the words. 
getMostProbable <- function(x, n) {
    words <- hashmap[[x]]
    if(n > length(words)) {
        return(names(words))
    }
    else {
        return(names(words[1:n]))
    }
}


# -------------------------------------- shiny
# the reactive part
shinyServer(
    function(input, output) {
        # the hello thingy
        output$nameecho <- renderText({paste("Hi ", input$name, "!")})
        
        # the suggestions for the next words
        output$suggest <- renderUI({
            inputtext <- input$input
            if (inputtext=="") {
                return()
            }
            # get the text from input, find last word, get the best 3 choices based on that
            inputwords <- unlist(strsplit(input$input, " "))
            # pick the last word
            inputlastword <- inputwords[length(inputwords)]
            print(inputlastword)
            options <- getMostProbable(inputlastword, 3)
            "selectInput (multi)" = selectInput("dynamic", "",
                                                choices = options,
                                                selected = options,
                                                multiple = TRUE
            )})
    }
)
