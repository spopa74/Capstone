library(shiny)

# in short, the story is: 
# I want to predict the "next" word a user will type. For this: 
# 1. I'll analyze a "big" body of text, in English - a book from Gutenberg, on gardening :)
# 2. I'll extract the list of words, for each will list the words which most often follow (3 
# words, decreasing prob)
# 3. When a user types a word and space, I'll check and present the list of 3 possible words. User can click on
# one of those. 

# 1. prepare the data, get the book
## book <- "http://www.gutenberg.org/cache/epub/46052/pg46052.txt"
## download.file(url=book, destfile="book.txt", method="curl", cacheOK=TRUE)
## lines <- readLines("book.txt")
## print(length(lines))
lines <- c("line one", "line two", "gogo line two", "momo two", "momo one", "momo 3", "line momo two three")

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

lastInputAction <- c(0,0,0)
lastOptions <- c()

# -------------------------------------- shiny
# the reactive part
shinyServer(
    function(input, output, clientData, session) {
        
        # the suggestions for the next words
        output$suggestButtons <- renderUI({
            inputtext <- input$inputText
            if (inputtext=="") {
                return()
            }
            
            # get the text from input, find last word, get the best 3 choices based on that
            inputwords <- unlist(strsplit(input$inputText, " "))
            # pick the last word
            inputlastword <- inputwords[length(inputwords)]
            print(inputlastword)
            # find the best 3 options
            options <- getMostProbable(inputlastword, 3)
            # save the options in a temp var
            ##length(lastOptions) <- 0
            ##lastOptions <- options
            # render the buttons with the options
            choices <- list()
            if (! is.null(options))
                for (i in 1:length(options)) {
                    ## create and add buttons to the list, will return the list in the end
                    choices[[i]] <- actionButton(inputId = paste("option",i, sep = ""), label = options[i])
                }
            # return the list of buttons
            return(choices)
        })
        
        # You can access the value of the buttons with input$id_of_button, e.g.
        output$captureOptionClick <- renderPrint({ 
            
            print(paste(input$option1, input$option2, input$option3))
            
            newOption1 <- input$option1
            if (! is.null(newOption1)) {
                if (newOption1 > lastInputAction[1]) {
                    ## YES! this button was pressed
                    lastInputAction[1] <- newOption1
                    return(paste("Option1 pressed", lastOptions[1]))
                }
            }
            newOption2 <- input$option2
            if (! is.null(newOption2)) {
                if (newOption2 > lastInputAction[2]) {
                    ## YES! this button was pressed
                    lastInputAction[2] <- newOption2
                    return(paste("Option2 pressed", lastOptions[2]))
                }
            }
            newOption3 <- input$option3
            if (! is.null(newOption3)) {
                if (newOption3 > lastInputAction[3]) {
                    ## YES! this button was pressed
                    lastInputAction[3] <- newOption3
                    return(paste("Option3 pressed", lastOptions[3]))
                }
            }
        }
        )
    }
)
