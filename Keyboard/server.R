library(shiny)


## ---------------------------------- functions ----------------------------
## this function return the most probable n words coming after the given one. 
## returns a vector with the words. 
getMostProbable <- function(x, n) {
    return(c("one", "two", "three"))
}


## -------------------------------------- shiny -----------------------------
## the reactive part
shinyServer(func = function(input, output, clientData, session) {

        ## SESSION SCOPED VARIABLES 

        ## current options on the buttons
        currentOptions <- c()
        currentOption <- ""
        
        ## save the last text in the input, use it as flag (at least)
        lastText <- ""
        
        ## hits/totalWords will give the accuracy
        hits <- 0
        totalWords <- 0
        
        
        ## the suggestions for the next words
        output$captureWriting <- renderText({
            inputText <- input$inputText
            
            ## return if nothing in the box
            if (inputText=="") return("Input text rendered...")
            if (inputText == lastText) return("Input text rendered...")

            ## save in variable
            lastText <<- inputText
            
            ## get the text from input, find last word, get the best 3 choices based on that
            inputWords <- unlist(strsplit(input$inputText, " "))
            ## pick the last word
            inputLastWord <- inputWords[length(inputWords)]
            print(paste("lastInput", inputLastWord))
            ## find the best 3 options
            currentOptions <<- getMostProbable(inputLastWord, 3)
            if (! is.null(currentOptions) && length(currentOptions) > 0)
                currentOption <<- currentOptions[1]
            ## update the checkboxes
            updateRadioButtons(session, inputId = "options", choices = currentOptions, selected = currentOption)
            "Input text rendered..."
        })
        
        
        ## listen for clicking in the radio widget
        output$captureChangeOption <- renderText({
            currentOption <<- input$options
            "Radiobuttons rendered..."
        })
        
        
        ## you can access the value of the buttons with input$id_of_button, e.g.
        output$captureAddChoice <- renderText({ 
            
            ## make this code depend on the button click
            buttonClicked <- input$addChoice
            
            print(paste("current option in AddChoice: ", currentOption))
            
            if (is.null(currentOption) || length(currentOption) == 0) return
            
            ## get the choices selected
            if (! is.null(currentOption) && (nchar(currentOption) == 1) && (currentOption %in% c("d", "t"))) {
                    newText <- paste(lastText, "'", currentOption, sep = "")
                    lastText <<- newText
                    updateTextInput(session = session, inputId = "inputText", value = newText)
            }
            else { 
                if (! is.null(currentOption) && (nchar(currentOption) == 2) && (currentOption %in% c("ll", "nt"))) {
                        newText <- paste(lastText, currentOption, sep = "")
                        lastText <<- newText
                        updateTextInput(session = session, inputId = "inputText", value = newText)
                }
                else {
                    newText <- paste(lastText, currentOption)
                    lastText <<- newText
                    updateTextInput(session = session, inputId = "inputText", value = newText)
                }
            }
            "Button rendered..."
        })
    }
)