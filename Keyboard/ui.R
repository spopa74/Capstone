library(shiny)
shinyUI(fluidPage(
    responsive = TRUE,
    
    titlePanel(windowTitle = "Predictive keyboard", title = "Predictive Keyboard"),
    
    sidebarLayout(position = "left",
                  sidebarPanel("",
                            helpText("Input any text you want in this text box"),
                            tagList(
                                tags$textarea(id="inputText", rows=10, cols=20, "")
                            ),
                            radioButtons(inputId = "options", label = "Suggestions for next word", choices = c("", "", "")), 
                            actionButton(inputId = "addChoice", label = "Add choice")
                  ), 
                  mainPanel(
                            helpText("This is model of a predictive keyboard. As the user starts writing ",
                                     "text in the input box at the left, the system tries to suggest the ", 
                                     "most probable following words. Clicking on one of the suggestions ", 
                                     "presented should update the text in the input box with the selected", 
                                     "choice. The system will also try to track the accuracy of the predictor ", 
                                     "by counting the number of words and how many times the user does choose ", 
                                     "one of the presented options. English please..."),
                            br(),
                            textOutput(outputId = "captureWriting"), 
                            textOutput(outputId = "captureChangeOption"), 
                            textOutput(outputId = "captureAddChoice"), 
                            
                            br(),
                            br(),
                            br(),
                            em("Capstone project, Data Science Specialization at Coursera"), 
                            br(),
                            em("Sebastian Popa, oct. 2014")
                  )
                 )
            )
)
