library(shiny)
shinyUI(fluidPage(
    titlePanel("Here I predict YOU!"),
    
    sidebarLayout(position = "left",
                  sidebarPanel("",
                               helpText("This is an exercise in 'prediction'. I'll", 
                                        "take a body of text, extract the words from", 
                                        "it, I'll organize a hashmap with the 'most ", 
                                        "probable' words following a certain word, and", 
                                        "present those words as options. For example, one", 
                                        " would expect 'the' to follow an 'in' typed IN ", 
                                        "THE input box. Nothing much can be done with the", 
                                        "options presented, though..."),
                               br(),
                               helpText("ALSO, THERE SEEMS TO BE AN ISSUE WITH", 
                                        " GETTING THE TEXT OVER INTERNET, THE ", 
                                        "DOWNLOAD ON THE SHINYAPPS SERVER DOESN'T ", 
                                        "SEEM TO WORK, IT WOULD WORK ON A NORMAL", 
                                        "MACHINE CONNECTED TO INTERNET - ANYWAY, ", 
                                        "TRY IN THE INPUT WORDS LIKE 'THE' OR ", "
                                        'ERROR' !!! :("),
                               br(),
                               p("Hi, what's your name?"),
                               textInput(inputId="name", label="", value=""),
                               
                               helpText("Input any text you want in this text box,", 
                                        "The options to the right are based on the", 
                                        "last word input in this text box. English ", 
                                        "please..."),
                               textInput(inputId="input", label="", value="")
                               ),
                  mainPanel("",
                            textOutput(outputId="log"),
                            br(),
                            textOutput(outputId="nameecho"),
                            br(),
                            br(),
                            p("This is where I 'guess' the next possible words:"),
                            uiOutput(outputId="suggest")
                            )
                 )
            )
)
