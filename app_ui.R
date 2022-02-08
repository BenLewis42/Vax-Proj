library(shiny)

#summary <- tabPanel(output$test)


ui <- basicPage(
  textInput("txt", "Enter the text to display below:"),
  textOutput("text"),
  verbatimTextOutput("verb")
                 
)
