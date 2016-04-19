library(shiny)

shinyUI(fluidPage(
  titlePanel("Word prediction algorithm demo"),
  fluidRow(
    column(3, wellPanel(
      textInput("input_text", "Query", "Your sentence here..."),
      submitButton("Submit")
    )
  )),
  column(6,
    h4("you entered"),
    verbatimTextOutput("out_sentence"),
    h4("predicted word"),
    verbatimTextOutput("out_word_predicted")
  )
))










