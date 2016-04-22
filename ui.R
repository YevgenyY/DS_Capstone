library(shiny)

shinyUI(fluidPage(
  titlePanel("Word prediction algorithm demo"),
  fluidRow(
    column(6, wellPanel(
      textInput("input_text", "Query", "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"),
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










