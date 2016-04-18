library(shiny)
source("code/final/0_stats_helper.R")
#load(file="data/f12.Rda")
#load(file="data/f12345raw.Rda")
#load(file="data/f21_22_23_24.Rda")

shinyServer(
  function(input, output) {
    output$out_sentence <- renderText({input$input_text})
    output$out_word_predicted <- renderText({paste("input text is:", input$input_text)})
  }
)

