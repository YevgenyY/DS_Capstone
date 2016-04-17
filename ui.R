library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Illustrating input"),
  sidebarPanel(
    numericInput("id1", "numeric input, labeled id1", 0, min=0, max=10, step=1),
    checkboxGroupInput("id2", "Checkbox",
                       c("Value 1" = "1",
                         "Value 2" = "2",
                         "Velue 3" = "3")),
    dateInput("date", "Date:")
  ),
  mainPanel(
    h3('Ilustrating ouput'),
    h4("you entered"),
    verbatimTextOutput("oid1"),
    h4("you entered"),
    verbatimTextOutput("oid2"),
    h4("you entered"),
    verbatimTextOutput("odate")
  )
))










