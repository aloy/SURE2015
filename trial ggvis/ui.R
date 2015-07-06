library(shiny)
library(shinyjs)
library(ggvis)

shinyUI(pageWithSidebar(
  div(),
  sidebarPanel(
    fileInput('datfile', ''),
    selectInput('group', 'Grouping variable:' ,'group'),
    selectInput('response', 'Response variable:', 'response'),
    numericInput("n", 
                 label = "Permutation samples", 
                 value = 1000, min = 1, max = 10000),
    actionButton("goButton", "Permute!"),
    br(),
    sliderInput("w", "Binwidth", min = .1, max = 5,
                value = .5, step = .1)
  ),
  mainPanel(
    h3("Permutation Distribution"),
    #uiOutput("plot_ui"),
    ggvisOutput("plot"),
    dataTableOutput("trials")
  )
))