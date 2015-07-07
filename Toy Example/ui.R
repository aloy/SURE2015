library(shiny)
library(shinyjs)
library(ggvis)

shinyUI(pageWithSidebar(
  div(),
  sidebarPanel(
    radioButtons("chooseData", label=h5("Choose data set"),
                 c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                 selected = "uploadNo"),
    conditionalPanel(
      condition= "input.chooseData=='uploadYes'",
      fileInput('file1', 'Choose file to upload. If you have already permuted, restart the app first.',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    selectInput('group', 'Grouping variable:' ,'group'),
    selectInput('response', 'Response variable:', 'response'),
#     radioButtons("type", label="Plot Type", c("Histogram" = "his", "Density" = "den"), selected="his"),
    numericInput("n", 
                 label = "Permutation samples", 
                 value = 1000, min = 1, max = 10000),
    uiOutput("varChoose"),
    uiOutput("varChoose2"),
    actionButton("goButton", "Permute!"), actionButton("goButton2", "Reset"),
    uiOutput("plot_ui")
  ),

  mainPanel(
    h3("Permutation Distribution"),
    ggvisOutput("plot"),
    h3("Summary Statistics"),
    tableOutput("stats"),
    dataTableOutput("trials")
  )
))