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
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
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
    numericInput("n", 
                 label = "Permutation samples", 
                 value = 1000, min = 1, max = 10000),
    sliderInput("w", "Binwidth", min = .1, max = 5,
                value = .5, step = .1),
    uiOutput("varChoose"),
    uiOutput("varChoose2")
  ),

  mainPanel(
    tableOutput("test"),
    h3("Permutation Distribution"),
    uiOutput("plot_ui"),
    ggvisOutput("plot"),
    h3("Summary Statistics"),
    tableOutput("stats")
  )
))