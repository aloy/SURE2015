library(shiny)
library(shinyjs)
library(ggvis)

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Uploading Files"),
  sidebarLayout(
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
      ), #conditionalPanel
      uiOutput("boot"),
      uiOutput("groupChoose"),
      uiOutput("groupChoose2"),
      uiOutput("bootAgain"),
      actionButton("hideData", label="Show/hide data"),
      numericInput("num", 
                   label = h5("Number of Bootstraps"), 
                   value = 100, min = 1, max = 100000),
      sliderInput("width", label="Bin width", value=0.5, min=0, max=2, step=0.1)
    ), #sidebar
    mainPanel(
      tableOutput("contents"),
      plotOutput("plot"),
      htmlOutput("ggvisplot_ui"),
      tableOutput("statTest")
  )
  )
))