library(shiny)
library(shinyjs)
library(ggvis)

shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("Permutation for Regression"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "$('li.active a').first().html()==='Input'",
      radioButtons("chooseData", label=h5("Choose data set"),
                   c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                   selected = "uploadNo"),
      conditionalPanel(
        condition= "input.chooseData=='uploadYes'",
        tags$div(id="dataOptions",
                 fileInput('file1', 'Choose a file to upload.',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv',
                             '.tsv'
                           )
                 ),
                 h5("Data Set Options"),
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
        actionButton("hideDataOptions", "Show/hide data set options")
      ),#conditionalPanel
  selectInput('group', 'Grouping variable:' ,'group'),
  selectInput('response', 'Response variable:', 'response')
      ), #conditionalPanel
  conditionalPanel(
    "$('li.active a').first().html()==='Permutation Test'",
  numericInput("num", 
               label = "Permutation samples:", 
               value = 1000, min = 1, max = 10000),
  actionButton("goButton", "Permute!"),
  uiOutput("hist_ui")
  ) #conditionalPanel
    ), #sidebarPanel
  mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Input",
    actionButton("hideData", "Show/hide data set"),
    hidden(
      tableOutput("contents")
    )
                  ), #tabPanel
    tabPanel("Permutation Test",
    ggvisOutput("hist"),
    dataTableOutput("trials")
    ) #tabPanel
      ) #tabsetPanel
  ) #mainPanel
    ) #sidebarLayout
))