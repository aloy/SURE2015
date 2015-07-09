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
      )#conditionalPanel
 ), #conditionalPanel
 conditionalPanel(
   "$('li.active a').first().html()==='Summaries'",
   selectInput('x', 'Variable 1:','x'),
   selectInput('y', 'Variable 2:', 'y'),
   hidden(
     shiny::p(id = "warning", strong("Make sure you select different variables!"),  style = "color:red")
   )
   ),
  conditionalPanel(
    "$('li.active a').first().html()==='Permutation Test'",
    h3("Resampling Control Panel"),
    radioButtons("plot", label=h4("Plotting"), c("Histogram"="his", "Density"="den", "Q-Q Plot" = "qq"),
                 selected="his"),
  numericInput("num", 
               label = h5("Permutation Samples"), 
               value = 1000, min = 1, max = 10000),
  actionButton("goButton", "Permute!"), actionButton("reset", "Reset"),
  h5("Histogram Bin Width"),
  uiOutput("hist_ui")
  ), #conditionalPanel
  conditionalPanel(
    "$('li.active a').first().html()==='Confidence Intervals'",
    radioButtons("ci", label = h5("Confidence Interval"),
                 c("Percentile" = "perc", "Normal-Based" = "norm"), selected = "perc"),
    numericInput("level", 
                 label = h5("Confidence Level"), 
                 value = 0.95, min = 0.01, max = 0.99, step=0.01)
  )
    ), #sidebarPanel
  mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Input",
     dataTableOutput("contents")
                  ), #tabPanel
    tabPanel("Summaries",
             ggvisOutput("origPlot"),
             verbatimTextOutput("origSummary")
             ),
    tabPanel("Permutation Test",
    ggvisOutput("hist"),
    tableOutput("summary"),
    actionButton("hideData", "Show/hide data set"),
    hidden(
    dataTableOutput("trials")
    )
    ), #tabPanel
    tabPanel("Confidence Intervals",
             conditionalPanel(
               condition = "input.ci == 'perc'",
               h6("Two-Tailed Confidence Interval (Percentile)"),
               verbatimTextOutput("ciPrint"),
               h6("Lower Bound"),
               verbatimTextOutput("percLower"),
               h6("Upper Bound"),
               verbatimTextOutput("percUpper")
             ),
             conditionalPanel(
               condition = "input.ci == 'norm'",
               h6("Two-Tailed Confidence Interval (Normal)"),
               verbatimTextOutput("normPrint"),
               h6("Lower Bound"),
               verbatimTextOutput("normLower"),
               h6("Upper Bound"),
               verbatimTextOutput("normUpper")
             )             
    ) #tabPanel
      ) #tabsetPanel
  ) #mainPanel
    ) #sidebarLayout
))