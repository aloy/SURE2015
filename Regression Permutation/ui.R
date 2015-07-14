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
    radioButtons("plot", label=h4("Plotting"), c("Histogram"="his", "Density"="den", 
                "Histogram and Kernel Density" = "hisDen","Q-Q Plot" = "qq"),
                 selected="his"),
  numericInput("num", 
               label = h5("Permutation Samples"), 
               value = 1000, min = 1, max = 10000),
  hidden(
    shiny::p(id = "warning2", strong("Make sure you select different variables on the Summaries tab!"),  
             style = "color:red")
  ),
  actionButton("goButton", "Permute!"), actionButton("reset", "Reset"),
  h5("Histogram Bin Width"),
  conditionalPanel(
    condition="input.plot=='hisDen'",
    sliderInput("w", 
                label = "",
                value =3.5, step=0.01, min = 0.01, max=7.5)
  ),
  conditionalPanel(
    condition="input.plot != 'hisDen'",
    uiOutput("hist_ui")
  ),
  radioButtons("test", label=h5("Permutation Test"), c("Two-Tailed" = "tt", "Lower Tail" = "lt", "Upper Tail" = "ut"), 
               selected="tt")
  ), #conditionalPanel
  conditionalPanel(
    "$('li.active a').first().html()==='Confidence Intervals'",
    radioButtons("stat", label=h5("Statistic"), c("Slope (β̂)"="slope", "ŷ"="yhat"), selected="slope"),
    radioButtons("ci", label = h5("Type of Interval"),
                 c("Percentile Confidence" = "perc", "Normal-Based Confidence" = "norm",
                   "Prediction Interval for ŷ" = "ypred"), selected = "perc"),
    numericInput("R", label="Number of bootstrap samples", value=1999),
    numericInput("level", 
                 label = h5("Confidence Level"), 
                 value = 0.95, min = 0.01, max = 0.99, step=0.01),
    conditionalPanel(
      condition="input.stat=='yhat'",
    numericInput("xval", "Value of X", value=0)
    )
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
             conditionalPanel(
               condition="input.plot=='hisDen'",
               plotOutput("hisDen")
             ),
             conditionalPanel(
               condition="input.plot != 'hisDen'",
               ggvisOutput("hist")
             ),
    h6("Summary"),
    tableOutput("summary"),
    h6("P-Value"),
    verbatimTextOutput("pval"),
    actionButton("hideData", "Show/hide data set"),
    hidden(
    dataTableOutput("trials")
    )
    ), #tabPanel
    tabPanel("Confidence Intervals",
             conditionalPanel(
               condition = "input.ci == 'perc'",
               h4("Percentile-Based"),
               h6("Two-Tailed Confidence Interval"),
               verbatimTextOutput("ciPrint"),
               h6("One-Tailed Confidence Intervals (Lower, Upper)"),
               verbatimTextOutput("percOneTail")
             ),
             conditionalPanel(
               condition = "input.ci == 'norm'",
               h4("Normal-Based"),
               h6("Two-Tailed Confidence Interval"),
               verbatimTextOutput("normPrint"),
               h6("One-Tailed Confidence Intervals (Lower, Upper)"),
               verbatimTextOutput("normOneTail")
             ),
             conditionalPanel(
               condition = "input.ci == 'ypred'",
               h4("Prediction Interval"),
               conditionalPanel(
                 condition = "input.stat == 'slope'",
                 p(strong("Please select the correct statistic."),  style = "color:red")
               ),
               conditionalPanel(
                 condition = "input.stat == 'yhat'",
               h6("Two-Tailed Prediction Interval"),
               verbatimTextOutput("predInt"),
               h6("One-Tailed Prediction Intervals (Lower, Upper)"),
               verbatimTextOutput("predOneTail")
               )
             )
    ) #tabPanel
      ) #tabsetPanel
  ) #mainPanel
    ) #sidebarLayout
))