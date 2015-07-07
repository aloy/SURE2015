library(shiny)
library(shinyjs)
shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Two-Sample Bootstrapping"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "$('li.active a').first().html()==='Input'",
        radioButtons("chooseData2", label=h5("Choose data set"),
                     c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                     selected = "uploadNo"),
        conditionalPanel(
          condition= "input.chooseData2=='uploadYes'",
          tags$div(id="dataOptions",
                   fileInput('file1', 'Choose a file to upload. The data set will appear below the main panel.',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )
                   ),
                   h4("Data Set Options"),
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
          ), #divid
          actionButton("hideDataOptions", "Show/hide data set options")
        ), #conditionalpanel
        selectInput('group', 'Grouping variable:' ,'group'),
        selectInput('response', 'Response variable:', 'response')
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Summaries'",
        h3("Bootstrap Control Panel"),
        radioButtons("plot", label=h4("Plotting"),
                     c("Histogram" = "his", "Kernel Density" = "den", "Histogram and Kernel Density" = "hisDen",
                       "Q-Q Plot" = "qq"), selected="his"),
          numericInput("w", 
                       label = h5("One-Variable Bin Width"), 
                       value = 1.3, step=0.1, min = 0.1)
        ),
      conditionalPanel(
        "$('li.active a').first().html()==='Bootstrap'",
        h3("Bootstrap Control Panel"),
        h4("Resampling"),
        numericInput("num", 
                     label = h5("Number of Bootstraps"), 
                     value = 1000, min = 1, max = 100000),
        radioButtons("stat", label = h5("Statistic"),
                     c("Difference of Means" = "bootMean", "Difference of Medians" = "bootMedian", 
                       "Ratio of Means" = "bootMeanRatio", "Ratio of Medians" = "bootMedRatio",
                       "Ratio of Standard Deviations" = "bootSdRatio"), selected = "bootMean"),
        radioButtons("plot2", label=h4("Plotting"),
                     c("Histogram" = "his2", "Kernel Density" = "den2", "Histogram and Kernel Density" = "hisDen2",
                       "Q-Q Plot" = "qq2"), selected="his2"),
        h5("Bootstrap Bin Width"),
        conditionalPanel(
          condition="input.plot2=='hisDen2'",
          sliderInput("w2", 
                      label = "",
                      value = 0.02, step=0.005, min = 0.005, max=0.1)
        ),
        conditionalPanel(
          condition="input.plot2 != 'hisDen2'",
          uiOutput("bootHist_ui")
        ),
        actionButton("goButton", "Permute!")
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Confidence Intervals'",
        radioButtons("ci", label = h5("Confidence Interval"),
                     c("Percentile" = "perc", "Normal-Based" = "norm"), selected = "perc"),
        numericInput("level", 
                     label = h5("Confidence Level"), 
                     value = 0.95, min = 0.01, max = 0.99, step=0.01)
      )
    ),
    mainPanel(title="Two-Sample Bootstrap",
              tabsetPanel(type="tabs",
                          tabPanel("Input",
                                   actionButton("hideData", "Show/hide data set"),
                                   hidden(
                                     tableOutput("contents")
                                   )
                                   ),
                          tabPanel("Summaries",
                         h3("One-Variable Statistics"),
                                  plotOutput("origHist"),
                                  h5("Original Summary Statistics"),
                                  tableOutput("basicSummary")
                    ), #tabPanel
                    tabPanel("Bootstrap",
                       h3("Bootstrap Samples"),
                       conditionalPanel(
                         condition="input.plot2=='hisDen2'",
                         plotOutput("hisDenPlot2")
                       ),
                       conditionalPanel(
                         condition="input.plot2 != 'hisDen2'",
                         ggvisOutput("bootHist")
                       ),
                       h6("Estimate Five-Number Summary"),
                       tableOutput("bootSummary"),
                       h6("Estimate Bias"),
                       verbatimTextOutput("bootBias"),
                       h6("Estimate Standard Deviation"),
                       verbatimTextOutput("bootSd")
                    ),
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
                    )
              ) #tabsetPanel
  ) # mainPanel
) #sidebarLayout
) #fluidPage
) #shinyUI