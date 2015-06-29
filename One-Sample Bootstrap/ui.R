library(shiny)
library(shinyjs)
library("ggvis")
shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("One-Sample Bootstrap"),
  sidebarLayout(
    sidebarPanel(
        radioButtons("chooseData", label=h5("Choose data set"),
                     c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                    selected = "uploadNo"),
        conditionalPanel(
          condition= "input.chooseData=='uploadYes'",
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
                       '"'),
          uiOutput("varChoose")
          ),
          actionButton("hideDataOptions", "Show/hide data set options")
        ), #conditionalPanel
          actionButton("hideData", "Show/hide data set"),
        h3("Bootstrap Control Panel"),
        radioButtons("plot", label=h4("Plotting"),
                     c("Histogram" = "his", "Kernel Density" = "den", "Histogram and Kernel Density" = "hisDen",
                       "Q-Q Plot" = "qq"), selected="his"),
        numericInput("w", 
                     label = h5("Original Bin Width"), 
                     value = 0.45, step=0.005, min = 0.005),
        numericInput("w2", 
                     label = h5("Bootstrap Bin Width"), 
                     value = 0.025, step=0.005, min = 0.005),
        h4("Resampling"),
        numericInput("num", 
                     label = h5("Number of Bootstraps"), 
                     value = 1000, min = 1, max = 100000),
        radioButtons("stat", label = h5("Bootstrap Statistic"),
                     c("Mean" = "bootMean", "Median" = "bootMedian", 
                       "Standard Deviation" = "bootSd"), selected = "bootMean"),
        radioButtons("ci", label = h5("Confidence Interval"),
                     c("Percentile" = "perc", "Normal-Based" = "norm"), selected = "perc"),
        numericInput("level", 
                     label = h5("Confidence Level"), 
                     value = 0.95, min = 0.01, max = 0.99, step=0.01),
        p("This might be a good place to put description, instead of having another column.")
    ), #sidebarPanel
    mainPanel(
                           column(5,
                                  wellPanel(h3("Original Sample"),
                                            plotOutput("origHist"),
                                            h5("Original Summary Statistics"),
                                            h6("Five-Number Summary"),
                                            verbatimTextOutput("summary"),
                                            h6("Standard Deviation"),
                                            verbatimTextOutput("sd")
                                            )
                           ),
                           column(7,
                                  wellPanel(h3("Bootstrap Samples"),
                                            plotOutput("bootHist"),
                                            h5("Bootstrap Summary Statistics"),
                                            h6("Estimate Five-Number Summary"),
                                            verbatimTextOutput("bootSummary"),
                                            h6("Estimate Bias"),
                                            verbatimTextOutput("bootBias"),
                                            h6("Estimate Standard Deviation"),
                                            verbatimTextOutput("bootSd"),
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
                                            ) #conditionalPanel
                                  ) #wellPanel
                           ),      #column
                           hidden(
                           tableOutput("contents")
                           )
    ) # mainPanel
  ) #sidebarLayout
) #fluidPage
) #shinyUI