library(shiny)
library(shinyjs)
library(ggvis)
shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("One-Sample Bootstrap"),
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
                              '"')
        ),
        actionButton("hideDataOptions", "Show/hide data set options")
      ),
      uiOutput("varChoose")
      ),
        conditionalPanel(
          "$('li.active a').first().html()==='Summaries'",
          h3("Control Panel"),
        radioButtons("plot", label=h4("Plotting"),
                     c("Histogram" = "his", "Kernel Density" = "den", "Histogram and Kernel Density" = "hisDen",
                       "Q-Q Plot" = "qq"), 
                     selected="his"),
        conditionalPanel(
          condition="input.plot=='hisDen'",
          sliderInput("w", 
                       label = "", 
                       value = 0.7, step=0.1, min = 0.1, max=3)
        ),
        conditionalPanel(
          condition="input.plot != 'hisDen'",
          uiOutput("origHist_ui")
        )
        ),
        conditionalPanel(
        "$('li.active a').first().html()==='Bootstrap'",
        h3("Control Panel"),
        radioButtons("plot2", label=h4("Plotting"),
                     c("Histogram" = "his2", "Kernel Density" = "den2", "Histogram and Kernel Density" = "hisDen2"), 
                     selected="his2"),
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
                     value = 0.95, min = 0.01, max = 0.99, step=0.01)
        )
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(type="tabs",  	     
                  tabPanel("Input",
                           actionButton("hideData", "Show/hide data set"),
                           hidden(
                             tableOutput("contents")
                           )
                           ),
                      tabPanel("Summaries",
                                  wellPanel(h3("Original Sample"),
                                            conditionalPanel(
                                              condition="input.plot=='hisDen'",
                                              plotOutput("hisDenPlot")
                                              ),
                                            conditionalPanel(
                                              condition="input.plot != 'hisDen'",
                                            ggvisOutput("origHist")
                                              ),
                                            h5("Original Summary Statistics"),
                                            h6("Summary"),
                                            tableOutput("summary"),
                                            h6("Standard Deviation"),
                                            verbatimTextOutput("sd")
                                            )
                           ),
                  tabPanel("Bootstrap",
                                  wellPanel(h3("Bootstrap Samples"),
                                            conditionalPanel(
                                              condition="input.plot2=='hisDen2'",
                                              plotOutput("hisDenPlot2")
                                            ),
                                            conditionalPanel(
                                              condition="input.plot2 != 'hisDen2'",
                                              ggvisOutput("bootHist")
                                            ),
                                            h5("Bootstrap Summary Statistics"),
                                            h6("Estimate Five-Number Summary"),
                                            tableOutput("bootSummary"),
                                            h6("Estimate Bias"),
                                            verbatimTextOutput("bootBias"),
                                            h6("Estimate Standard Deviation"),
                                            verbatimTextOutput("bootSd"),
                                            conditionalPanel(
                                              condition = "input.ci == 'perc'",
                                              h6("Two-Tailed Confidence Interval (Percentile)"),
                                              verbatimTextOutput("percPrint"),
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
                  )
      ) #tabset
    ) # mainPanel
  ) #sidebarLayout
) #fluidPage
) #shinyUI