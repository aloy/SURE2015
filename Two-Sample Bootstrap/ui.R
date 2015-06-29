library(shiny)
library(shinyjs)
shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Two-Sample Bootstrapping"),
  sidebarLayout(
    sidebarPanel(
        radioButtons("chooseData2", label=h5("Choose data set"),
                     c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                     selected = "uploadNo"),
        conditionalPanel(
          condition= "input.chooseData2=='uploadYes'",
          tags$div(id="dataOptions2",
                   fileInput('file2', 'Choose a file to upload. The data set will appear below the main panel.',
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
                   checkboxInput('header2', 'Header', TRUE),
                   radioButtons('sep2', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                ','),
                   radioButtons('quote2', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                '"'),
                   uiOutput("varChoose2"),
                   uiOutput("varChoose3")
          ), #divid
          actionButton("hideDataOptions2", "Show/hide data set options")
        ), #conditionalpanel
        actionButton("hideData2", "Show/hide data set"),
        checkboxInput("one", "Show one-variable statistics"),
        h3("Bootstrap Control Panel"),
        radioButtons("plot2", label=h4("Plotting"),
                     c("Histogram" = "his2", "Kernel Density" = "den2", "Histogram and Kernel Density" = "hisDen2",
                       "Q-Q Plot" = "qq2"), selected="his2"),
        conditionalPanel(
          condition = "input.one == true",
          numericInput("w3", 
                       label = h5("One-Variable Bin Width"), 
                       value = 1.3, step=0.1, min = 0.1)
        ),
        numericInput("w4", 
                     label = h5("Bootstrap Bin Width"), 
                     value = 0.2, step=0.05, min = 0.05),
        h4("Resampling"),
        numericInput("num2", 
                     label = h5("Number of Bootstraps"), 
                     value = 1000, min = 1, max = 100000),
        radioButtons("stat2", label = h5("Statistic"),
                     c("Difference of Means" = "bootMean2", "Difference of Medians" = "bootMedian2", 
                       "Ratio of Means" = "bootMeanRatio", "Ratio of Medians" = "bootMedRatio",
                       "Ratio of Standard Deviations" = "bootSdRatio"), selected = "bootMean2"),
        radioButtons("ci2", label = h5("Confidence Interval"),
                     c("Percentile" = "perc2", "Normal-Based" = "norm2"), selected = "perc2"),
        numericInput("level2", 
                     label = h5("Confidence Level"), 
                     value = 0.95, min = 0.01, max = 0.99, step=0.01),
        p("More description about two-sample stuff.")
      ),
    mainPanel(title="Two-Sample Bootstrap",
                       conditionalPanel(
                         condition = "input.one == true",
                         h3("One-Variable Statistics"),
                         fluidRow(
                           column(6,
                                  plotOutput("origHist2")
                           ), #column
                           column(6,
                                  h5("Original Summary Statistics"),
                                  verbatimTextOutput("basicSummary")
                           ) #column
                         ) #fluidRow
                       ), #conditionalPanel
                       h3("Bootstrap Samples"),
                       plotOutput("bootHist2"),
                       h6("Estimate Five-Number Summary"),
                       verbatimTextOutput("bootSummary2"),
                       h6("Estimate Bias"),
                       verbatimTextOutput("bootBias2"),
                       h6("Estimate Standard Deviation"),
                       verbatimTextOutput("bootSd2"),
                       conditionalPanel(
                         condition = "input.ci2 == 'perc2'",
                         h6("Two-Tailed Confidence Interval (Percentile)"),
                         verbatimTextOutput("ciPrint2"),
                         h6("Lower Bound"),
                         verbatimTextOutput("percLower2"),
                         h6("Upper Bound"),
                         verbatimTextOutput("percUpper2")
                       ),
                       conditionalPanel(
                         condition = "input.ci2 == 'norm2'",
                         h6("Two-Tailed Confidence Interval (Normal)"),
                         verbatimTextOutput("normPrint2"),
                         h6("Lower Bound"),
                         verbatimTextOutput("normLower2"),
                         h6("Upper Bound"),
                         verbatimTextOutput("normUpper2")
                       ),
                       hidden(
                         tableOutput("contents2")
                       )
  ) # mainPanel
) #sidebarLayout
) #fluidPage
) #shinyUI