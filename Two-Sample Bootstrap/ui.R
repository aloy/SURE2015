library(shiny)
library(shinyjs)
library(ggvis)

shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("Two-Sample Bootstrapping"),
  tags$div(class = "header", 
           p("This app was created by Alex Damisch ",a(href="mailto:damischa@lawrence.edu","(damischa@lawrence.edu)"),
             "and Adam Loy ",a(href="mailto:loya@lawrence.edu","(loya@lawrence.edu)."))
  ),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "$('li.active a').first().html()==='Input'",
        radioButtons("chooseData", label=h4("Choose data set"),
                     c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                     selected = "uploadNo"),
        conditionalPanel(
          condition= "input.chooseData=='uploadYes'",
          tags$div(id="dataOptions",
                   h4("Data Set Options"),
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
                   p("Note: The file size limit is 5MB. Larger files will take longer to upload and bootstrap.
                  You can upload text, .csv, or .tsv files."),
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
        selectInput('group', label=h4('Grouping variable'),'group'),
        selectInput('response', label=h4('Response variable'), 'response')
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Summaries'",
        radioButtons("plot", label=h4("Plotting"),
                     c("Histogram" = "his", "Kernel Density" = "den", "Histogram and Kernel Density" = "hisDen",
                       "Q-Q Plot" = "qq"), selected="his"),
          sliderInput("w", 
                       label = h4("Histogram Bin Width"), 
                      min = 0.1, max=3, value = 1.2, step=0.1)
        ),
      conditionalPanel(
        "$('li.active a').first().html()==='Bootstrap'",
        radioButtons("stat", label = h4("Statistic"),
                     c("Difference of Means" = "bootMean", "Difference of Medians" = "bootMedian", 
                       "Ratio of Means" = "bootMeanRatio", "Ratio of Medians" = "bootMedRatio",
                       "Ratio of Standard Deviations" = "bootSdRatio"), selected = "bootMean"),
        numericInput("num", 
                     label = h4("Number of Bootstraps"), 
                     value = 1000, min = 1, max = 100000),
        actionButton("goButton", "Bootstrap!"),
#         actionButton("reset", "Reset"),
        radioButtons("plot2", label=h4("Plotting"),
                     c("Histogram" = "his2", "Kernel Density" = "den2", "Histogram and Kernel Density" = "hisDen2",
                       "Q-Q Plot" = "qq2"), selected="his2"),
        h4("Histogram Bin Width"),
        conditionalPanel(
          condition="input.plot2=='hisDen2'",
          sliderInput("w2", 
                      label = "",
                      value = 0.2, step=0.005, min = 0.005, max=0.3)
        ),
        conditionalPanel(
          condition="input.plot2 != 'hisDen2'",
          uiOutput("bootHist_ui")
        )
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Confidence Intervals'",
        radioButtons("ci", label = h4("Confidence Interval"),
                     c("Percentile" = "perc", "Normal-Based" = "norm"), selected = "perc"),
        sliderInput("level", 
                     label = h4("Confidence Level"), 
                     value = 0.95, min = 0.01, max = 0.99, step=0.01)
      )
    ),
    mainPanel(title="Two-Sample Bootstrap",
              tabsetPanel(type="tabs",
                          tabPanel("Input",
                                   dataTableOutput("contents")
                                   ),
                          tabPanel("Summaries",
                                  plotOutput("origHist"),
#                                     ggvisOutput("group1Hist"),
                                  h5("Original Summary Statistics"),
                                  tableOutput("basicSummary")
                    ), #tabPanel
                    tabPanel("Bootstrap",
                       conditionalPanel(
                         condition="input.plot2=='hisDen2'",
                         plotOutput("hisDenPlot2")
                       ),
                       conditionalPanel(
                         condition="input.plot2 != 'hisDen2'",
                         ggvisOutput("bootHist")
                       ),
                       h6("Mean"),
                       verbatimTextOutput("bootSummary"),
                       h6("Estimate Bias"),
                       verbatimTextOutput("bootBias"),
                       h6("Estimate Standard Deviation"),
                       verbatimTextOutput("bootSd"),
                       actionButton("hideData", "Show/hide data set"),
                       hidden(
                       dataTableOutput("trials")
                       )
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