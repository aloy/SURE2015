library(shiny)
library(shinyjs)
library(ggvis)
shinyUI(fluidPage(
  useShinyjs(),
  titlePanel("One-Sample Bootstrap"),
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
        ),
        actionButton("hideDataOptions", "Show/hide data set options")
      ),
      selectInput('response', label=h4('Response variable'), 'response')
      ),
        conditionalPanel(
          "$('li.active a').first().html()==='Summaries'",
        radioButtons("plot", label=h4("Plotting"),
                     c("Histogram" = "his", "Kernel Density" = "den", "Histogram and Kernel Density" = "hisDen",
                       "Q-Q Plot" = "qq"), 
                     selected="his"),
        h4("Histogram Bin Width"),
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
        radioButtons("stat", label = h4("Bootstrap Statistic"),
                     c("Mean" = "bootMean", "Median" = "bootMedian", 
                       "Standard Deviation" = "bootSd"), selected = "bootMean"),
        numericInput("num", 
                     label = h4("Number of Bootstraps"), 
                     value = 10000, min = 1, max = 100000),
        actionButton("goButton", "Bootstrap!"), 
#         actionButton("reset", "Reset"),
        radioButtons("plot2", label=h4("Plotting"),
                     c("Histogram" = "his2", "Kernel Density" = "den2", "Histogram and Kernel Density" = "hisDen2",
                       "Q-Q Plot" = "qq2"), 
                     selected="his2"),
        h4("Histogram Bin Width"),
        conditionalPanel(
          condition="input.plot2=='hisDen2'",
          sliderInput("w2", 
                      label = "",
                      value = 0.02, step=0.005, min = 0.005, max=0.1)
        ),
        conditionalPanel(
          condition="input.plot2 != 'hisDen2'",
          uiOutput("bootHist_ui")
        )
        ),
        conditionalPanel(
          "$('li.active a').first().html()==='Confidence Intervals'",
        radioButtons("ci", label = h4("Confidence Interval"),
                     c("Percentile" = "perc", "t Interval with Bootstrap
Standard Error" = "t"), selected = "perc"),
        sliderInput("level", 
                     label = h4("Confidence Level"), 
                     value = 0.95, min = 0.01, max = 0.99, step=0.01)
        )
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(type="tabs",  	     
                  tabPanel("Input",
                           dataTableOutput("contents")
                  ),
                      tabPanel("Summaries",
                                  
                                            conditionalPanel(
                                              condition="input.plot=='hisDen'",
                                              plotOutput("hisDenPlot")
                                              ),
                                            conditionalPanel(
                                              condition="input.plot != 'hisDen'",
                                            ggvisOutput("origHist")
                                              ),
                                            h5("Original Summary Statistics"),
                                            tableOutput("summary")
                                            
                           ),
                  tabPanel("Bootstrap",
                                            conditionalPanel(
                                              condition="input.plot2=='hisDen2'",
                                              plotOutput("hisDenPlot2")
                                            ),
                                            conditionalPanel(
                                              condition="input.plot2 != 'hisDen2'",
                                              ggvisOutput("bootHist")
                                            ),
                                            h5("Bootstrap Summary Statistics"),
                           h6("Original Statistic"),
                           verbatimTextOutput("origStat"),
                                            h6("Mean"),
                                            verbatimTextOutput("bootMean"),
                                            h6("Bias"),
                                            verbatimTextOutput("bootBias"),
                                            h6("Standard Error"),
                                            verbatimTextOutput("bootSd"),
                                            hidden(
                                            dataTableOutput("trials")
                                            ),
                                            actionButton("hideData", "Show/hide data")
                                 
                  ),
                  tabPanel("Confidence Intervals",
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
                             condition = "input.ci == 't'",
                             h6("Two-Tailed Confidence Interval (t-based)"),
                             verbatimTextOutput("tPrint"),
                             h6("Lower Bound"),
                             verbatimTextOutput("tLower"),
                             h6("Upper Bound"),
                             verbatimTextOutput("tUpper")
                           )
                  )
      ) #tabset
    ) # mainPanel
  ) #sidebarLayout
) #fluidPage
) #shinyUI