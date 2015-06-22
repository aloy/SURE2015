library(shiny)
library(markdown)
library(datasets)
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "$('li.active a').first().html()==='One-Sample Bootstrap'",
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
        radioButtons("stat", label = h5("Statistic"),
                     c("Mean" = "bootMean", "Median" = "bootMedian", 
                       "Standard Deviation" = "bootSd"), selected = "bootMean"),
        radioButtons("ci", label = h5("Confidence Interval"),
                     c("Percentile" = "perc", "Normal-Based" = "norm"), selected = "perc"),
        numericInput("level", 
                     label = h5("Confidence Level"), 
                     value = 0.95, min = 0.01, max = 0.99, step=0.01),
        p("This might be a good place to put description, instead of having another column.")
      ), #conditionalPanel
      conditionalPanel(
        "$('li.active a').first().html()==='Two-Sample Bootstrap'",
        checkboxInput("one", "Show One-Variable Statistics"),
        h3("Bootstrap Control Panel"),
        radioButtons("plot2", label=h4("Plotting"),
                     c("Histogram" = "his2", "Kernel Density" = "den2", "Histogram and Kernel Density" = "hisDen2",
                       "Q-Q Plot" = "qq2"), selected="his2"),
        conditionalPanel(
          condition = "input.one == true",
          numericInput("w3", 
                       label = h5("Original Bin Width"), 
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
      )
      #conditionalPanel
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("One-Sample Bootstrap",
                           column(5,
                                  wellPanel(h3("Original Sample"),
                                            plotOutput("origHist"),
                                            h5("Original Summary Statistics"),
                                            h6("Five-Number Summary"),
                                            verbatimTextOutput("summary"),
                                            h6("Standard Deviation"),
                                            verbatimTextOutput("sd"))
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
                           )      #column
                  ), #tabPanel
                  tabPanel("Two-Sample Bootstrap",
                           conditionalPanel(
                          condition = "input.one == true",
                           h3("One-Variable Statistics"),
                           fluidRow(
                          column(6,
                           h4("Basic Cable"),
                           plotOutput("basicHist")
                          ), #column
                          column(6,
                           h5("Original Summary Statistics"),
                           h6("Five-Number Summary"),
                           verbatimTextOutput("basicSummary"),
                           h6("Standard Deviation"),
                           verbatimTextOutput("basicSd")
                          ) #column
                            ), #fluidRow
                          fluidRow(
                            column(6,
                                   h4("Extended Cable"),
                                   plotOutput("extendedHist")
                            ), #column
                            column(6,
                                   h5("Original Summary Statistics"),
                                   h6("Five-Number Summary"),
                                   verbatimTextOutput("extendedSummary"),
                                   h6("Standard Deviation"),
                                   verbatimTextOutput("extendedSd")
                            ) #column
                          ) #fluidRow
                           ), #conditionalPanel
                          h3("Bootstrap Samples"),
                           conditionalPanel(
                             condition = "input.stat2 == 'bootMean2'",
                             plotOutput("bootMeanHist2"),
                             h6("Estimate Five-Number Summary"),
                             verbatimTextOutput("bootMeanSummary2"),
                             h6("Estimate Bias"),
                             verbatimTextOutput("bootMeanBias2"),
                             h6("Estimate Standard Deviation"),
                             verbatimTextOutput("bootMeanSd2")
                           ), #conditionalPanel
                           conditionalPanel(
                             condition = "input.stat2 == 'bootMedian2'",
                             plotOutput("bootMedianHist2"),
                             h6("Estimate Five-Number Summary"),
                             verbatimTextOutput("bootMedianSummary2"),
                             h6("Estimate Bias"),
                             verbatimTextOutput("bootMedianBias2"),
                             h6("Estimate Standard Deviation"),
                             verbatimTextOutput("bootMedianSd2")
                           ), #conditionalPanel
                           conditionalPanel(
                             condition = "input.stat2 == 'bootMeanRatio'",
                             plotOutput("bootMeanRatioHist"),
                             h6("Estimate Five-Number Summary"),
                             verbatimTextOutput("bootMeanRatioSummary"),
                             h6("Estimate Bias"),
                             verbatimTextOutput("bootMeanRatioBias"),
                             h6("Estimate Standard Deviation"),
                             verbatimTextOutput("bootMeanRatioSd")
                           ), #conditionalPanel
                           conditionalPanel(
                             condition = "input.stat2 == 'bootMedRatio'",
                             plotOutput("bootMedRatioHist"),
                             h6("Estimate Five-Number Summary"),
                             verbatimTextOutput("bootMedRatioSummary"),
                             h6("Estimate Bias"),
                             verbatimTextOutput("bootMedRatioBias"),
                             h6("Estimate Standard Deviation"),
                             verbatimTextOutput("bootMedRatioSd")
                           ), #conditionalPanel
                           conditionalPanel(
                             condition = "input.stat2 == 'bootSdRatio'",
                             plotOutput("bootSdRatioHist"),
                             h6("Estimate Five-Number Summary"),
                             verbatimTextOutput("bootSdRatioSummary"),
                             h6("Estimate Bias"),
                             verbatimTextOutput("bootSdRatioBias"),
                             h6("Estimate Standard Deviation"),
                             verbatimTextOutput("bootSdRatioSd")
                           ),
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
                           ) 
                  ) #tabPanel
      ) #tabsetPanel
    ) # mainPanel
  ) #sidebarLayout
) #fluidPage
) #shinyUI