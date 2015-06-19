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
                 c("Histogram" = "his", "Kernel Density" = "den"), selected="his"),
               numericInput("w", 
                 label = h5("Bootstrap Bin Width"), 
                 value = 0.025, step=0.005, min = 0.005),
              h4("Resampling"),
                numericInput("num", 
                  label = h5("Number of Bootstraps"), 
                  value = 1000, min = 1, max = 100000),
                radioButtons("stat", label = h5("Statistic"),
                  c("Mean" = "bootMean", "Median" = "bootMedian", 
                    "Standard Deviation" = "bootStdDev"), selected = "bootMean"),
                radioButtons("ci", label = h5("Confidence Interval"),
                  c("Percentile" = "perc", "Normal-Based" = "norm"), selected = "perc"),
                numericInput("level", 
                  label = h5("Confidence Level"), 
                  value = 0.95, min = 0.01, max = 0.99, step=0.01),
              p("This might be a good place to put description, instead of having another column.")
             ), #conditionalPanel
             conditionalPanel(
               "$('li.active a').first().html()==='Two-Sample Bootstrap'",
               h3("Bootstrap Control Panel"),
               radioButtons("plot2", label=h4("Plotting"),
                            c("Histogram" = "his2", "Kernel Density" = "den2"), selected="his2"),
               numericInput("w2", 
                            label = h5("Bootstrap Bin Width"), 
                            value = 0.2, step=0.05, min = 0.05),
               h4("Resampling"),
               numericInput("num2", 
                            label = h5("Number of Bootstraps"), 
                            value = 1000, min = 1, max = 100000),
               radioButtons("stat2", label = h5("Statistic"),
                    c("Means" = "bootMean2", "Medians" = "bootMedian2", 
                     "Standard Deviations" = "bootSd2", "Difference in Ratios" = "ratioDiff",
                     "Ratio of Standard Deviations" = "sdRatio"), selected = "bootMean2"),
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
              h6("Mean"),
                verbatimTextOutput("mean"),
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
            condition = "input.stat2 == 'bootSd2'",
            plotOutput("bootSdHist2"),
            h6("Estimate Five-Number Summary"),
            verbatimTextOutput("bootSdSummary2"),
            h6("Estimate Bias"),
            verbatimTextOutput("bootSdBias2"),
            h6("Estimate Standard Deviation"),
            verbatimTextOutput("bootSdSd2")
          ), #conditionalPanel
          conditionalPanel(
            condition = "input.stat2 == 'ratioDiff'",
            plotOutput("bootRatioHist2"),
            h6("Estimate Five-Number Summary"),
            verbatimTextOutput("bootRatioSummary2"),
            h6("Estimate Bias"),
            verbatimTextOutput("bootRatioBias2"),
            h6("Estimate Standard Deviation"),
            verbatimTextOutput("bootRatioSd2")
          ), #conditionalPanel
          conditionalPanel(
            condition = "input.stat2 == 'sdRatio'",
            plotOutput("bootSdRatioHist2"),
            h6("Estimate Five-Number Summary"),
            verbatimTextOutput("bootSdRatioSummary2"),
            h6("Estimate Bias"),
            verbatimTextOutput("bootSdRatioBias2"),
            h6("Estimate Standard Deviation"),
            verbatimTextOutput("bootSdRatioSd2")
          )
      ) #tabPanel
    ) #tabsetPanel
    ) # mainPanel
  ) #sidebarLayout
 ) #fluidPage
) #shinyUI