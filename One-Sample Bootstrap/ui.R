library(markdown)
shinyUI(fluidPage(
    sidebarLayout(
           sidebarPanel(h3("Bootstrap Control Panel"),
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
                  c("Mean" = "bootMean", "Median (One-Sample)" = "bootMedian", 
                    "Standard Deviation (One-Sample)" = "bootStdDev"), selected = "bootMean"),
                radioButtons("ci", label = h5("Confidence Interval (One-Sample)"),
                  c("Percentile" = "perc", "Normal-Based" = "norm"), selected = "perc"),
                numericInput("level", 
                  label = h5("Confidence Level (One-Sample)"), 
                  value = 0.95, min = 0.01, max = 0.99, step=0.01),
              p("This might be a good place to put description, instead of having another column.")
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
             plotOutput("bootHist2")
      ) #tabPanel
    ) #tabsetPanel
    ) # mainPanel
  ) #sidebarLayout
 ) #fluidPage
) #shinyUI