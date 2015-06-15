shinyUI(fluidPage(
  titlePanel("One-Sample Bootstrapping"),
  fluidRow(
    column(2,
          wellPanel("Sidebar, presumably with some description about the app.")
    ),
    column(3,
           wellPanel(h3("Bootstrap Control Panel"),
           radioButtons("plot", label=h4("Plotting"),
            c("Histogram" = 1, "Density" = 2), selected=1),
           numericInput("w", 
                        label = h5("Bin Width"), 
                        value = 0.025),
           h4("Resampling"),
           numericInput("num", 
                        label = h5("Number of Bootstraps"), 
                        value = 1000, min = 1, max = 100000),
           radioButtons("stat", label = h5("Statistic"),
                        c("Mean" = "bootMean", "Median" = "med", "Standard Deviation" = "sdev"), 
                        selected = "bootMean"),
           radioButtons("ci", label = h5("Confidence Interval"),
                        c("Percentile" = 6, "Normal-Based" = 7), selected = 6),
           numericInput("level", 
                        label = h5("Confidence Level"), 
                        value = 0.95, min = 0.01, max = 0.99))
    ),
    column(3,
      wellPanel(h3("Original Sample"),
      plotOutput("origHist"),
      h5("Summary Statistics"),
        h6("Five-Number Summary"),
      verbatimTextOutput("summary"),
      h6("Mean"),
        verbatimTextOutput("mean"),
      h6("Standard Deviation"),
       verbatimTextOutput("sd"))
    ),
  column(4,
         wellPanel(h3("Bootstrap Distribution"),
         plotOutput("bootHist"),
         h5("Summary Statistics")
         )
    )
)
))