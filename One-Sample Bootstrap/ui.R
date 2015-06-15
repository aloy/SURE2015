shinyUI(fluidPage(
  titlePanel("One-Sample Bootstrapping"),
  fluidRow(
    column(2,
          wellPanel("Sidebar, presumably with some description about the app.")
    ),
    column(3,
           wellPanel(h3("Control Panel"),
           radioButtons("plot", label=h4("Plotting"),
            choices=list("Histogram" = 1, "Density" = 2), selected=1),
           h4("Resampling"),
           numericInput("num", 
                        label = h5("Number of Bootstraps"), 
                        value = 1000),
           radioButtons("stat", label = h5("Statistic"),
            choices = list("Mean" = 3, "Median" = 4, "Standard Deviation" = 5), selected = 1),
           radioButtons("ci", label = h5("Confidence Interval"),
                        choices = list("Percentile" = 6, "Normal-Based" = 7), selected = 1),
           numericInput("level", 
                        label = h5("Confidence Level"), 
                        value = 0.95)
           )
    ),
    column(7,
      mainPanel(h3("Output Panel"),
      plotOutput("origHist"),
      h5("Summary Statistics"),
      h6("Five-Number Summary"),
      verbatimTextOutput("summary"),
      h6("Mean"),
      verbatimTextOutput("mean"),
      h6("Standard Deviation"),
      verbatimTextOutput("sd"))
  )
)
))