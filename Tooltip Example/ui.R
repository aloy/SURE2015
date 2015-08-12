library(shiny)
library(shinyBS)
fluidPage(
    mainPanel(
      ggvisOutput("distPlot")
    )
  )