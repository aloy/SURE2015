library(shiny)
library(shinyBS)

fluidPage(
    mainPanel(
      h4("ggvis tooltip"),
      ggvisOutput("visplot"),
      h4("ggvis linked brushing"),
      uiOutput("scatterx"), uiOutput("scattery"),
      ggvisOutput("linked1"),
      ggvisOutput("linked2"),
#       actionButton("exclude", "Exclude brushed"),
#       actionButton("reset", "reset"),
      verbatimTextOutput("lbtest"),
      h4("Tweaked ggplot2 excluding"),
      plotOutput("ggplot",
                 # Equivalent to: click = clickOpts(id = "plot_click")
                 click = "plot_click",
                 dblclick = dblclickOpts(
                   id = "plot_dblclick"
                 ),
                 hover = hoverOpts(
                   id = "plot_hover"
                 ),
                 brush = brushOpts(
                   id = "plot_brush"
                 )),
      actionButton("toggle", label="Toggle Point"),
      actionButton("reset", label="Reset Point"),
      verbatimTextOutput("info")
    )
  )