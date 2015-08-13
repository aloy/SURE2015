library(shiny)
library(shinyBS)

fluidPage(
    mainPanel(
      ggvisOutput("visplot"),
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