library(shiny)

shinyUI(bootstrapPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput("lm", "Add linear model"),
      conditionalPanel(
        condition="input.lm == true",
        checkboxInput("resid", "Show residual plot")
        )
    ),
  mainPanel(
           # In a plotOutput, passing values for click, dblclick, hover, or brush
           # will enable those interactions.
    fluidRow(
      column(width=6,
           plotOutput("plot1",
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
                      )
           )
      ),
      column(width=6,
             conditionalPanel(
               condition="input.resid == true",
               plotOutput("plot2",
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
                          )
               )
             )
      )
    ),
  fluidRow(
    column(width = 3,
           verbatimTextOutput("click_info")
    ),
    column(width = 3,
           verbatimTextOutput("dblclick_info")
    ),
    column(width = 3,
           verbatimTextOutput("hover_info")
    ),
    column(width = 3,
           verbatimTextOutput("brush_info")
    )
  ) #fluidRow
  ) #mainPanel
  ) #sidebarLayout
  ))