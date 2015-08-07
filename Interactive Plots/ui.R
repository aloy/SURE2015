library(shiny)

shinyUI(bootstrapPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "$('li.active a').first().html()==='Linear Model'",
      checkboxInput("lm", "Add linear model"),
      conditionalPanel(
        condition="input.lm == true",
        radioButtons("plot", label="Plots", c("Residual plot"="resid","Q-Q Plot"="qq"), selected="resid")
        ),
      actionButton("reset", "Reset"),
      p("Clicking on a point will automatically exclude it. Click the 'Reset' button to include all points.")
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Diagnostics'",
      radioButtons("diag", label="Diagnostic Plots", c("Leverage"="lev", "Cook's Distance"="cooks"), selected="lev")
      )
      ),
  mainPanel(
   tabsetPanel(type="tabs",
    tabPanel("Linear Model",          
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
               condition="input.lm == true",
               plotOutput("plot2",
                          # Equivalent to: click = clickOpts(id = "plot_click")
                          click = "plot2_click",
                          dblclick = dblclickOpts(
                            id = "plot2_dblclick"
                          ),
                          hover = hoverOpts(
                            id = "plot2_hover"
                          ),
                          brush = brushOpts(
                            id = "plot2_brush"
                          )
               )
             )
      )
    ),
  fluidRow(
    column(width = 3,
           verbatimTextOutput("click_info"),
           conditionalPanel(
             condition="input.lm == true",
           tableOutput("lm")
           )
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
    ),#tabPanel
  tabPanel("Diagnostics",
           plotOutput("diagPlot")
    )#tabPanel
  ) #tabsetPanel
  ) #mainPanel
  ) #sidebarLayout
  ))