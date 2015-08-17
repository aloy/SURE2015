library(shiny)

shinyUI(bootstrapPage(
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "$('li.active a').first().html()==='Input'",
        radioButtons("chooseData", label=h4("Choose data set"),
                     c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                     selected = "uploadNo"),
        conditionalPanel(
          condition= "input.chooseData=='uploadYes'",
          tags$div(id="dataOptions",
                   h4("Data Set Options"),
                   fileInput('file1', 'Choose a file to upload.',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )
                   ),
                   p("Note: The file size limit is 5MB. Larger files will take longer to upload and bootstrap.
                  You can upload text, .csv, or .tsv files."),
                   checkboxInput('header', 'Header', TRUE),
                   radioButtons('sep', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t',
                                  Space = " "),
                                ','),
                   radioButtons('quote', 'Quote',
                                c(None='',
                                  'Double Quote'='"',
                                  'Single Quote'="'"),
                                '"')
          ),
          actionButton("hideDataOptions", "Show/hide data set options")
        ) #conditionalPanel
      ),#conditionalPanel
      conditionalPanel(
        "$('li.active a').first().html()!='Input'&&$('li.active a').first().html()!='Diagnostics'&&
          $('li.active a').first().html()!='Multiple Regression'",
        uiOutput("scatterx")
      ),
      conditionalPanel(
        "$('li.active a').first().html()!='Input'&&$('li.active a').first().html()!='Diagnostics'",
        uiOutput("scattery")
      ),
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
      radioButtons("diag", label="Diagnostic Plots", c("Leverage"="lev", "Cook's Distance"="cooks", 
                                                       "DFFITS" = "dffits"), selected="lev")
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Multiple Regression'",
uiOutput("residChoices")
)
      ),
  mainPanel(
   tabsetPanel(type="tabs",
               tabPanel("Input",
                        dataTableOutput("contents")               
               ), #tabPanel
       tabPanel("Linked Brushing",
                ggvisOutput("linked1"),
                ggvisOutput("linked2"),
                tableOutput("filtered")
                        ),
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
           verbatimTextOutput("click_info")
    ),
    column(width = 3,
           verbatimTextOutput("hover_info")
    ),
    column(width = 3,
           verbatimTextOutput("brush_info")
    )
  ),#fluidRow
  conditionalPanel(
    condition="input.lm == true",
  verbatimTextOutput("lm") 
  )
    ),#tabPanel
  tabPanel("Diagnostics",
           plotOutput("diagPlot")
    ),#tabPanel
  tabPanel("Multiple Regression",
           h4("Full Model Residual Plot"),
           plotOutput("resid"),
           plotOutput("residPlot")
           )
  ) #tabsetPanel
  ) #mainPanel
  ) #sidebarLayout
  ))