library(shiny)
library(shinyjs)

shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("Visual Inference"),
  tags$div(class = "header", 
           p("This app was created by Alex Damisch ",a(href="mailto:damischa@lawrence.edu","(damischa@lawrence.edu)"),
             "and Adam Loy ",a(href="mailto:loya@lawrence.edu","(loya@lawrence.edu)."))
  ),
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
                                      Tab='\t'),
                                    ','),
                       radioButtons('quote', 'Quote',
                                    c(None='',
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"')      
              ),
              actionButton("hideDataOptions", "Show/hide data set options")
            )#conditionalPanel
          ), #conditionalPanel
          conditionalPanel(
            "$('li.active a').first().html()==='Plots'",
            h4("Variables"),
            conditionalPanel(
              condition='input.plot!="box"&&input.plot!="den"',
            selectInput("x", label="X Variable","x")
            ),
            conditionalPanel(
            condition='input.plot=="box"||input.plot=="den"',
            selectInput("x2", label="Grouping Variable","x2"),
            selectInput("y2", label="Response Variable", "y2")
            ),
            conditionalPanel(
              condition='input.plot!="qq"&&input.plot!="box"&&input.plot!="den"',
            selectInput("y", label="Y Variable","y")
            ),
            hidden(
              shiny::p(id = "warning", strong("Make sure you select different variables!"),  style = "color:red")
            ),
            hidden(
              shiny::p(id = "warning2", strong("Make sure you select different variables! (2)"),  style = "color:red")
            ),
            radioButtons("plot", label=h4("Plotting"), c("Scatterplot" = "scatter", 
               "Scatterplot with Smoother" = "scatterSmooth", "Boxplot" = "box", 
               "Layered Densities" ="den", "Q-Q Plot"="qq", "Residual Plot" = "resid"), selected="scatter"),
            numericInput("num", label=h4("Number of Plots"), value=9, min=2, max=24, step=1)
          ) #conditionalPanel
        ), #sidebarPanel
        mainPanel(
          tabsetPanel(type="tabs",
             tabPanel("Input",
                 dataTableOutput("contents")
          ), #tabPanel
            tabPanel("Plots",
                    plotOutput("lineup", width="100%"),
                    actionButton("trueData", "Show/hide true data"),
                    hidden(
                      verbatimTextOutput("plotPos")
                    )
                     )
        ) #tabsetPanel
      ) #mainPanel
    ) #sidebarLayout
  ))