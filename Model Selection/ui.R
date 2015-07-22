library(shiny)
library(shinyjs)
library(ggvis)
shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("Model Selection for MLR"),
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
        ) #conditionalPanel
      ),#conditionalPanel
      conditionalPanel(
        "$('li.active a').first().html()==='Model Selection'",
        selectInput('responseVar', label=h4('Response variable'), 'responseVar'),
        radioButtons("mod", label=h4("Model"), c("Full Model" = "full", "Selected Model" = "other",
        "Backwards Selection from Full" = "back", "Forwards Selection from Null"="forwards"), selected="full"),
        tags$div(id="selectOptions",
           uiOutput("factorSelect"),
          conditionalPanel(
          condition='input.mod=="other"',
          uiOutput("select"),
        uiOutput("yint")
          )
        ),
        actionButton("hideSelectOptions", "Show/hide selection options")
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Model Checking'",
        radioButtons("check", label=h4("Diagnostics"), c("Multicollinearity" = "mult", "Non-Linearity" = "line", 
                "Influence" = "infl", "Non-Constant Variance" = "var", "Non-Normal Errors" = "norm"), selected="mult"),
        uiOutput("warn")
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Inference'",
        radioButtons("inf", label=h4("Inference"), c("All Coefficients" = "all", "Single Coefficient" = "single")),
        conditionalPanel(
          condition='input.inf=="single"',
          sliderInput("level", label=h4("Confidence Level"), min=0.01, max=0.99, value=0.95, step=0.01)
          )
      )
      ), #sidebar Panel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Input",
                           dataTableOutput("contents")               
                  ), #tabPanel
                  tabPanel("Model Selection",
                           verbatimTextOutput("summary")
                  ),
                  tabPanel("Model Checking",
                           conditionalPanel(
                             condition='input.check=="mult"',
                             h4("Plotting Matrix"),
                             plotOutput("corrPlot"),
                             h4("Correlations Table"),
                             tableOutput("corrTable"),
                             h4("Variance Inflation Factor"),
                             verbatimTextOutput("vif")
                           ),
                           conditionalPanel(
                             condition='input.check=="line"',
                             h4("Added-Variable Plots"),
                             plotOutput("av")
                           ),
                           conditionalPanel(
                             condition='input.check=="infl"',
                             h4("Hat Values"),
                             tableOutput("hat"),
                             h4("Cook's Distance"),
                             tableOutput("cooks"),
                             h4("DFFITS"),
                             tableOutput("dffits"),
                             h4("DFBETAS"),
                             tableOutput("dfbetas")
                           ),
                           conditionalPanel(
                             condition='input.check=="var"',
                             h4("Breusch-Pagan Test"),
                             verbatimTextOutput("bp")
                           ),
                           conditionalPanel(
                             condition='input.check=="norm"',
                             h4("Normal Q-Q Plot"),
                             plotOutput("qq")
                           )
                  ),
                  tabPanel("Inference",
                           conditionalPanel(
                             condition='input.inf=="all"',
                             tableOutput("anovaNull")
                           ),
                           conditionalPanel(
                             condition='input.inf=="single"',
                             tableOutput("coefCI")
                             )
                  )
      ) #tabsetPanel
    ) #mainPanel
  )#sidebarLayout
))