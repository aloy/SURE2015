library(shiny)
library(shinyjs)

shinyUI(bootstrapPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        "$('li.active a').first().html()==='Input'",
      radioButtons("method", label=h4("Select method of entry"), choices=c("Numeric input" ="num",
                                                                       "Data set" = "data")),
      conditionalPanel(
        condition='input.method=="data"',
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
          ), #tags
          actionButton("hideDataOptions", "Show/hide data set options")
        ), #conditionalPanel
        uiOutput("selectVar")
      ) #conditionalPanel (fileInput)
      ), #conditionalPanel (input tab)
      conditionalPanel(
        "$('li.active a').first().html()==='Tests'",
        radioButtons("test", label=h4("Binomial Test"), 
          c("Two-Tailed" = "tt", "Lower Tail" = "lt", "Upper Tail" = "ut"))
      )
    ), 
  mainPanel(
    tabsetPanel(type="tabs",
      tabPanel("Input",
               conditionalPanel(
                 condition='input.method=="data"',
               dataTableOutput("contents")  
               ),
               conditionalPanel(
                 condition='input.method=="num"',
                 numericInput("success", "Number of Successes", value=0),
                 numericInput("trials", "Number of Trials", value=0),
                 hidden(
                   shiny::p(id = "warning", strong("Number of successes must be 
                  less than or equal to number of trials."),  style = "color:red")
                 )
               )
        ), #tabPanel
      tabPanel("Tests",
               h5(HTML(paste("p", tags$sub(0), sep = ""))),
          verbatimTextOutput("p0")
               )
      ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
))
  