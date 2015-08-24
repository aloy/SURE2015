library(shiny)
library(shinyjs)
library(ggvis)

shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("Binomial Proportions"),
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
                   p("Note: The file size limit is 5MB. Larger files will take longer to upload and manipulate
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
                                '"'),
                   radioButtons("factor", label="Numeric/Factor", c("Column of 0/1s" = "col",
                                "Factor with Two Levels" = "fact"))
          ), #tags
          actionButton("hideDataOptions", "Show/hide data set options")
        ), #conditionalPanel
        uiOutput("selectVar"),
        conditionalPanel(
          condition='input.factor=="fact"',
          uiOutput("factSuccess")
        )
        ), #conditionalPanel (input tab)
      conditionalPanel(
        "$('li.active a').first().html()==='Tests'",
        radioButtons("test", label=h4("Permutation Test"), c("Two-Tailed" = "tt", "Lower Tail" = "lt", "Upper Tail" = "ut"), 
                     selected="tt"),
        h4(HTML(paste("p", tags$sub(0), sep = ""))),
        sliderInput("p0", label="", min=0.01, max=0.99, value=0.5, step=0.01),
        numericInput("num", label=h4("Number of Resamples"), value=1000, min=1, max=100000, step=1),
        actionButton("goButton", "Permute!"),
        sliderInput("w", label=h4("Bootstrap Bin Width"),value=0.05, min=0.001, max=0.1, step=0.001)
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Confidence Intervals'",
        sliderInput("ci", label=h4("Confidence Levels"), value=0.95, min=0.01, max=0.99, step=0.01)
      )
    ), 
  mainPanel(
    tabsetPanel(type="tabs",
      tabPanel("Input",
               dataTableOutput("contents")
        ), #tabPanel
      tabPanel("Tests",
              ggvisOutput("permHist"),
              h5("Sample Mean"),
              verbatimTextOutput("mean"),
              h5("Permutation Mean"),
              verbatimTextOutput("permMean"),
              h5("P-Value"),
              verbatimTextOutput("pval")
               ),
      tabPanel("Confidence Intervals",
               verbatimTextOutput("ci")
      )
      ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
))
  