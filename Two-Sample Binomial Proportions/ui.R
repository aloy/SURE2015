library(shiny)
library(shinyjs)
library(ggvis)

shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("Two-Sample Binomial Proportions"),
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
                                '"'),
                   radioButtons("factor", label="Numeric/Factor", c("Column of 0/1s" = "col",
                                                                    "Factor with Levels" = "fact"))
          ), #tags
          actionButton("hideDataOptions", "Show/hide data set options")
        ), #conditionalPanel
        uiOutput("selectPop"),
        uiOutput("selectDiff"),
        uiOutput("factSuccess"),
        hidden(
          shiny::p(id = "suggest", "Suggested: Use 'Home' for Populations and 'WinLoss' for Proportions.")
        )
      ), #conditionalPanel (input tab)
      conditionalPanel(
        "$('li.active a').first().html()==='Tests'",
        radioButtons("test", label=h4("Permutation Test"), c("Two-Tailed" = "tt", "Lower Tail" = "lt", "Upper Tail" = "ut"), 
                     selected="tt"),
        numericInput("num", label=h4("Number of Resamples"), value=1000, min=1, max=100000, step=1),
        actionButton("goButton", "Permute!"),
        sliderInput("w", label=h4("Histogram Bin Width"),value=0.1, min=0.01, max=0.5, step=0.01)
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Confidence Interval'",
        numericInput("num2", label=h4("Number of Bootstraps"), value=1000, min=1, max=100000),
        actionButton("goButton2", "Bootstrap!"),
        sliderInput("ci", label=h4("Confidence Level"), value=0.95, min=0.01, max=0.99, step=0.01),
        hidden(
          shiny::p(id = "warning", strong("You must permute on the previous tab before bootstrapping for 
                   the confidence interval."), style = "color:red")
      )
      )
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Input",
                      dataTableOutput("contents")
                  ), #tabPanel
                  tabPanel("Tests",
                           ggvisOutput("permsHist"),
                           h5("Original Contingency Table"),
                      tableOutput("orig"),
                      h5("Observed Difference In Proportions"),
                      verbatimTextOutput("propDiff"),
                      h5("Permutation Difference In Proportions"),
                      verbatimTextOutput("permDiff"),
                      h5("P-Value"),
                      verbatimTextOutput("pval")
                           ),
                  tabPanel("Confidence Interval",
                           verbatimTextOutput("confInt")
                           )
      ) #tabsetPanel
    )#mainPanel
  )#sidebarLayout
))