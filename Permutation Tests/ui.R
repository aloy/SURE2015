library(shiny)
library(shinyjs)
library(ggvis)
library(dplyr)
shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("Permutation Tests"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("chooseData", label=h5("Choose data set"),
                   c("Use built-in data set" = "uploadNo", "Upload my own data set" = "uploadYes"),
                   selected = "uploadNo"),
      conditionalPanel(
        condition= "input.chooseData=='uploadYes'",
        tags$div(id="dataOptions",
                 fileInput('file1', 'Choose a file to upload. The data set will appear below the main panel.',
                           accept = c(
                             'text/csv',
                             'text/comma-separated-values',
                             'text/tab-separated-values',
                             'text/plain',
                             '.csv',
                             '.tsv'
                           )
                 ),
                 h5("Data Set Options"),
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
                 uiOutput("varChoose"),
                 uiOutput("varChoose2")
        ),
        actionButton("hideDataOptions", "Show/hide data set options")
      ), #conditionalPanel
      actionButton("hideData", "Show/hide data set"),
      h4("Resampling"),
      radioButtons("type", label=h5("Plot Type"),
                   c("Histogram" = "his", "Kernel Density" = "den",
                     "Q-Q Plot" = "qq"), selected="his"),
      numericInput("num", 
                   label = h5("Number of Permutation Resamples"), 
                   value = 1000, min = 1, max = 100000),
      h5("Permutation Histogram Bin Width"),
      uiOutput("trialsHist2_ui"),
      radioButtons("test", label=h5("Permutation Test"), c("Two-Tailed" = "tt", "Lower Tail" = "lt", "Upper Tail" = "ut"), 
                   selected="tt"),
      p("Permutation tests compare whether or not there is a significant difference between two experimental groups performing 
    the same task. We resample without replacement from the pooled data set of both groups, assigning the first half of resamples 
    to the first group and the second half to the second group, and then calculate the statistic we're interested in."), 
      p("If there is truly no difference between the groups, there would be no significant difference in the test statistic when 
  we randomly assigning their values to the another group.")
     ),#sidebarPanel
     mainPanel(
       h6("Summary of Original Data"),
       tableOutput("summary"),
      h6("Observed Mean Difference"),
      verbatimTextOutput("observedDiff"),
      ggvisOutput("trialsHist2"),
      h6("P-Value"),
      verbatimTextOutput("pval"),
      h6("Summary of Permutation Resampling"),
      tableOutput("summary2")
#       hidden(
#         tableOutput("contents")
        # )
    ) #mainPanel
   )#sidebarLayout
)
)