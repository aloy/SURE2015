library(shiny)
library(shinyjs)
library(ggvis)
library(dplyr)
shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("Permutation Tests"),
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
      ), #conditionalPanel
      selectInput('group', label=h4('Grouping variable') ,'group'),
      selectInput('response', label=h4('Response variable'), 'response')
#       p("Permutation tests compare whether or not there is a significant difference between two experimental groups performing 
#     the same task. We resample without replacement from the pooled data set of both groups, assigning the first half of resamples 
#     to the first group and the second half to the second group, and then calculate the statistic we're interested in."), 
#       p("If there is truly no difference between the groups, there would be no significant difference in the test statistic when 
#   we randomly assigning their values to the another group.")
      ), #conditionalPanel
      conditionalPanel(
        "$('li.active a').first().html()==='Summaries'",
        radioButtons("plot", label=h4("Plotting"),
                     c("Histogram" = "his", "Kernel Density" = "den", "Histogram and Kernel Density" = "hisDen",
                       "Q-Q Plot" = "qq"), selected="his"),
        sliderInput("w", 
                    label = h4("Histogram Bin Width"), 
                    value = 1.2, step=0.1, min = 0.1, max=3)
      ),
      conditionalPanel(
        "$('li.active a').first().html()==='Permutation Test'",
      radioButtons("test", label=h4("Permutation Test"), c("Two-Tailed" = "tt", "Lower Tail" = "lt", "Upper Tail" = "ut"), 
                   selected="tt"),
      numericInput("num", 
                   label = h4("Permutation Resamples"), 
                   value = 1000, min = 1, max = 100000),
      actionButton("goButton", "Permute!"), actionButton("reset", "Reset"),
      radioButtons("plot2", label=h4("Plotting"),
                   c("Histogram" = "his2", "Kernel Density" = "den2", "Histogram and Kernel Density" = "hisDen2",
                     "Q-Q Plot" = "qq2"), selected="his2"),
      h4("Histogram Bin Width"),
      conditionalPanel(
        condition="input.plot2=='hisDen2'",
        sliderInput("w2", 
                    label = "", 
                    value = 0.55, step=0.05, min = 0.05, max=1)
      ),
      conditionalPanel(
        condition="input.plot2 != 'hisDen2'",
        uiOutput("trialsHist_ui")
      )
      )
     ),#sidebarPanel
     mainPanel(
       tabsetPanel(type="tabs",
                   tabPanel("Input",
                              dataTableOutput("contents")
                   ), #tabPanel
                   tabPanel("Summaries",
        plotOutput("origHist"),
       h6("Summary of Original Data"),
       tableOutput("summary"),
      h6("Observed Mean Difference"),
      verbatimTextOutput("observedDiff")
                   ), #tabPanel
      tabPanel("Permutation Test",
      conditionalPanel(
        condition="input.plot2=='hisDen2'",
        plotOutput("hisDenPlot")
      ),
      conditionalPanel(
        condition="input.plot2 != 'hisDen2'",
        ggvisOutput("trialsHist")  
      ),
      h6("P-Value"),
      verbatimTextOutput("pval"),
      h6("Summary"),
      tableOutput("summary2"),
      actionButton("hideData", "Show/hide data set"),
      hidden(
      dataTableOutput("trials")
      )
      ) #tabPanel
     ) #tabsetPanel
    ) #mainPanel
   )#sidebarLayout
)
)