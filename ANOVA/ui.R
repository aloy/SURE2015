library(shiny)
library(shinyjs)
library(ggvis)
shinyUI(bootstrapPage(
  useShinyjs(),
  titlePanel("ANOVA"),
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
    ), #conditionalPanel
    conditionalPanel(
      "$('li.active a').first().html()==='Summaries'",
    radioButtons("plot", label=h4("Plotting"), c("Boxplot" = "box", "Histogram"="his", "Kernel Density" = "den",
       "Histogram with Kernel Density"="hisDen", "Q-Q Plot"="qq", "Residual Plot" = "resid"), selected="box"),
    sliderInput("w", label=h4("Histogram Bin Width"), min=0.1, max=1, step=0.1, value=0.5)
      ),
    conditionalPanel(
      "$('li.active a').first().html()==='ANOVA'",
      radioButtons("stat", label="", c("ANOVA" = "printANOVA", "Individual Confidence Intervals" = "individualCI",
                   "Multiple Comparison Confidence Intervals" = "multCI"), 
                   selected="printANOVA"),
      conditionalPanel(
        condition='input.stat !="ANOVA"',
        sliderInput("level", label="Confidence Level", min=0.01, max=0.99, step=0.01, value=0.95)
      )
    ),
      conditionalPanel(
        "$('li.active a').first().html()==='Permutation F-Test'",
        numericInput("num", label="Number of Permutations", min=1, max=100000, step=1, value=1000),
        actionButton("goButton", "Permute!"), actionButton("reset", "Reset"),
        radioButtons("plot2", label=h4("Plotting"), c("Histogram"="his2", "Density"="den2", 
                     "Histogram and Kernel Density" = "hisDen2","Q-Q Plot" = "qq2"),
                     selected="his2"),
        h4("Histogram Bin Width"),
        conditionalPanel(
          condition="input.plot2=='hisDen2'",
          sliderInput("w2", 
                      label = "",
                      value =3.5, step=0.01, min = 0.01, max=7.5)
        ),
        conditionalPanel(
          condition="input.plot2 != 'hisDen2'",
          uiOutput("hist_ui")
        )
      )
    ), #sidebarPanel
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Input",
                         dataTableOutput("contents")               
        ), #tabPanel
        tabPanel("Summaries",
                 conditionalPanel(
                   condition='input.plot=="box" || input.plot== "qq" || input.plot== "resid"',
                 ggvisOutput("origBox")
                 ),
                 conditionalPanel(
                   condition='input.plot == "his" || input.plot== "den" || input.plot=="hisDen"',
                   plotOutput("origPlot")
                 ),
                 h6("Summary"),
                 tableOutput("summary"),
                 h6("Observed F-Statistic"),
                 verbatimTextOutput("f")
                 ),
        tabPanel("ANOVA",
              verbatimTextOutput("anova")
              ),
        tabPanel("Permutation F-Test",
                 conditionalPanel(
                   condition="input.plot2=='hisDen2'",
                   plotOutput("hisDen2")
                 ),
                 conditionalPanel(
                   condition="input.plot2 != 'hisDen2'",
                   ggvisOutput("hist")
                 ),
                 h6("Summary"),
                 tableOutput("summary2"),
                 actionButton("hideData", "Show/hide data set"),
                 hidden(
                 dataTableOutput("trials")
                 )
                 )
    ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
  ))