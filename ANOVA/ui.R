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
    radioButtons("plot", label=h4("Plotting"), c("Boxplot" = "box", "Facetted Histograms"="his",
       "Facetted Histograms with Kernel Density"="hisDen", "Q-Q Plot"="qq", "Residual Plots" = "resid"), selected="box"),
    sliderInput("w", label=h4("Histogram Bin Width"), min=0.1, max=1, step=0.1, value=0.5)
      ),
    conditionalPanel(
      "$('li.active a').first().html()==='ANOVA'",
      radioButtons("stat", label="", c("ANOVA" = "printANOVA", "Individual Confidence Intervals" = "individualCI",
                   "Multiple Comparison Confidence Intervals" = "multCI"), 
                   selected="printANOVA"),
      conditionalPanel(
        condition='input.stat !="ANOVA"',
        numericInput("level", label="Confidence Level", min=0.01, max=0.99, step=0.01, value=0.95)
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
                   condition='input.plot == "his" || input.plot=="hisDen"',
                   plotOutput("origPlot")
                 ),
                 h6("Summary Statistics"),
                 tableOutput("summary"),
                 h6("Observed F-Statistic"),
                 verbatimTextOutput("f")
                 ),
        tabPanel("ANOVA",
              verbatimTextOutput("anova")
              )
#         tabPanel("Permutation F-Test",
#                  )
    ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
  ))