library("ggvis")
library("Lock5Data")
data("CaffeineTaps")
library(mosaic)

shinyServer(function(input, output, session) {
  # A reactive subset of mtcars
  
  filedata <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      return(      
        read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else
      read.csv("~/Desktop/SURE2015/Permutation Tests/data/CaffeineTaps.csv")
  })
  
  output$varChoose <- renderUI({
    df <- filedata()
    vars <- colnames(df)[sapply(df,is.factor)]
    names(vars)=vars
    if(input$chooseData=="uploadNo")
      textInput("choose",label="Choose a categorical variable with two groups of equal size:", value="Group")
    else
      selectInput("choose",label="Choose a categorical variable with two groups of equal size:",vars)
  })
  
  output$varChoose2 <- renderUI({
    df <- filedata()
    vars2 <- colnames(df)[sapply(df,is.numeric)]
    names(vars2)=vars2
    if(input$chooseData=="uploadNo")
      textInput("choose2",label="Choose a quantitative variable to examine between groups:", value="Taps")
    else
      selectInput("choose2",label="Choose a quantitative variable to examine between groups:",vars2)
  })
  
#   Category <- reactive({ 
#     catName <- input$choose
#     filedata()[,catName]
#   })
#   
#   Quantitative <- reactive({ 
#     quantName<-input$choose2
#     filedata()[,quantName]
#   })
  
  simdata <- reactive({
    catName <- input$choose
    Category <- filedata()[,catName]
    quantName <- input$choose2
    Quantitative <- filedata()[,quantName]
    data.frame(Category, Quantitative)  
  })
  
output$test <- renderTable({
  head(simdata())
})

  trials <- reactive({
    perms <- do(input$n) * diff(mean(Quantitative ~ shuffle(Category), data = simdata()))
    colnames(perms) <- "perms"
    data.frame(perms)
  })
  
  input_width <- reactive(input$w)
  
  # A simple visualisation. In shiny apps, need to register observers
  # and tell shiny where to put the controls
  trials %>%
    ggvis(~perms) %>%
    layer_histograms(width = input_width) %>%
    bind_shiny("plot", "plot_ui")
  
  output$stats <- renderTable({
    favstats(~perms, data = trials())
  })
})