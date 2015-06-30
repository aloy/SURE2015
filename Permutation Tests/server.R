library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
library(Lock5Data)

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
output$contents <- renderTable({
  filedata()
})

shinyjs::onclick("hideData",
                 shinyjs::toggle(id = "contents", anim = TRUE))
shinyjs::onclick("hideDataOptions",
                 shinyjs::toggle(id = "dataOptions", anim = TRUE))

output$varChoose <- renderUI({
  df <- filedata()
  vars <- colnames(df)[sapply(df,is.factor)]
  names(vars)=vars
  if(input$chooseData=="uploadNo")
    textInput("choose",label="Choose a categorical variable with two groups:", value="Group")
  else
    selectInput("choose",label="Choose a categorical variable with two groups:",vars)
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

catVals <- reactive({ 
  catName <- input$choose
  filedata()[,catName]
})

quantVals <- reactive({ 
  quantName<-input$choose2
  filedata()[,quantName]
})

output$observedDiff <- renderPrint({
  catName <- input$choose
  quantName<-input$choose2
tapply(quantVals(), catVals(), mean)
})



})