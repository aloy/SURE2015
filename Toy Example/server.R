library(shiny)
library(shinyjs)

shinyServer(function(input, output, session) {
  library(ggplot2)
  library(dplyr)
  library(mosaic)
  
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
      read.csv("~/Desktop/Example/data/TV.csv")
    })
  
  output$contents <- renderTable({
    filedata()
  })
  
  output$boot <- renderUI({
    df <- filedata()
    if (is.null(df)) 
      return(NULL)
    else
    vars <- colnames(df)[sapply(df,is.factor)]
    names(vars)=vars
    selectInput("boot2","Choose a categorical variable to bootstrap:",vars)
    
  })
  
  output$bootAgain<- renderUI({
    df <- filedata()
    if (is.null(df)) 
      return(NULL)
    else
    vars2 <-colnames(df)[sapply(df,is.numeric)]
    names(vars2)=vars2
    selectInput("boot3","Choose a quantitative variable",vars2)
  })
  
  grp <- reactive({ input$boot2})
  qvar <- reactive({ input$boot3})
  
#   colnames(filedata())[colnames(filedata()) == as.character(grp())] <- "group"
#   colnames(filedata())[colnames(filedata()) == as.character(qvar())] <- "variable"
  
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "contents", anim = TRUE))

  catVals <- reactive({
    catVals <- input$boot2
    filedata()[,catVals] #list of categorical values
  })
  
  quantVals <- reactive({
    quantVals<-input$boot3
    filedata()[,quantVals] # list of all quantitative values
  })
  
output$plot <- renderPlot({
  x<-paste(input$boot2, "~", ".")
  qplot(quantVals(), data=filedata(), facets=x)
})

cat <- reactive({
  filedata()[,input$boot2] 
})

trials2 <- reactive({
  do(input$num) * sample(group_by(filedata(), cat()), replace = TRUE)
})

statBase <- reactive({
grouped_trials <- group_by(trialsBase(), .index, Cable) 
summarise(group_by(grouped_trials, .index, Cable), mean(Time))
})

# grouped <- group_by(tv, Cable)
# B <- 1000
# trials <- do(B) * sample(grouped, replace = TRUE)
# 
# grouped_trials <- group_by(trials, .index, Cable)
# group_means <- summarise(grouped_trials, mean = mean(Time))
# diffs <- summarise(group_means, mean.diff = diff(mean))

output$trials2 <- renderPrint({
 colnames(filedata())[colnames(filedata()) == grp()] <- "group"
})

output$statTest <- renderTable({
  grouped_trials <- group_by(trials2(), .index, cat..) 
  y <- paste(input$boot3)
  z <- paste(input$boot2)
  index <- c(".index")
 head(aggregate(y~z, data=grouped_trials, mean))
})

})