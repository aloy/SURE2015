library(shiny)
library(shinyjs)

shinyServer(function(input, output, session) {
  library(ggplot2)
  library(dplyr)
  library(mosaic)
  library(magrittr)
  library(ggvis)
  
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
      read.csv("~/Desktop/SURE2015/Toy Example/data/TV.csv")
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

simdata <- reactive({ 
  data.frame(col1= catVals(), col2= quantVals())
})

trials <- reactive({
  do(input$num) * sample(group_by(simdata(), catVals..), replace = TRUE)
})

# grouped <- group_by(tv, Cable)
# B <- 1000
# trials <- do(B) * sample(grouped, replace = TRUE)
# 
# grouped_trials <- group_by(trials, .index, Cable)
# group_means <- summarise(grouped_trials, mean = mean(Time))
# diffs <- summarise(group_means, mean.diff = diff(mean))

output$ggvisplot_ui <- renderUI({
    trials() %>% 
    ggvis(~quantVals..) %>% 
    layer_histograms(width = input$width) %>% 
    add_axis("x", title = "means") %>%
    bind_shiny("ggvisplot")
    ggvisOutput("ggvisplot")
})

allStat <- reactive({
grouped_trials <- group_by(trials(), .index, catVals..)
group_means <- summarise(grouped_trials, mean=mean(quantVals..), med=median(quantVals..)*1.0, sd = sd(quantVals..))
summarise(group_means, mean.diff=diff(mean), med.diff=diff(med) * 1.0,
               sd.diff=diff(sd), mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2]*1.0, sd.ratio = sd[1]/sd[2])
})

output$statTest <- renderTable({
  head(allStat())
})

})