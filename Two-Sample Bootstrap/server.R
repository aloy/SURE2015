library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
library(mosaic)
library(Lock5Data)
library(dplyr)

tv <- read.csv("../data/TV.csv")

filedata2 <- reactive({
  if(input$chooseData2=="uploadYes"){
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    return(      
      read.csv(inFile2$datapath, header=input$header2, sep=input$sep2, quote=input$quote2)
    )
  }
  else
    as.data.frame(tv)
})

output$contents2 <- renderTable({
  filedata2()
})

shinyjs::onclick("hideData2",
                 shinyjs::toggle(id = "contents2", anim = TRUE))

shinyjs::onclick("hideDataOptions2",
                 shinyjs::toggle(id = "dataOptions2", anim = TRUE))

output$varChoose2 <- renderUI({
  df2 <- filedata2()
  vars2 <- colnames(df2)[sapply(df2,is.factor)]
  names(vars2)=vars2
  if(input$chooseData2=="uploadNo")
    textInput("choose1",label="Choose a categorical variable with two groups:", value="Cable")
  else
    selectInput("choose1",label="Choose a categorical variable with two groups:",vars2)
})

output$varChoose3 <- renderUI({
  df2 <- filedata2()
  vars3 <- colnames(df2)[sapply(df2,is.numeric)]
  names(vars3)=vars3
  if(input$chooseData2=="uploadNo")
    textInput("choose2",label="Choose a quantitative variable to examine between groups:", value="Time")
  else
    selectInput("choose2",label="Choose a quantitative variable to examine between groups:",vars3)
})

catVals <- reactive({ 
  catVals <- input$choose1
  filedata2()[,catVals]
})

quantVals <- reactive({ 
  quantVals<-input$choose2
  filedata2()[,quantVals]
})

output$origHist2 <- renderPlot({
  f<-paste(input$choose1, "~", ".")
  dataPlot <-switch(input$plot2,
                    his2 = qplot(data=filedata2(), x=quantVals(), facets=f, binwidth=input$w3, 
                                 main="Original Sample", asp=1),
                    den2 = qplot(data=filedata2(), x=quantVals(), facets=f, geom="density", asp=1),
                    qq2 = qplot(sample=quantVals(), data=filedata2(), facets=f, asp=1),
                    hisDen2 = qplot(data=filedata2(), x=quantVals(), facets=f) + aes(y=..density..)+geom_density()
  )
  dataPlot
})

output$basicSummary <- renderPrint({
  favstats(~quantVals()|catVals())  
})

simdata <- reactive({ 
  data.frame(catVals(), quantVals())
})

origStat <- reactive({ #Our original data frame, with all of the calculations we want (for bias)
  grouped_trials <- group_by(simdata(), catVals..)
  group_means <- summarise(grouped_trials, mean=mean(quantVals..), med=median(quantVals..), sd = sd(quantVals..))
  summarise(group_means, mean.diff=diff(mean), med.diff=diff(med),
            mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2], sd.ratio = sd[1]/sd[2])
  
})

trials2 <- reactive({
  do(input$num2) * sample(group_by(simdata(), catVals..), replace = TRUE)
})

allStat <- reactive({
  grouped_trials <- group_by(trials2(), .index, catVals..)
  group_means <- summarise(grouped_trials, mean=mean(quantVals..), med=median(quantVals..) * 1.0, sd = sd(quantVals..))
  summarise(group_means, mean.diff=diff(mean), med.diff=diff(med) * 1.0,
            mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2] * 1.0, sd.ratio = sd[1]/sd[2])
})

x1 = list()
x2 = list()

origStatSwitch <- function(x1, list){
  switch(input$stat2, 
         bootMean2= assign("x1", origStat()$mean.diff),
         bootMedian2= assign("x1", origStat()$med.diff),
         bootMeanRatio = assign("x1", origStat()$mean.ratio),
         bootMedRatio = assign("x1", origStat()$med.ratio),
         bootSdRatio= assign("x1", origStat()$sd.ratio)
  )}


plotType2 <- function(x2, type) {
  switch(type,
         his2 =  qplot(x2, binwidth=input$w4, 
                       ylab="Frequency", asp=1),
         den2 = qplot(x2, geom="density", 
                      ylab="Density", asp=1),
         qq2 = qplot(sample=x2, asp=1),
         hisDen2 = qplot(x2, binwidth=input$w4)+ aes(y=..density..) + geom_density()
  )}

dataSwitch <- function(x2, list){
  switch(input$stat2, 
         bootMean2= assign("x2", allStat()$mean.diff),
         bootMedian2= assign("x2", allStat()$med.diff),
         bootMeanRatio = assign("x2", allStat()$mean.ratio),
         bootMedRatio = assign("x2", allStat()$med.ratio),
         bootSdRatio= assign("x2", allStat()$sd.ratio)
  )}

output$bootHist2 <- renderPlot({
  plotType2(dataSwitch(), input$plot2)
})

output$bootSummary2 <- renderPrint({
  summary(dataSwitch())
})

output$bootBias2 <- renderText({
  mean(origStatSwitch())-mean(dataSwitch())
})


output$bootSd2 <- renderText({
  sd(dataSwitch())
})

level2 <- reactive(
  input$level2
)

alpha2 <- reactive(
  1 - level2()
)

SE2 <- reactive (
  sd(dataSwitch())
)

output$ciPrint2 <- renderPrint({
  quantile(dataSwitch(), 
           probs=c(alpha2()/2, 1-alpha2()/2))
})

output$percLower2 <- renderPrint({
  quantile(dataSwitch(), probs = c(alpha2()))
})

output$percUpper2 <- renderPrint({
  quantile(dataSwitch(), probs = c(1-alpha2()))
})

output$normPrint2 <- renderText({
  c(mean(origStatSwitch()) - qnorm(1-alpha2()/2)*SE2(), mean(origStatSwitch()) + qnorm(1-(alpha2()/2)) * SE2())
})

output$normLower2 <- renderText({
  c(paste(100*alpha2(),'%'), mean(origStatSwitch()) - (qnorm(1-alpha2()) * SE2()))
})

output$normUpper2 <- renderText({
  c(paste(100*level2(),'%'), mean(origStatSwitch()) + qnorm(1-alpha2()) * SE2())
  
})

})