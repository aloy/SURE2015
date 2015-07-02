library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
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

catVals <- reactive({ 
  catName <- input$choose
  filedata()[,catName]
})

quantVals <- reactive({ 
  quantName<-input$choose2
  filedata()[,quantName]
})

output$summary <- renderTable({
favstats(~quantVals()|catVals())  
})

simdata <- reactive({
  data.frame(catVals(), quantVals())  
})

output$observedDiff <- renderPrint({
  catName <- input$choose
  quantName<-input$choose2
  grouped<- group_by(simdata(), catVals..)
diff <- summarise(summarise(grouped, mean = mean(quantVals..)), mean.diff=mean[1]-mean[2])
diff$mean.diff
})

trials <- reactive({
dataSize <- nrow(simdata())
size <- nrow(simdata())/2
n <- input$num
toSample <- simdata()$quantVals..
result <- matrix(nrow=n)
for (i in 1:n){
index <- sample(dataSize, size=size, replace = FALSE)
result[i,] <- mean(toSample[index]) - mean(toSample[-index])
}
colnames(result) <- c("result")
as.data.frame(result)
})

output$trialsHist<- renderPlot({
  qplot(data=trials(), x=result, xlab="xbar1-xbar2", main="Permutation Distribution", binwidth=input$width, asp=1) + 
    geom_vline(xintercept=diff$mean.diff, col="red")
})

output$pval <- renderPrint({
n <- input$num
(sum(trials()$result >= diff$mean.diff) +1)/(n+1)
})

})