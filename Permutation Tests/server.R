library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
library(mosaic)
library(ggvis)
library(dplyr)

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

Category <- reactive({ 
  catName <- input$choose
  filedata()[,catName]
})

Quantitative <- reactive({ 
  quantName<-input$choose2
  filedata()[,quantName]
})

output$summary <- renderTable({
favstats(~Quantitative()|Category())  
})

simdata <- reactive({
  data.frame(Category(), Quantitative())  
})

diff <- reactive({
catName <- input$choose
quantName<-input$choose2
grouped<- group_by(simdata(), Category..)
summarise(summarise(grouped, mean = mean(Quantitative..)), mean.diff=mean[1]-mean[2])
})

output$observedDiff <- renderText({
diff()$mean.diff
})


trials <- reactive({
dataSize <- nrow(simdata())
size <- nrow(simdata())/2
n <- input$num
toSample <- simdata()$Quantitative..
result <- matrix(nrow=n)
for (i in 1:n){
index <- sample(dataSize, size=size, replace = FALSE)
result[i,] <- mean(toSample[index]) - mean(toSample[-index])
}
colnames(result) <- c("result")
as.data.frame(result)
})

output$pval <- renderText({
n <- input$num
(sum(trials()$result >= diff()$mean.diff) +1)/(n+1)
})

observe({
  n <- input$num
  probabilities <- (1:n)/(1+n)
  normal.quantiles <- qnorm(probabilities, mean(result, na.rm = T), sd(result, na.rm = T))
  qqdata <- data.frame(sort(normal.quantiles), sort(result))
ggSwitch <- switch(input$type,
  his = as.data.frame(result) %>% 
    ggvis(~result) %>% 
    layer_histograms(width = input_slider(0, 2, step=0.1, value=0.6)) %>%
    add_axis("x", title = "Mean Difference") %>%
    add_axis("y", title="Count") %>%
    add_axis("x", title="Permutation Distribution", ticks=0, orient="top", properties = axis_props(
      axis = list(stroke = "white"),
      labels = list(fontSize = 0))) %>% 
    bind_shiny("trialsHist2", "trialsHist2_ui"),
  den = as.data.frame(result) %>% 
    ggvis(~result) %>% 
    layer_densities() %>%
    add_axis("x", title = "Mean Difference") %>%
    add_axis("y", title="Density") %>%
    add_axis("x", title="Permutation Distribution", ticks=0, orient="top", properties = axis_props(
      axis = list(stroke = "white"),
      labels = list(fontSize = 0))) %>% 
    bind_shiny("trialsHist2", "trialsHist2_ui"),
  qq = qqdata %>% 
    ggvis(~sort.normal.quantiles., ~sort.result.) %>% 
    layer_points() %>% 
    add_axis("x", title="Theoretical") %>%
    add_axis("y", title="Sample") %>%
    bind_shiny("trialsHist2", "trialsHist2_ui")
  )
ggSwitch
})

})