library(shiny)
library(shinyjs)
library(mosaic)
library(ggvis)
library(dplyr)
library(Lock5Data)
library(resample)
shinyServer(function(input,output, session){
data("CaffeineTaps")

#Permutation Tests

theData <- reactive({
  if(input$chooseData=="uploadYes"){
    inFile1 <- input$file1
    if (is.null(inFile1))
      return(NULL)
    return(      
      read.csv(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote)
    )
  }
  else
    data.frame(CaffeineTaps)
})

output$contents <- renderDataTable(theData(), options = list(pageLength = 10))

shinyjs::onclick("hideData",
                 shinyjs::toggle(id = "trials", anim = TRUE))
shinyjs::onclick("hideDataOptions",
                 shinyjs::toggle(id = "dataOptions", anim = TRUE))

observe({
  data <- theData()
  cvars <- colnames(data)[sapply(data,is.factor)]
  qvars <- colnames(data)[sapply(data,is.numeric)]
  updateSelectInput(session, 'group', choices = cvars)
  updateSelectInput(session, 'response', choices = qvars)
})


filteredData<-reactive({
  data<-isolate(theData())
  #if there is no input, make a dummy dataframe
  if(input$group=="group" && input$response=="response"){
    if(is.null(data)){
      data<-data.frame(group=0, response=0)
    }
  }else{
    data <- data[,c(input$group,input$response)]
    names(data)<-c("group","response")
  }
  data
})

output$origHist <- renderPlot({
  dataPlot <-switch(input$plot,
                    his =  ggplot(filteredData(), aes(response)) +ggtitle("Original Sample") + 
                      geom_histogram(colour="black", fill="grey19", binwidth=input$w) + facet_grid(.~group), 
                    den = ggplot(filteredData(), aes(response)) +
                      geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + facet_grid(.~group),
                    qq = ggplot(filteredData(), aes(sample=response)) + stat_qq() + facet_grid(.~group, scales="free") + theme(aspect.ratio=1),
                    hisDen = ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19", binwidth=input$w, aes(y=..density..)) +  
                      geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + facet_grid(.~group) 
  )
  dataPlot
})

output$slider <- renderUI({
  if (round(diff(range(filteredData()$response))/100, digits=2) == 0){
    range.100 <- 0.01
  }
  else(
    range.100 <- round(diff(range(filteredData()$response))/100, digits=2)
  )
  sliderInput("w", "Histogram Bin Width", min=range.100, max=range.100*20, value=range.100*10, step=.01)
})


output$summary <- renderTable({
  favstats(~response|group, data=filteredData())  
})

observedDiff <- reactive({
  diff(mean(response, groups=group, data=filteredData()))
})

output$observedDiff <- renderText({
  round(observedDiff(), digits=3)
})



permResult <- reactive({
  if(input$goButton > 0) {
    PR <- permutationTest2(filteredData(), mean(response), treatment = group, 
                           R = input$num, statisticNames = "Difference in Means")
  }
  PR
})

trials <- reactive({
  
  if(input$goButton > 0) {
#     perms <- do(input$num) * diff(mean(response ~ shuffle(group), data = filteredData()))
#     colnames(perms) <- "perms"
#     data.frame(perms)
    result <- cbind(index = 1:permResult()$R, result = permResult()$replicates)
    colnames(result) <- c("index", "statistic")
    as.data.frame(result)
  } else {
    data.frame(statistic = rep(0, 10))
  }
})

output$trials <- renderDataTable(trials(), options = list(pageLength = 10))

output$pval <- renderText({
# n <- input$num
if(input$goButton > 0) {
  pvalSwitch <- switch(input$test, 
                       tt = resample:::.PermutationStats(x = permResult(), alternative = "two.sided")$PValue,
                       lt = resample:::.PermutationStats(x = permResult(), alternative = "less")$PValue,
                       ut = resample:::.PermutationStats(x = permResult(), alternative = "greater")$PValue
  )
#                        tt = min((sum(trials()$perms>=observedDiff())+1)/(n+1),
#                                 (sum(trials()$perms<=observedDiff())+1)/(n+1))*2,
#                        lt = (sum(trials()$perms <= observedDiff()) +1)/(n+1),
#                        ut = (sum(trials()$perms>=observedDiff())+1)/(n+1)
signif(pvalSwitch, digits = 4)
} else{
  return(0)
}
  })

qqdata <- reactive({
  n <- input$num
  probabilities <- (1:n)/(1+n)
#   normal.quantiles <- qnorm(probabilities, mean(trials()[,"perms"], na.rm = T), sd(trials()[,"perms"], na.rm = T))
  normal.quantiles <- qnorm(probabilities, mean(trials()$statistic, na.rm = T), sd(trials()$statistic, na.rm = T))
  qqdata0 <- data.frame(sort(normal.quantiles), sort(trials()$statistic))
  colnames(qqdata0) <- c("normal.quantiles", "statistic")
  data.frame(qqdata0)
  })

observe({
#   if(input$reset > 0 ){
#     trials <- data.frame(perms=rep(0, 10))
#     output$trials <- renderDataTable(data.frame(perms = rep(0, 10)))
#   }
  if(input$goButton > 0){
    range2.100 <- round(diff(range(trials()$statistic))/100, digits=3)
    if (round(diff(range(trials()$statistic))/100, digits=3) == 0)
      range2.100 <- 0.001
  }
  else(
    range2.100 <- 0.001
  )
  if(input$plot2=="his2"){
    trials %>%
      ggvis(~statistic) %>%
      layer_histograms(width = input_slider(range2.100, range2.100*50,
                                            value=range2.100*10, step=0.001)) %>%
      bind_shiny("trialsHist", "trialsHist_ui")
  }
  if(input$plot2=="den2"){
    trials %>%
      ggvis(~statistic) %>%
      layer_densities(fill := "dodgerblue") %>%
      add_axis("y", title="Density") %>%
      bind_shiny("trialsHist", "trialsHist_ui")
  }
  if(input$plot2=="qq2"){
    qqdata %>% 
      ggvis(~normal.quantiles, ~statistic) %>% 
      layer_points() %>% 
      add_axis("x", title="Theoretical") %>%
      add_axis("y", title="Sample") %>%
      bind_shiny("trialsHist", "trialsHist_ui")
  }
})

output$summary2 <- renderTable({
  favstats(~statistic, data=trials())  
})

output$hisDenPlot <- renderPlot ({
  ggplot(data=trials(), aes(x=statistic)) + geom_histogram(colour="black", fill="grey19", 
  binwidth=input$w2, aes(y=..density..)) + 
    geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + 
    theme(panel.grid.minor = element_line(colour = "grey"), 
          panel.background = element_rect(fill = "white"), 
          axis.line = element_line(colour="black"), 
          axis.text = element_text(colour = "black"))
})

output$slider2 <- renderUI({
  if (round(diff(range(trials()$statistic))/100, digits=3) == 0){
    range2.100 <- 0.01
  }
  else{
    range2.100 <- round(diff(range(trials()$statistic))/100, digits=3)
  }
  sliderInput("w2", "", min=range2.100, max=range2.100*50, value=range2.100*10, step=.001)
})

})