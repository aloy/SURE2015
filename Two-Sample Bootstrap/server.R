library(shiny)
library(shinyjs)
library(mosaic)
# library(Lock5Data)
library(ggvis)
library(resample)
shinyServer(function(input, output, session){

#Two-Sample Bootstrap

tv <- read.csv("http://aloy.github.io/data/TV.csv")

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
    as.data.frame(tv)
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

output$basicSummary <- renderTable({
  favstats(~response|group, data=filteredData())  
  })

bootResult <- reactive({
  if(input$goButton > 0) {
    if(input$stat=="bootMean"){
      #     result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      #     result <- summarise(summarise(group_by(result0, .index, group), mean=mean(response)), 
      #                         mean.diff=diff(mean))
      #     names(result) <- c("index", "result")
      BS <- bootstrap2(filteredData(), mean(response), treatment = group, 
                       R = input$num, statisticNames = "Difference in Means")
    }
    if(input$stat=="bootMedian"){
      #     result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      #     result <- summarise(summarise(group_by(result0, .index, group), median=median(response)), 
      #                         median.diff=diff(median))
      #     names(result) <- c("index", "result")
      BS <- bootstrap2(filteredData(), median(response), treatment = group, 
                       R = input$num, statisticNames = "Difference in Medians")
    }
    if(input$stat=="bootMeanRatio"){
      #     result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      #     result <- summarise(summarise(group_by(result0, .index, group), mean=mean(response)), 
      #                         mean.ratio=mean[1]/mean[2])
      #     names(result) <- c("index", "result")
      BS <- bootstrap2(filteredData(), mean(response), treatment = group, 
                       R = input$num, statisticNames = "Ratio of Means", ratio = TRUE)
    }
    if(input$stat=="bootMedRatio"){
      #     result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      #     result <- summarise(summarise(group_by(result0, .index, group), median=median(response)), 
      #                         median.ratio=median[1]/median[2])
      #     names(result) <- c("index", "result")
      BS <- bootstrap2(filteredData(), median(response), treatment = group, 
                       R = input$num, statisticNames = "Ratio of Medians", ratio = TRUE)
    }
    if(input$stat=="bootSdRatio"){
      #     result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      #     result <- summarise(summarise(group_by(result0, .index, group), sd=sd(response)), 
      #                         sd.ratio=sd[1]/sd[2])
      #     names(result) <- c("index", "result")
      BS <-  bootstrap2(filteredData(), sd(response), treatment = group, 
                        R = input$num, statisticNames = "Ratio of SDs", ratio = TRUE)
    }
    BS
  }
})

trials <- reactive({
    if(input$goButton > 0) {
      result <- cbind(index = 1:bootResult()$R, result = bootResult()$replicates)
      colnames(result) <- c("index", "statistic")
      as.data.frame(result)
    } else {
    data.frame(statistic = rep(0, 10))
  }
})

output$trials <- renderDataTable(trials(), options = list(pageLength = 10))

observe({
  
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
       value=range2.100*10, step=0.001))  %>%
      bind_shiny("bootHist", "bootHist_ui")
  }
  if(input$plot2=="den2"){
    trials %>%
      ggvis(~statistic) %>%
      layer_densities(fill := "dodgerblue") %>%
      add_axis("y", title="Density") %>%
      bind_shiny("bootHist", "bootHist_ui")
  }
  if(input$plot2=="qq2"){
    qqdata2 %>% 
      ggvis(~normal.quantiles, ~diffs) %>% 
      layer_points() %>% 
      add_axis("x", title="Theoretical") %>%
      add_axis("y", title="Sample") %>%
      bind_shiny("bootHist", "bootHist_ui")
  }
})

qqdata2 <- reactive({
  n <- input$num
  probabilities <- (1:n)/(1+n)
  normal.quantiles <- qnorm(probabilities, mean(trials()$statistic, na.rm = T), sd(trials()$statistic, na.rm = T))
  qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$statistic))
  colnames(qqdata1) <- c("normal.quantiles", "diffs")
  data.frame(qqdata1)
})

observed <- reactive({
  switch(input$stat,
         bootMean=  summarise(summarise(group_by(filteredData(), group), mean=mean(response)), stat=diff(mean)),
         bootMedian=  summarise(summarise(group_by(filteredData(), group), median=median(response)), stat=diff(median)),
         bootMeanRatio = summarise(summarise(group_by(filteredData(), group), mean=mean(response)), stat=mean[1]/mean[2]),
         bootMedRatio =  summarise(summarise(group_by(filteredData(), group), median=median(response)),
                                   stat=median[1]/median[2]),
         bootSdRatio = summarise(summarise(group_by(filteredData(), group), sd=sd(response)),stat=sd[1]/sd[2])
  )
})

output$hisDenPlot2 <- renderPlot ({
  ggplot(data=trials(), aes(x=statistic)) + geom_histogram(colour="black", fill="grey19",
 binwidth=input$w2, aes(y=..density..)) +  geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + theme(panel.grid.minor = element_line(colour = "grey"), 
panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), axis.text = element_text(colour = "black"))
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


output$origStat <- renderText({ 
  # round(mean(trials()$statistic), digits=3)
  bootResult()$observed
})

output$bootSummary <- renderText({ 
  # round(mean(trials()$statistic), digits=3)
  bootResult()$stats$Mean
})


output$bootBias <- renderText({
 # as.numeric(mean(trials()$statistic) - observed())
  bootResult()$stats$Bias
 })

output$bootSd <- renderText({
  # signif(sd(trials()$statistic), digits=3)
  bootResult()$stats$SE
})

level <- reactive(
  input$level
)

alpha <- reactive(
  1 - level()
)

# SE <- reactive (
#   sd(trials()$result)
# )

output$ciPrint <- renderPrint({
#   round(quantile(trials()$result, 
#            probs=c(alpha()/2, 1-alpha()/2)), digits=3)
#   range.100()
  CI.percentile(bootResult(), probs = c(alpha()/2, 1-alpha()/2))
})

output$percLower <- renderPrint({
  # round(quantile(trials()$result, probs = c(alpha())), digits=3)
  CI.percentile(bootResult(), probs = alpha())
})

output$percUpper <- renderPrint({
  # round(quantile(trials()$result, probs = c(1-alpha())), digits=3)
  CI.percentile(bootResult(), probs = 1-alpha())
})

output$tPrint <- renderPrint({
#   c(round(mean(trials()$result) - qnorm(1-alpha()/2)*SE(), digits=3),
#      round(observed()$stat + qnorm(1-(alpha()/2)) * SE(), digits=3))
  CI.t(bootResult(), probs = c(alpha()/2, 1-alpha()/2))
})

output$tLower <- renderPrint({
#   c(paste(100*alpha(),'%'), 
#     round(observed()$stat - qnorm(1-alpha()) * SE(),  digits=3))
  CI.t(bootResult(), probs = alpha())
})

output$tUpper <- renderPrint({
  # c(paste(100*level(),'%'), round(observed()$stat + qnorm(1-alpha()) * SE(), digits=3))
  CI.t(bootResult(), probs = 1-alpha())
})

})