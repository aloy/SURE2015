library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
library(mosaic)
library(Lock5Data)
library(dplyr)

tv <- read.csv("../data/TV.csv")

filedata <- reactive({
  if(input$chooseData2=="uploadYes"){
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

output$contents <- renderTable({
  filedata()
})

shinyjs::onclick("hideData",
                 shinyjs::toggle(id = "contents", anim = TRUE))

shinyjs::onclick("hideDataOptions",
                 shinyjs::toggle(id = "dataOptions", anim = TRUE))

observe({
  data <- filedata()
  cvars <- colnames(data)[sapply(data,is.factor)]
  qvars <- colnames(data)[sapply(data,is.numeric)]
  updateSelectInput(session, 'group', choices = cvars)
  updateSelectInput(session, 'response', choices = qvars)
})

filteredData<-reactive({
  data<-isolate(filedata())
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
                    his = qplot(data=filteredData(), x=response, facets=group~., binwidth=input$w, 
                                 main="Original Sample"),
                    den = qplot(data=filteredData(), x=response, facets=group~., geom="density"),
                    qq = qplot(sample=response, data=filteredData(), facets=group~.),
                    hisDen = qplot(data=filteredData(), x=response, facets=group~.) + aes(y=..density..)+geom_density()
  )
  dataPlot
})


output$basicSummary <- renderTable({
  favstats(~response|group, data=filteredData())  
  })


origStat <- reactive({ #Our original data frame, with all of the calculations we want (for bias)
  grouped_trials <- group_by(simdata(), catVals..)
  group_means <- summarise(grouped_trials, mean=mean(quantVals..), med=median(quantVals..), sd = sd(quantVals..))
  summarise(group_means, mean.diff=diff(mean), med.diff=diff(med),
            mean.ratio = mean[1] / mean[2], med.ratio=med[1]/med[2], sd.ratio = sd[1]/sd[2])
  
})

trials <- reactive({
  
  if(input$goButton > 0) {
    if(input$stat=="bootMean"){
      perms <- do(input$num) * diff(mean(response ~ shuffle(group), data = filteredData()))
      colnames(perms) <- "perms"
    }
    if(input$stat=="bootMedian"){
      perms <- do(input$num) * diff(median(response ~ shuffle(group), data = filteredData()))
      colnames(perms) <- "perms"
    }
    if(input$stat=="bootMeanRatio"){
      perms0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      grouped_trials <- group_by(perms0, .index, group)
      perms <- summarise(summarise(grouped_trials, mean=mean(response)), mean.ratio = mean[1]/mean[2])
      names(perms) <- c("index", "perms")
    }
    if(input$stat=="bootMedianRatio"){
      perms0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      grouped_trials <- group_by(perms0, .index, group)
      perms <- summarise(summarise(grouped_trials, median=median(response)), med.ratio = median[1]/median[2])
      names(perms) <- c("index", "perms")
    }
    if(input$stat=="bootSdRatio"){
      perms0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      grouped_trials <- group_by(perms0, .index, group)
      perms <- summarise(summarise(grouped_trials, sd=sd(response)), sd.ratio = sd[1]/sd[2])
      names(perms) <- c("index", "perms")
    }
    data.frame(perms)
  } else {
    data.frame(perms = rep(0, 10))
  }
})

observe({
  if(input$plot2=="his2"){
    trials %>%
      ggvis(~perms) %>%
      layer_histograms(width = input_slider(0.001, 0.5, step=0.001, value=0.06)) %>%
      bind_shiny("bootHist", "bootHist_ui")
  }
  if(input$plot2=="den2"){
    trials %>%
      ggvis(~perms) %>%
      layer_densities() %>%
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
  normal.quantiles <- qnorm(probabilities, mean(trials()$perms, na.rm = T), sd(trials()$perms, na.rm = T))
  qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$perms))
  colnames(qqdata1) <- c("normal.quantiles", "diffs")
  data.frame(qqdata1)
})

observed <- reactive({
  switch(input$stat,
         bootMean= summarise(summarise(group_by(filteredData(), group), mean=mean(response)), 
                             stat=diff(mean)),
         bootMedian= summarise(summarise(group_by(filteredData(), group), median=median(response)), 
                               stat=diff(median)),
         bootMeanRatio = summarise(summarise(group_by(filteredData(), group), mean=mean(response)), 
                        stat=mean[1]/mean[2]),
         bootMedianRatio =  summarise(summarise(group_by(filteredData(), group), median=median(response)), 
                                      stat=median[1]/median[2]),
         bootSdRatio = summarise(summarise(group_by(filteredData(), group), sd=sd(response)), 
                                 stat=sd[1]/sd[2])
  )
})

output$hisDenPlot2 <- renderPlot ({
  qplot(perms, data=trials(), ylab = "Density", binwidth=input$w2) + aes(y=..density..) + geom_density()
})

output$bootSummary <- renderTable({
  favstats(~perms, data=trials())
})

output$bootBias <- renderPrint({
  observed()$stat-mean(trials()$perms)
 })

output$bootSd <- renderPrint({
  sd(trials()$perms)
})

level <- reactive(
  input$level
)

alpha <- reactive(
  1 - level()
)

SE <- reactive (
  sd(trials()$perms)
)

output$ciPrint <- renderPrint({
  quantile(trials()$perms, 
           probs=c(alpha()/2, 1-alpha()/2))
})

output$percLower <- renderPrint({
  quantile(trials()$perms, probs = c(alpha()))
})

output$percUpper <- renderPrint({
  quantile(trials()$perms, probs = c(1-alpha()))
})

output$normPrint <- renderText({
  c(mean(trials()$perms) - qnorm(1-alpha()/2)*SE(), observed()$stat + qnorm(1-(alpha()/2)) * SE())
})

output$normLower <- renderText({
  c(paste(100*alpha(),'%'), observed()$stat - qnorm(1-alpha()) * SE())
})

output$normUpper <- renderText({
  c(paste(100*level(),'%'), observed()$stat + qnorm(1-alpha()) * SE())
  
})

})