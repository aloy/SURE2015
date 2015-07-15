library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
library(mosaic)
library(Lock5Data)
library(plyr)
library(ggvis)

tv <- read.csv("../data/TV.csv")

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
                    his = qplot(data=filteredData(), x=response, facets=group~., binwidth=input$w, 
                                 main="Original Sample"),
                    den = qplot(data=filteredData(), x=response, facets=group~., geom="density"),
                    qq = qplot(sample=response, data=filteredData(), facets=group~.),
                    hisDen = qplot(data=filteredData(), x=response, facets=group~., 
                                   binwidth=input$w) + aes(y=..density..)+geom_density()
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
      result <- do(input$num) * diff(mean(response ~ shuffle(group), data = filteredData()))
      colnames(result) <- "result"
    }
    if(input$stat=="bootMedian"){
      result <- do(input$num) * diff(median(response ~ shuffle(group), data = filteredData()))
      colnames(result) <- "result"
    }
    if(input$stat=="bootMeanRatio"){
      result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      df <- ddply(result0, .(.index, group), summarise, mean=mean(response))
      result <- ddply(df, .(.index), summarise, mean.ratio=mean[1]/mean[2])
      names(result) <- c("index", "result")
    }
    if(input$stat=="bootMedRatio"){
      result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      df <- ddply(result0, .(.index, group), summarise, median=median(response))
      result <- ddply(df, .(.index), summarise, median.ratio=median[1]/median[2])
      names(result) <- c("index", "result")
    }
    if(input$stat=="bootSdRatio"){
      result0 <- do(input$num) * sample(group_by(filteredData(), group), replace = TRUE)
      df <- ddply(result0, .(.index, group), summarise, sd=sd(response))
      result <- ddply(df, .(.index), summarise, sd=sd[1]/sd[2])
      names(result) <- c("index", "result")
    }
    data.frame(result)
  } else {
    data.frame(result = rep(0, 10))
  }
})

output$trials <- renderDataTable(trials() %>% head)

observe({
    if(input$reset > 0 ){
      trials <- data.frame(result=rep(0, 10))
      output$trials <- renderDataTable(data.frame(result = rep(0, 10)))
    }
  if(input$plot2=="his2"){
    trials %>%
      ggvis(~result) %>%
      layer_histograms(width = input_slider(0.001, 0.5, step=0.001, value=0.2)) %>%
      bind_shiny("bootHist", "bootHist_ui")
  }
  if(input$plot2=="den2"){
    trials %>%
      ggvis(~result) %>%
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
  normal.quantiles <- qnorm(probabilities, mean(trials()$result, na.rm = T), sd(trials()$result, na.rm = T))
  qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$result))
  colnames(qqdata1) <- c("normal.quantiles", "diffs")
  data.frame(qqdata1)
})

observed <- reactive({
  switch(input$stat,
         bootMean= summarise(ddply(filteredData(), .(group), summarise, mean=mean(response)), stat=diff(mean)),
         bootMedian= summarise(ddply(filteredData(), .(group), summarise, median=median(response)), stat=diff(median)),
         bootMeanRatio = summarise(ddply(filteredData(), .(group), summarise, mean=mean(response)), stat=mean[1]/mean[2]),
         bootMedRatio =  summarise(ddply(filteredData(), .(group), summarise, median=median(response)),
                                   stat=median[1]/median[2]),
         bootSdRatio = summarise(ddply(filteredData(), .(group), summarise, sd=sd(response)), stat=sd[1]/sd[2])
  )
})

output$hisDenPlot2 <- renderPlot ({
  qplot(result, data=trials(), ylab = "Density", binwidth=input$w2) + aes(y=..density..) + geom_density()
})

    output$bootSummary <- renderPrint({ 
      round(mean(trials()$result), digits=3)
      })


output$bootBias <- renderPrint({
  signif(observed()$stat-mean(trials()$result), digits=3)
 })

output$bootSd <- renderPrint({
  signif(sd(trials()$result), digits=3)
})

level <- reactive(
  input$level
)

alpha <- reactive(
  1 - level()
)

SE <- reactive (
  sd(trials()$result)
)

output$ciPrint <- renderPrint({
  round(quantile(trials()$result, 
           probs=c(alpha()/2, 1-alpha()/2)), digits=3)
})

output$percLower <- renderPrint({
  round(quantile(trials()$result, probs = c(alpha())), digits=3)
})

output$percUpper <- renderPrint({
  round(quantile(trials()$result, probs = c(1-alpha())), digits=3)
})

output$normPrint <- renderText({
  c(round(mean(trials()$result) - qnorm(1-alpha()/2)*SE(), digits=3),
     round(observed()$stat + qnorm(1-(alpha()/2)) * SE(), digits=3))
})

output$normLower <- renderText({
  c(paste(100*alpha(),'%'), 
    round(observed()$stat - qnorm(1-alpha()) * SE(),  digits=3))
})

output$normUpper <- renderText({
  c(paste(100*level(),'%'), round(observed()$stat + qnorm(1-alpha()) * SE(), digits=3))
  
})

})