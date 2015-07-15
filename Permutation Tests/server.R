library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
library(mosaic)
library(ggvis)
library(dplyr)
library(Lock5Data)
data("CaffeineTaps")

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
                    his = qplot(data=filteredData(), x=response, facets=group~., binwidth=input$w, 
                                main="Original Sample"),
                    den = qplot(data=filteredData(), x=response, facets=group~., geom="density"),
                    qq = qplot(sample=response, data=filteredData(), facets=group~.),
                    hisDen = qplot(data=filteredData(), x=response, facets=group~., binwidth=input$w) 
                    + aes(y=..density..)+geom_density()
  )
  dataPlot
})

output$summary <- renderTable({
  favstats(~response|group, data=filteredData())  
})

observedDiff <- reactive({
  diff(mean(response, groups=group, data=filteredData()))
})

output$observedDiff <- renderText({
  observedDiff()
})


trials <- reactive({
  
  if(input$goButton > 0) {
    perms <- do(input$num) * diff(mean(response ~ shuffle(group), data = filteredData()))
    colnames(perms) <- "perms"
    data.frame(perms)
  } else {
    data.frame(perms = rep(0, 10))
  }
})

output$trials <- renderDataTable(trials(), options = list(pageLength = 10))

output$pval <- renderText({
n <- input$num
if(input$goButton > 0) {
  pvalSwitch <- switch(input$test, 
                       tt = min((sum(trials()$perms>=observedDiff())+1)/(n+1),
                                (sum(trials()$perms<=observedDiff())+1)/(n+1))*2,
                       lt = (sum(trials()$perms <= observedDiff()) +1)/(n+1),
                       ut = (sum(trials()$perms>=observedDiff())+1)/(n+1)
  )
signif(pvalSwitch, 4)
}
else{
  return(0)
}
  })

qqdata <- reactive({
  n <- input$num
  probabilities <- (1:n)/(1+n)
  normal.quantiles <- qnorm(probabilities, mean(trials()[,"perms"], na.rm = T), sd(trials()[,"perms"], na.rm = T))
  qqdata0 <- data.frame(sort(normal.quantiles), sort(trials()[,"perms"]))
  colnames(qqdata0) <- c("normal.quantiles", "perms")
  data.frame(qqdata0)
  })

observe({
  if(input$reset > 0 ){
    trials <- data.frame(perms=rep(0, 10))
    output$trials <- renderDataTable(data.frame(perms = rep(0, 10)))
  }
  if(input$plot2=="his2"){
    trials %>%
      ggvis(~perms) %>%
      layer_histograms(width = input_slider(0.01, 0.5, step=0.01, value=0.2)) %>%
      bind_shiny("trialsHist", "trialsHist_ui")
  }
  if(input$plot2=="den2"){
    trials %>%
      ggvis(~perms) %>%
      layer_densities() %>%
      add_axis("y", title="Density") %>%
      bind_shiny("trialsHist", "trialsHist_ui")
  }
  if(input$plot2=="qq2"){
    qqdata %>% 
      ggvis(~normal.quantiles, ~perms) %>% 
      layer_points() %>% 
      add_axis("x", title="Theoretical") %>%
      add_axis("y", title="Sample") %>%
      bind_shiny("trialsHist", "trialsHist_ui")
  }
})

output$summary2 <- renderTable({
  favstats(~perms, data=trials())  
})

output$hisDenPlot <- renderPlot ({
  qplot(perms, data=trials(), ylab = "Density", binwidth=input$w2) + aes(y=..density..) + geom_density()
})


})