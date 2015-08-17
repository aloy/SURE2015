library(shiny)
library(shinyjs)
library(mosaic)
library(Lock5Data)
library(ggvis)
library(ggplot2)
data(FishGills3)

# ANOVA

shinyServer(function(input,output, session){

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
      data.frame(FishGills3)
  })
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "trials", anim = TRUE))

  observe({
    data <- theData()
    cvars <- colnames(data)[sapply(data,nlevels) > 1]
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'group', choices = cvars)
    updateSelectInput(session, 'response', choices = qvars)
  })
  
  filteredData <- reactive({
    data<-isolate(theData())
    #if there is no input, make a dummy dataframe
    if(input$group=="group" && input$response=="response"){
      if(is.null(data)){
        data <- data.frame(group=letters[1:10], response=1:10)
      }
    }else{
      data <- data[,c(input$group,input$response)]
    }
    names(data)<-c("group","response")
    as.data.frame(data)
  })
  
  observe({
    model <- lm(response~factor(group), data=filteredData())
    data <- data.frame(isolate(filteredData()))
    data$resid <- resid(model)
    
   if(input$plot=="box"){
   filteredData() %>%
    ggvis(x=~group, y=~response) %>%
    layer_boxplots() %>%
    bind_shiny("origBox")
   }
   if(input$plot=="resid"){
   data %>%
     ggvis(x=~group, y=~resid)  %>%
     layer_points() %>%
     bind_shiny("origBox")
   }
})

output$origPlot <- renderPlot({
plot <- switch(input$plot,
   his= ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19", binwidth=input$w) +
    facet_grid(.~group) + theme(panel.grid.minor = element_line(colour = "grey"), panel.background = element_rect(fill = "white"),
    axis.line = element_line(colour="black"), axis.text = element_text(colour = "black")),
   den = ggplot(filteredData(), aes(response)) +
     geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + facet_grid(.~group) + 
     theme(panel.grid.minor = element_line(colour = "grey"), panel.background = element_rect(fill = "white"),
     axis.line = element_line(colour="black"), axis.text = element_text(colour = "black")),
  hisDen=  ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19", binwidth=input$w, aes(y=..density..)) +
    geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + facet_grid(.~group) + theme(panel.grid.minor = element_line(colour = "grey"),
   panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
   axis.text = element_text(colour = "black")),
  qq= ggplot(filteredData(), aes(sample=response)) + stat_qq() + facet_grid(.~group, scales="free") + theme(aspect.ratio=1, panel.grid.minor = element_line(colour = "grey"),
  panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black")),
  hisDen = ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19", binwidth=input$w, aes(y=..density..)) +  
    geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + facet_grid(.~group)
)
plot
})

observe({
  range <- diff(range(filteredData()$response))
  updateSliderInput(session, "w", min=round(range/100, digits=2), 
          max=round(range/5, digits=2), step=round(range/100, digits=2), value=round(range/10, digits=2))
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
  favstats(~response|factor(group), data=filteredData())
  })

output$anova <- renderTable({
  model <- lm(response~factor(group), data=filteredData())
  anova(model)
})

output$anova2 <- renderTable({
  model2 <- lm(response~factor(group) -1, data=filteredData())
  alpha <- 1- input$level
  print <- switch(input$stat,
individualCI = confint(model2, level=input$level),
multCI = confint(model2, level= 1-(alpha/(2*nlevels(factor(filteredData()$group)))))
)
print
  })

trials <- reactive({
  if(input$goButton > 0) {
perms <-do(input$num) * summary(lm(formula = response ~ shuffle(factor(group)), data = filteredData()))$fstatistic[1]
colnames(perms) <- c("perms")
data.frame(perms)
  } else {
    data.frame(perms = rep(0, 10))
  }
})

output$trials <- renderDataTable(trials(), options = list(pageLength = 10))

observe({
#   if(input$reset > 0 ){
#     trials <- data.frame(perms=rep(0, 10))
#     output$trials <- renderDataTable(data.frame(perms = rep(0, 10)))
#   }
  if(input$goButton > 0){
    range2.100 <- round(diff(range(trials()$perms))/100, digits=3)
    if (round(diff(range(trials()$perms))/100, digits=3) == 0)
      range2.100 <- 0.001
  }
  else(
    range2.100 <- 0.001
  )
  if(input$plot2=="his2"){
    trials %>%
      ggvis(~perms) %>%
      layer_histograms(width = input_slider(range2.100, range2.100*50,
                                            value=range2.100*10, step=0.001)) %>%
      bind_shiny("hist", "hist_ui")
  }
  if(input$plot2=="den2"){
    trials %>%
      ggvis(~perms) %>%
      layer_densities(fill := "dodgerblue") %>%
      add_axis("y", title="Density") %>%
      bind_shiny("hist", "hist_ui")
  }
  if(input$plot2=="qq2"){
    qqdata2 %>% 
      ggvis(~normal.quantiles, ~diffs) %>% 
      layer_points() %>% 
      add_axis("x", title="Theoretical") %>%
      add_axis("y", title="Sample") %>%
      bind_shiny("hist", "hist_ui")
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

output$hisDen2 <- renderPlot({
  ggplot(data=trials(), aes(x=perms)) + geom_histogram(colour="black", fill="grey19", 
    binwidth=input$w2, aes(y=..density..)) + geom_density(colour="royalblue", fill="royalblue", alpha=0.6) + 
    theme(panel.grid.minor = element_line(colour = "grey"), panel.background = element_rect(fill = "white"), 
    axis.line = element_line(colour="black"), axis.text = element_text(colour = "black"))
})

output$slider2 <- renderUI({
  if (round(diff(range(trials()$perms))/100, digits=3) == 0){
    range2.100 <- 0.01
  }
  else{
    range2.100 <- round(diff(range(trials()$perms))/100, digits=3)
  }
  sliderInput("w2", "", min=range2.100, max=range2.100*50, value=range2.100*10, step=.001)
})

output$summary2 <- renderTable({
favstats(perms, data=trials())
})

})