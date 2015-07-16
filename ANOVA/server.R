library(shiny)
library(shinyjs)
library(mosaic)
library(Lock5Data)
library(ggvis)
library(ggplot2)
data(FishGills3)

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

  
  observe({
    data <- theData()
    cvars <- colnames(data)
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
    data.frame(data)
  })
  
  observe({
   if(input$plot=="box"){
   filteredData() %>%
    ggvis(x=~group, y=~response) %>%
    layer_boxplots() %>%
     layer_points() %>%
    bind_shiny("origBox")
   }
   if(input$plot=="qq"){
   qqdata %>% 
     ggvis(~normal.quantiles, ~diffs) %>% 
     layer_points() %>% 
     add_axis("x", title="Theoretical") %>%
     add_axis("y", title="Sample") %>%
     bind_shiny("origBox")
   }
})

output$origPlot <- renderPlot({
plotType <- switch(input$plot,
  his =                 
    ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19", binwidth=input$w) +
    facet_grid(.~group) + theme(panel.grid.minor = element_line(colour = "grey"), panel.background = element_rect(fill = "white"),
    axis.line = element_line(colour="black"), axis.text = element_text(colour = "black")),
  hisDen = 
    ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19", binwidth=input$w, aes(y=..density..)) +
    geom_density(colour="blue") + facet_grid(.~group) + theme(panel.grid.minor = element_line(colour = "grey"),
   panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
   axis.text = element_text(colour = "black"))
)
plotType
})

qqdata <- reactive({
  n <- nrow(filteredData())
  probabilities <- (1:n)/(1+n)
  normal.quantiles <- qnorm(probabilities, mean(filteredData()$response, na.rm = T), 
                            sd(filteredData()$response, na.rm = T))
  qqdata0 <- data.frame(sort(normal.quantiles), sort(filteredData()$response))
  colnames(qqdata0) <- c("normal.quantiles", "diffs")
  data.frame(qqdata0)
})


observe({
  range <- diff(range(filteredData()$response))
  updateSliderInput(session, "w", min=round(range/100, digits=2), 
          max=round(range/5, digits=2), step=round(range/100, digits=2), value=round(range/10, digits=2))
})

output$summary <- renderTable({
  favstats(response~factor(group), data=filteredData())
})



})