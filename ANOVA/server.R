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
   filteredData() %>%
    ggvis(x=~group, y=~response) %>%
    layer_boxplots() %>%
    bind_shiny("origBox")
})

output$origPlot <- renderPlot({
plotType <- switch(input$plot,
  his =                 
    ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19") +
    facet_grid(.~group) + theme(panel.grid.minor = element_line(colour = "grey"), panel.background = element_rect(fill = "white"),
    axis.line = element_line(colour="black"), axis.text = element_text(colour = "black")),
  hisDen = 
    ggplot(filteredData(), aes(response)) + geom_histogram(colour="black", fill="grey19", aes(y=..density..)) +
    geom_density(colour="blue") + facet_grid(.~group) + theme(panel.grid.minor = element_line(colour = "grey"),
   panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), 
   axis.text = element_text(colour = "black"))
)
plotType
  
})

output$summary <- renderTable({
  favstats(response~factor(group), data=filteredData())
})



})