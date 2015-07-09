library(ggvis)
library(Lock5Data)
data("CaffeineTaps")
library(mosaic)

shinyServer(function(input, output, session) {
  
  theData <- reactive({
    infile <- input$datfile        
    if(is.null(infile))
      return(NULL)        
    d <- read.csv(infile$datapath, header = T)
    d        
  })
  
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
    }
    names(data)<-c("group","response")
    data
  })
  
  # reactive df for permutations
  trials <- reactive({
    
    if(input$goButton > 0) {
      perms <- do(input$n) * diff(mean(response ~ shuffle(group), data = filteredData()))
      colnames(perms) <- "perms"
      data.frame(perms)
    } else {
      data.frame(perms = rep(0, 10))
    }
      
  })
  
  output$trials <- renderDataTable(trials() %>% head)
  
  input_width <- reactive(input$w)
  
#   A simple visualisation. In shiny apps, need to register observers
#   and tell shiny where to put the controls
#  output$plot <- renderPlot({
#    if(input$goButton > 0) {
        trials %>%
          ggvis(~perms) %>%
          layer_histograms(width = input_width) %>%
          bind_shiny("plot")
#    }
#  })
  
})