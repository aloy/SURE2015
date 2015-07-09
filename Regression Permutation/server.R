library(stats)
library(mosaic)
library(Lock5Data)
library(shinyjs)
library(ggvis)
data("CaffeineTaps")

shinyServer(function(input, output, session) {
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
  
  
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "trials", anim = TRUE))
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  output$contents <- renderDataTable({theData() %>%head})
  observe({
    data <- theData()
    cvars <- colnames(data)[sapply(data,is.factor)]
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'group', choices = cvars)
    updateSelectInput(session, 'response', choices = qvars)
    
  })
  
  filteredData<-reactive({
    data<-isolate(theData())
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
  
  trials <- reactive({
    
    if(input$goButton > 0) {
      perms <-do(input$num) * summary(lm(formula = response ~ shuffle(group), data = filteredData()))$coefficients[2,1]
      colnames(perms) <- "perms"
      data.frame(perms)
    } else {
      data.frame(perms = rep(0, 10))
    }
  })
  output$trials <- renderDataTable(trials() %>% head)
  
  observe({
  trials %>%
    ggvis(~perms) %>%
    layer_histograms(width = input_slider(0.001, 0.5, step=0.001, value=0.2)) %>%
    bind_shiny("hist", "hist_ui")
  })
  
  output$summary <- renderTable({
    favstats(trials()$perms)
  })
  
})