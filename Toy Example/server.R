library("ggvis")
library("Lock5Data")
data("CaffeineTaps")
library(mosaic)

shinyServer(function(input, output, session) {
  # A reactive subset of mtcars
  
  
  theData <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      return(      
        d <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else(
      d <- as.data.frame(CaffeineTaps)
    )
    d
  })
  
  observe({
    data <- theData()
    cvars <- colnames(data)[sapply(data,is.factor)]
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'group', choices = cvars)
    updateSelectInput(session, 'response', choices = qvars)
    
  })
  
  filedata<-reactive({
    data<-isolate(theData())
    #if there is no input, make a dummy dataframe
    if(input$chooseData=="uploadNo"){
      if(is.null(data)){
        data<-data.frame(group=0, response=0)
      }
      data <- CaffeineTaps[,c(input$group,input$response)]
      names(data)<-c("group","response")
    }else{
      data <- data[,c(input$group,input$response)]
      names(data)<-c("group","response")
    }
    data
  })

trials <- reactive({
  if(input$goButton > 0) {
    if(input$goButton2 < input$goButton){
    perms <- do(input$n) * diff(mean(response ~ shuffle(group), data = filedata()))
    colnames(perms) <- "perms"
    data.frame(perms)
    }
    else{
      data.frame(perms=rep(0, input$n))
    }
  } 
  else {
    data.frame(perms = rep(0, 10))
  }
})

output$trials <- renderDataTable(trials() %>% head)
# observe({
# if (input$type == "his"){ 
# trials %>%
#   ggvis(~perms) %>%
#   layer_histograms(width = input_slider(0.1, 1.6, step=0.1, value=0.6)) %>%
#   bind_shiny("plot", "plot_ui")
# }
# if (input$type == "den"){ 
#   trials %>% 
#     ggvis(~perms) %>% 
#     layer_densities() %>%
#     add_axis("y", title="Density") %>%
#     bind_shiny("plot", "plot_ui")
# }
# })
})