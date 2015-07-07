library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
  
  ## One-Sample Bootstrap
  library(mosaic)
  library(Lock5Data)
  library(dplyr)
  library(ggvis)
  
  data(SleepStudy)
  
  filedata <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      return(      
        d <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      )
    }
    else
      d <- as.data.frame(SleepStudy)
  })
  
  output$contents <- renderTable({
    filedata()
  })
  
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "contents", anim = TRUE))
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  output$varChoose <- renderUI({
    df <- filedata()
    if (is.null(df)) 
      return(NULL)
    vars <- colnames(df)[sapply(df,is.numeric)]
    names(vars)=vars
    if(input$chooseData=="uploadNo")
      selectInput("choose",label="Choose a quantitative variable to examine:", vars, selected="AverageSleep")
    else
      selectInput("choose",label="Choose a quantitative variable to examine:",vars)
  })

simdata <- reactive({
  quantName <- input$choose
  simdata0 <- data.frame(filedata()[,quantName])
  colnames(simdata0)[colnames(simdata0)=="filedata.....quantName."] <- "Quantitative"
  simdata0
})

  qqdata <- reactive({
    n <- nrow(simdata())
    probabilities <- (1:n)/(1+n)
    normal.quantiles <- qnorm(probabilities, mean(simdata()$Quantitative, na.rm = T), sd(simdata()$Quantitative, na.rm = T))
    qqdata0 <- data.frame(sort(normal.quantiles), sort(simdata()$Quantitative))
    colnames(qqdata0) <- c("normal.quantiles", "diffs")
    data.frame(qqdata0)
  })
  
observe(
  ggSwitch <-  switch(input$plot, 
                       his= simdata %>% 
                        ggvis(~simdata()$Quantitative) %>%
                        add_axis("x", title = input$choose) %>%
                        layer_histograms(width = input_slider(0.1, 1.6, step=0.1, value=0.6)) %>% 
                        bind_shiny("origHist", "origHist_ui"),
                       den = simdata %>% 
                          ggvis(~simdata()$Quantitative) %>% 
                         layer_densities() %>%
                          add_axis("x", title = input$choose) %>%
                          add_axis("y", title="Density") %>%
                          bind_shiny("origHist", "origHist_ui"),
                      qq = qqdata %>% 
                        ggvis(~normal.quantiles, ~diffs) %>% 
                        layer_points() %>% 
                        add_axis("x", title="Theoretical") %>%
                        add_axis("y", title="Sample") %>%
                        bind_shiny("origHist", "origHist_ui")
  )
)

output$summary <- renderTable({
favstats(Quantitative, data=simdata())
  })
  
  output$sd <- renderText({
    sd(simdata()$Quantitative)
  })

output$hisDenPlot <- renderPlot ({
  qplot(Quantitative, data=simdata(), xlab=input$choose, 
  ylab = "Density", binwidth=input$w) + aes(y=..density..) + geom_density()
})

trials <- reactive({
 trials0 <- switch(input$stat,
    bootMean= do(input$num) * mean(sample(simdata()$Quantitative, replace=TRUE)),
    bootMedian= do(input$num) * median(sample(simdata()$Quantitative, replace = TRUE)),
    bootSd= do(input$num) * sd(sample(simdata()$Quantitative, replace = TRUE))
  )
as.data.frame(trials0)
})

# observe(
#   ggSwitch2 <-  switch(input$plot2, 
#                       his2= trials %>% 
#                         ggvis(~trials()$result) %>%
#                         add_axis("x", title = paste(input$choose, "Bootstrap")) %>%
#                         layer_histograms(width = input_slider(0.1, 1.6, step=0.1, value=0.6)) %>% 
#                         bind_shiny("bootHist", "bootHist_ui"),
#                       den2 = trials %>% 
#                         ggvis(~trials()$result) %>% 
#                         layer_densities() %>%
#                         add_axis("x", title = paste(input$choose, "Bootstrap")) %>%
#                         add_axis("y", title="Density") %>%
#                         bind_shiny("bootHist", "bootHist_ui")
# 
#   )
#   )

qqdata2 <- reactive({
  n <- input$num
  probabilities <- (1:n)/(1+n)
  normal.quantiles <- qnorm(probabilities, mean(trials()$result, na.rm = T), sd(trials()$result, na.rm = T))
  qqdata1 <- data.frame(sort(normal.quantiles), sort(trials()$result))
  colnames(qqdata1) <- c("normal.quantiles", "diffs")
  data.frame(qqdata1)
})

output$test <- renderPlot({
  qplot(x=normal.quantiles, y=diffs, data=qqdata2())
})

output$hisDenPlot2 <- renderPlot ({
  qplot(result, data=trials(), xlab=input$choose, 
        ylab = "Density", binwidth=input$w2) + aes(y=..density..) + geom_density()
})

output$bootSummary <- renderTable({
favstats(~result, data=trials())
})

  output$bootBias <- renderText({
    biasStat <- switch(input$stat,
           bootMean = mean(simdata()$Quantitative)-mean(trials()$result),
           bootMedian= median(simdata()$Quantitative)-median(trials()$result),
           bootSd=sd(simdata()$Quantitative)-sd(trials()$result)
           )
   biasStat
  })
  
  output$bootSd <- renderText({
    sd(trials()$result)
  })
  
  level <- reactive({
    input$level
  })
  
  alpha <- reactive({
    1-as.numeric(level())
  })
  
  SE <- reactive({
    sd(trials()$result)
  })
  
observed <- reactive({
  switch(input$stat,
         bootMean= mean(simdata()$Quantitative),
         bootMedian= median(simdata()$Quantitative),
         bootSd= sd(simdata()$Quantitative)
  )
})

  output$percPrint <- renderText({
    quantile(trials()$result, probs = c(alpha()/2, 1-alpha()/2))
  })
  
  output$percLower <- renderText({
    quantile(trials()$result, probs = c(alpha()))
  })
  
  output$percUpper <- renderText({
    quantile(trials()$result, probs = c(1-alpha()))
  })

output$normPrint <- renderText({
  c(observed() - qnorm(1-alpha()/2) *  SE(),
    observed() + qnorm(1-alpha()/2) * SE())
})
  output$normUpper <- renderText({
    c(paste(100*level(),'%'), observed() + qnorm(1-alpha()) * SE())
  })
  
  output$normLower <- renderText({
    c(paste(100*alpha(),'%'), observed()- qnorm(1-alpha()) * SE())
  })
 
})