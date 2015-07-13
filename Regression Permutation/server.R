library(stats)
library(mosaic)
library(Lock5Data)
library(shinyjs)
library(ggvis)
data(InkjetPrinters)
data(mtcars)

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
    data.frame(mtcars)
  })
  
  shiny::observe({
    toggle(id = "warning", condition = input$x==input$y)
  })
  shiny::observe({
    toggle(id = "warning2", condition = input$x==input$y)
  })
  
  shinyjs::onclick("hideData",
                   shinyjs::toggle(id = "trials", anim = TRUE))
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  output$contents <- renderDataTable(theData())
  observe({
    data <- theData()
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'x', choices = qvars)
    updateSelectInput(session, 'y', choices = qvars)
  })
  
  filteredData<-reactive({
    data<-isolate(theData())
    if(input$x=="x" && input$y=="y"){
      if(is.null(data)){
        data<-data.frame(x = rep(0, 10), y = rep(0, 10))
      }
    }else{
      data <- data[,c(input$x,input$y)]
    }
    names(data)<-c("x","y")
    data
  })
  

# lineData <- reactive({
#   slope <- summary(lm(formula = y ~ x, data = filteredData()))$coefficients[2,1]
#   yint <- summary(lm(formula = y ~ x, data = filteredData()))$coefficients[1,1]
#   max <- max(unclass(filteredData()$x))
#   ycor <- yint + slope*max
#   data.frame(x=c(0, max), y=c(yint, ycor))
# })
#   #This should also work 
# output$test<- renderTable({
#   lineData()
# })

filteredData %>%
  ggvis(x=~x, y=~y) %>% 
  layer_points() %>%
  layer_model_predictions(model = 'lm') %>%
  bind_shiny("origPlot")


# output$origPlot <- renderPlot({
#   qplot(x, y, data=filteredData())
# })

output$origSummary <- renderPrint({
  summary(lm(formula = y ~ x, data = filteredData()))
})

  trials <- reactive({
    
    if(input$goButton > 0) {
      perms <-do(input$num) * summary(lm(formula = y ~ shuffle(x), data = filteredData()))$coefficients[,1]
      colnames(perms) <- c("yint", "perms")
      data.frame(perms)
    } else {
      data.frame(perms = rep(0, 10))
    }
  })
  output$trials <- renderDataTable(trials() %>% head)
  
  observe({
    if(input$reset > 0 ){
      trials <- data.frame(perms=rep(0, 10))
      output$trials <- renderDataTable(data.frame(perms = rep(0, 10)))
    }
    if(input$plot=="his"){
      trials %>%
        ggvis(~perms) %>%
        layer_histograms(width = input_slider(0.01, 7.5, step=0.01, value=2.5)) %>%
        bind_shiny("hist", "hist_ui")
    }
    if(input$plot=="den"){
      trials %>%
        ggvis(~perms) %>%
        layer_densities() %>%
        add_axis("y", title="Density") %>%
        bind_shiny("hist", "hist_ui")
    }
    if(input$plot=="qq"){
      qqdata %>% 
        ggvis(~normal.quantiles, ~diffs) %>% 
        layer_points() %>% 
        add_axis("x", title="Theoretical") %>%
        add_axis("y", title="Sample") %>%
        bind_shiny("hist", "hist_ui")
    }
  })
  
  qqdata <- reactive({
    n <- input$num
    probabilities <- (1:n)/(1+n)
    normal.quantiles <- qnorm(probabilities, mean(trials()$perms, na.rm = T), sd(trials()$perms, na.rm = T))
    qqdata0 <- data.frame(sort(normal.quantiles), sort(trials()$perms))
    colnames(qqdata0) <- c("normal.quantiles", "diffs")
    data.frame(qqdata0)
  })

output$hisDen <- renderPlot({
qplot(data=trials(), x=perms, binwidth=input$w) + aes(y=..density..)+geom_density()
})

  output$summary <- renderTable({
    favstats(trials()$perms)
  })
  
  observed <- reactive({
    obs <-summary(lm(formula = y ~ x, data = filteredData()))$coefficients[2,1]
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
  round(quantile(trials()$perms, 
                 probs=c(alpha()/2, 1-alpha()/2)), digits=3)
})

output$percLower <- renderPrint({
  round(quantile(trials()$perms, probs = c(alpha())), digits=3)
})

output$percUpper <- renderPrint({
  round(quantile(trials()$perms, probs = c(1-alpha())), digits=3)
})

output$normPrint <- renderText({
  c(round(mean(trials()$perms) - qnorm(1-alpha()/2)*SE(), digits=3),
    round(observed() + qnorm(1-(alpha()/2)) * SE(), digits=3))
})

output$normLower <- renderText({
  c(paste(100*alpha(),'%'), 
    round(observed() - qnorm(1-alpha()) * SE(),  digits=3))
})

output$normUpper <- renderText({
  c(paste(100*level(),'%'), round(observed() + qnorm(1-alpha()) * SE(), digits=3))
  
})


})