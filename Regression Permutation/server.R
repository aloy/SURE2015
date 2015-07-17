library(stats)
library(mosaic)
library(Lock5Data)
library(shinyjs)
library(ggvis)
library(car)
library(boot)
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
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  observe({
    data <- theData()
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'x', choices = qvars)
    updateSelectInput(session, 'y', choices = qvars)
  })
  
  filteredData <-reactive({
    data<-isolate(theData())
    if(input$x=="x" && input$y=="y"){
      if(is.null(data)){
        data<-data.frame(x = rep(0, 10), y = rep(0, 10))
      }
    }else{
      data <- data[,c(input$x,input$y)]
    }
    names(data)<-c("x","y")
    data.frame(data)
  })

filteredData %>%
  ggvis(x=~x, y=~y) %>% 
  layer_points() %>%
  layer_model_predictions(model = 'lm') %>%
  bind_shiny("origPlot")

output$origSummary <- renderPrint({
  summary(lm(y~x, data=filteredData()))
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
  output$trials <- renderDataTable(trials(), options = list(pageLength = 10))
  
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
        layer_densities(fill := "dodgerblue") %>%
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
  ggplot(data=trials(), aes(x=perms)) + geom_histogram(colour="black", fill="grey19", 
   binwidth=input$w, aes(y=..density..)) + geom_density(colour="royalblue", fill="royalblue", alpha=0.6)  + theme(panel.grid.minor = element_line(colour = "grey"), 
    panel.background = element_rect(fill = "white"), axis.line = element_line(colour="black"), axis.text = element_text(colour = "black"))
})

  output$summary <- renderTable({
    favstats(trials()$perms)
  })
  
output$pval <- renderText({
  observedSlope <- summary(lm(y~x, data=filteredData()))$coefficients[2]
  n <- input$num
  if(input$goButton > 0) {
    pvalSwitch <- switch(input$test, 
                         tt = min((sum(trials()$perms>=observedSlope)+1)/(n+1),
                                  (sum(trials()$perms<=observedSlope)+1)/(n+1))*2,
                         lt = (sum(trials()$perms <= observedSlope) +1)/(n+1),
                         ut = (sum(trials()$perms>=observedSlope)+1)/(n+1)
    )
    signif(pvalSwitch, 3)
  }
  else{
    return(0)
  }
})



alpha <- reactive({
  1 - level()
}) 
level <- reactive({
  input$level
})

observe({
updateNumericInput(session, "xval", label=paste("Value of", input$x))
})

observe({
  updateSliderInput(session, "x")
})

data.fn <- reactive({
  function(data1, index){
   slope.resample <- data1[index,]
    slope.lm <- lm(y~x, data=slope.resample) 
    slope.lm$coefficients[2]
  }
})

data.boot <- reactive({
  data1<-data.frame(isolate(filteredData()))
  boot(data1, R=input$R, statistic=data.fn())
})

yhat.fn <- reactive({
 function(data2, index){
  yhat.resample <- data2[index,]
  yhat.lm <- lm(y~x, data=yhat.resample)
  predict(yhat.lm, newdata=data.frame(x=input$xval))
  }
})

yhat.boot <- reactive({
data2<-data.frame(isolate(filteredData()))
boot(data2, R=input$R, statistic=yhat.fn())
})

pred <- reactive({
data2<-data.frame(isolate(filteredData()))
  pred0.fn <- function(data2, index){
    yhat.resample <- data2[index,]
    lm(y~x, data=yhat.resample
    )}
pred0.fn(data2)
})

percPrintFunction <- function(list, double){
  c(paste(round(100*(double),digits=2),'%'), round(list$percent[4], digits=3),
    paste(round(100*(1-(double)),digits=2),'%'), round(list$percent[5], digits=3))
}

normPrintFunction <- function(list, double){
  c(paste(round(100*(double),digits=2),'%'), round(list$normal[2], digits=3),
    paste(round(100*(1-(double)),digits=2),'%'), round(list$normal[3], digits=3))
}

output$ciPrint <- renderText({
  ciPrint <- switch(input$stat,
  slope = boot.ci(data.boot(), conf=level(), type="perc"),
  yhat =  boot.ci(yhat.boot(), conf=level(), type="perc")
  )
  percPrintFunction(ciPrint, alpha()/2)
})

output$percOneTail <- renderText({
  percOneTail <- switch(input$stat,
                    slope =boot.ci(data.boot(), conf=level()-alpha(),type="perc"),
                      yhat =  boot.ci(yhat.boot(), conf=level()-alpha(), type="perc")
  )
  percPrintFunction(percOneTail, alpha())
})

output$normPrint <- renderText({
  normPrint <- switch(input$stat,
                    slope = boot.ci(data.boot(), conf=level(), type="norm"),
                    yhat =  boot.ci(yhat.boot(), conf=level(), type="norm")
  )
  normPrintFunction(normPrint, alpha()/2)
})

output$normOneTail <- renderText({
  normOneTail <- switch(input$stat,
                        slope =boot.ci(data.boot(), conf=level()-alpha(),type="norm"),
                        yhat =  boot.ci(yhat.boot(), conf=level()-alpha(), type="norm")
  )
  normPrintFunction(normOneTail, alpha())
})

output$predInt <- renderPrint({
predict(pred(), newdata=data.frame(x=input$xval), interval="predict", level=level())
})

output$predOneTail <- renderPrint({
  predict(pred(), newdata=data.frame(x=input$xval), interval="predict", level=level()-alpha())
})

})