library(stats)
library(mosaic)
library(Lock5Data)
library(shinyjs)
library(ggvis)
library(car)
library(boot)
data(mtcars)

# Permutation for Regression

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

output$origCor <- renderText({
  cor(x, y, data=filteredData())
})

  trials <- reactive({    
    if(input$goButton > 0) {
      if(input$permStat=="slope"){
      perms <-do(input$num) * summary(lm(formula = y ~ shuffle(x), data = filteredData()))$coefficients[,1]
      colnames(perms) <- c("yint", "perms")
      df <- data.frame(perms)
      }
      if(input$permStat=="cor"){
        perms <- do(input$num) * cor(filteredData()$x, sample(filteredData()$y))
        colnames(perms) <- "perms"
        df <- data.frame(perms)
      }
    } else {
      df <- data.frame(perms = rep(0, 10))
    }
    df
  })
  output$trials <- renderDataTable(trials(), options = list(pageLength = 10))
  
  observe({
#     if(input$reset > 0 ){
#       trials <- data.frame(perms=rep(0, 10))
#       output$trials <- renderDataTable(data.frame(perms = rep(0, 10)))
#     }
    if(input$goButton > 0){
      range.100 <- round(diff(range(trials()$perms))/100, digits=3)
      if (round(diff(range(trials()$perms))/100, digits=3) == 0)
        range.100 <- 0.001
    }
    else{
      range.100 <- 0.001
    }
    if(input$plot=="his"){
      trials %>%
        ggvis(~perms) %>%
        layer_histograms(width = input_slider(range.100, range.100*50,
                                              value=range.100*10, step=0.001)) %>%
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

output$slider <- renderUI({
  if (round(diff(range(trials()$perms))/100, digits=3) == 0){
    range.100 <- 0.01
  }
  else{
    range.100 <- round(diff(range(trials()$perms))/100, digits=3)
  }
  sliderInput("w", "", min=range.100, max=range.100*50, value=range.100*10, step=.001)
})

  output$summary <- renderTable({
    favstats(trials()$perms)
  })
  
output$pval <- renderText({
  observed <- switch(input$permStat,
                     slope= summary(lm(y~x, data=filteredData()))$coefficients[2],
                     cor = cor(filteredData()$x, filteredData()$y)
  )
  n <- input$num
  if(input$goButton > 0) {
    pvalSwitch <- switch(input$test, 
                         tt = min((sum(trials()$perms>=observed)+1)/(n+1),
                                  (sum(trials()$perms<=observed)+1)/(n+1))*2,
                         lt = (sum(trials()$perms <= observed) +1)/(n+1),
                         ut = (sum(trials()$perms>=observed)+1)/(n+1)
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

# observe({
#   updateSliderInput(session, "x")
# })

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

cor.fn <- reactive({
  function(data3, index){
    data3 <- data3[index,]
    return(cor(data3$x, sample(data3$y)))
  }
})

cor.boot <- reactive({
  data3<-data.frame(isolate(filteredData()))
  boot(data3, R=input$R, statistic=cor.fn())
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
  yhat =  boot.ci(yhat.boot(), conf=level(), type="perc"),
  cor= boot.ci(cor.boot(), conf=level(), type="perc")
  )
  percPrintFunction(ciPrint, alpha()/2)
})

output$percOneTailLower <- renderText({
  percOneTail <- switch(input$stat,
                    slope =boot.ci(data.boot(), conf=level()-alpha(),type="perc"),
                      yhat =  boot.ci(yhat.boot(), conf=level()-alpha(), type="perc"),
                    cor= boot.ci(cor.boot(), conf=level()-alpha(), type="perc")
  )
  percPrintFunction(percOneTail, alpha())[1:2]
})

output$percOneTailUpper <- renderText({
  percOneTail <- switch(input$stat,
                        slope =boot.ci(data.boot(), conf=level()-alpha(),type="perc"),
                        yhat =  boot.ci(yhat.boot(), conf=level()-alpha(), type="perc"),
                        cor=boot.ci(cor.boot(), conf=level()-alpha(), type="perc")
  )
  percPrintFunction(percOneTail, alpha())[3:4]
})

output$normPrint <- renderText({
  normPrint <- switch(input$stat,
                    slope = boot.ci(data.boot(), conf=level(), type="norm"),
                    yhat =  boot.ci(yhat.boot(), conf=level(), type="norm"),
                    cor=boot.ci(cor.boot(), conf=level(), type="norm")
  )
  normPrintFunction(normPrint, alpha()/2)
})

output$normOneTailLower <- renderText({
  normOneTail <- switch(input$stat,
                        slope =boot.ci(data.boot(), conf=level()-alpha(),type="norm"),
                        yhat =  boot.ci(yhat.boot(), conf=level()-alpha(), type="norm"),
                        cor=boot.ci(cor.boot(), conf=level()-alpha(), type="norm")
  )
  normPrintFunction(normOneTail, alpha())[1:2]
})

output$normOneTailUpper <- renderText({
  normOneTail <- switch(input$stat,
                        slope =boot.ci(data.boot(), conf=level()-alpha(),type="norm"),
                        yhat =  boot.ci(yhat.boot(), conf=level()-alpha(), type="norm"),
                        cor= boot.ci(cor.boot(), conf=level()-alpha(), type="norm")
  )
  normPrintFunction(normOneTail, alpha())[3:4]
})

output$predInt <- renderPrint({
predict(pred(), newdata=data.frame(x=input$xval), interval="predict", level=level())
})

output$predOneTailLower <- renderPrint({
  paste(100*(1-level()),"%",
    round(predict(pred(), newdata=data.frame(x=input$xval), interval="predict", 
                  level=level()-alpha())[2], digits=3))
})

output$predOneTailUpper <- renderPrint({
  paste(100*level(),"%",
        round(predict(pred(), newdata=data.frame(x=input$xval), interval="predict", 
                      level=level()-alpha())[3], digits=3))
})

})