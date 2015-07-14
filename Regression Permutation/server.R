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
  
#   observed <- reactive({
#     summary(lm(formula = y ~ x, data = filteredData()))$coefficients[2,1]
#   })

alpha <- reactive({
  1 - level()
})
# 
# SE <- reactive (
#   sd(trials()$perms)
# )

# 
# SE2 <- reactive ({
#   sd(yhatDF())
# })
# observed2 <- reactive({
#   original.lm <- lm(y~x, data=filteredData())
#   predict(original.lm, data.frame(x = input$xval))
# })
level <- reactive(
  input$level
)

observe({
updateNumericInput(session, "xval", label=paste("Value of", input$x))
})

data.boot <- reactive({
  original.lm <- lm(y~x, data=filteredData())
  Boot(original.lm, R=input$R, method="case")
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

output$ciPrint <- renderPrint({
  ciPrint <- switch(input$stat,
  slope = confint(data.boot(), level=level(), type="perc")[2,],
  yhat =  confint(yhat.boot(), level=level(), type="perc")[1,]
  )
  ciPrint
})

output$percOneTail <- renderPrint({
  percOneTail <- switch(input$stat,
                    slope =confint(data.boot(), level=level()-alpha(),type="perc")[2,],
                      yhat =  confint(yhat.boot(), level=level()-alpha(), type="perc")[1,]
  )
  percOneTail
})

output$bootHist <- renderPlot({
  hist(data.boot(), col = "gray", level=level())
})


output$percBootHist <- renderPlot({
  hist(data.boot(), col = "gray", level=level(), ci="percentile")
})

output$normPrint <- renderPrint({
  normPrint <- switch(input$stat,
                    slope = confint(data.boot(), level=level(), type="norm")[2,],
                    yhat =  confint(yhat.boot(), level=level(), type="norm")[1,]
  )
  normPrint
})

output$normOneTail <- renderPrint({
  normOneTail <- switch(input$stat,
                        slope =confint(data.boot(), level=level()-alpha(),type="norm")[2,],
                        yhat =  confint(yhat.boot(), level=level()-alpha(), type="norm")[1,]
  )
  normOneTail
})

})