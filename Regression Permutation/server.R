library(stats)
library(mosaic)
library(Lock5Data)
library(shinyjs)
library(ggvis)
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

filteredData %>%
  ggvis(x=~x, y=~y) %>% 
  layer_points() %>%
  layer_model_predictions(model = 'lm') %>%
  bind_shiny("origPlot")

output$origSummary <- renderPrint({
  summary(original.lm())
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
#   level <- reactive({
#   input$level
# })

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

original.lm <- reactive({
  data2 <- data.frame(theData()[,c(input$x,input$y)])
  names(data2) <- c("x","y")
  lm(y~x, data=data2)
})

observe({
updateNumericInput(session, "xval", label=paste("Value of", input$x))
})

data.boot <- reactive({
  R <- input$R
  Boot(original.lm(), R=R, method="case")
})

output$ciPrint <- renderPrint({
  ciPrint <- switch(input$stat,
  slope = confint(data.boot(), level=level(), type="perc")[2,]
  )
  ciPrint
})

output$percOneTail <- renderPrint({
  percOneTail <- switch(input$stat,
                    slope =confint(data.boot(), level=level()-alpha(),type="perc")[2,]
  )
  percOneTail
})

output$bootHist <- renderPlot({
  hist(data.boot(), col = "gray", level=level())
})

output$normPrint <- renderPrint({
  normPrint <- switch(input$stat,
                    slope = confint(data.boot(), level=level(), type="norm")[2,]
  )
  normPrint
})

output$normOneTail <- renderPrint({
  normOneTail <- switch(input$stat,
                        slope =confint(data.boot(), level=level()-alpha(),type="norm")[2,]
  )
  normOneTail
})

# 
# yhatDF <- reactive({
#   trials1 <- data.frame(trials())
#   yhat <- function(trials1, c1, c2, xval){trials1[c1]+(xval*trials1[c2])}
#   df <-apply(trials1, 1, yhat, c1="yint", c2="perms", xval=input$xval)
#   data.frame(df)
# })
# 
# 
# output$yhatCIPrint <- renderPrint({
#   round(quantile(yhatDF()$df, probs = c(alpha()/2, 1-alpha()/2)), digits=3)
# })
# 
# output$yhatPercLower <- renderPrint({
#   round(quantile(yhatDF()$df, probs = c(alpha())), digits=3) 
#   })
# 
# output$yhatPercUpper <- renderPrint({
#   round(quantile(yhatDF()$df, probs = c(1-alpha())), digits=3)
# })
# 
# output$yhatNormPrint <- renderText({
#   c(round(observed2() - qnorm(1-alpha()/2)*SE2(), digits=3),
#     round(observed2() + qnorm(1-(alpha()/2)) * SE2(), digits=3))
# })
# 
# output$yhatNormLower <- renderText({
#   c(paste(round(100*(1-level()), digits=2),'%'), 
#     round(observed2() - qnorm(1-alpha()) * SE2(),  digits=3))
# })
# 
# output$yhatNormUpper <- renderText({
#   c(paste(100*level(),'%'), round(observed2() + qnorm(1-alpha()) * SE2(), digits=3))
# })
# 
# S <- reactive({ #Residual standard error for prediction interval
#   original.lm <- lm(y~x, data=filteredData())
#   summary(original.lm)[[6]]
# })
# 
# 
# output$yhatPredLower <- renderText({
#   c(paste(round(100*(1-level()), digits=3),"%"), 
#     round(observed2() - qnorm(1-alpha()) *S() * 
#       sqrt(1+(1/nrow(yhatDF()))-(input$xval-mean(yhatDF()$df))^2/(nrow(yhatDF())*sd(yhatDF()$df)^2)), digits=3)
#     )
#   
# })
# 
# output$yhatPredUpper <- renderText({
#   c(paste(round(100*level(), digits=3),"%"), 
#     round(observed2() + qnorm(1-alpha())* S() * 
#     sqrt(1+(1/nrow(yhatDF()))+(input$xval-mean(yhatDF()$df))^2/(nrow(yhatDF())*sd(yhatDF()$df)^2)), digits=3)
#     )
#   
# })

})