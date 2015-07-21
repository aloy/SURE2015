library(shiny)
library(shinyjs)
library(Lock5Data)
library(car)
data(BodyFat)

#Model Selection

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
      data.frame(BodyFat)
  })
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  
  shinyjs::onclick("hideSelectOptions",
                   shinyjs::toggle(id = "selectOptions", anim = TRUE))
  
  observe({
    data <- theData()
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'responseVar', choices = qvars)
  })
  
  filteredData <-reactive({
    data<-isolate(theData())
    response0 <- input$responseVar
    if(input$responseVar=="responseVar"){
      if(is.null(data)){
        data<-data.frame(response=0)
      }
    }
    else{
      colnames(data)[colnames(data) == response0] <- "response"
    }
    data
  })
  
  factorData <- reactive({
    if(is.null(input$factCols)==FALSE){
      data <- isolate(filteredData())
      factCols0 <- c(input$factCols)
      data[,factCols0] <- as.factor(data[,factCols0])
      data
    }
    else{
      filteredData()
    }
  })
  
  full.lm <- reactive({
    lm(response~., data=factorData())
  })
  
  null.lm <- reactive({
    lm(response~1, data=factorData())
  })
  
  output$anovaNull <- renderTable({
    anova(null.lm(), full.lm())
  })
  
  output$coefCI <- renderTable({
  confint(full.lm(), level = input$level)
  })
  
  output$select <- renderUI({
    cols <- colnames(subset(factorData(), select=-response))
    checkboxGroupInput("cols", "Choose Variables", cols)
  })
  
output$factorSelect <- renderUI({  
  sub <- subset(filteredData(), select=-response)
    qvars <- names(sub[sapply(sub, is.numeric)])
    checkboxGroupInput("factCols", "Select Numeric Variables as Factors", qvars)
  })
  
  output$yint <- renderUI({
    if(is.null(input$cols)){
      return(NULL)
    }
    else{
      checkboxInput("yint", "Include Y-Intercept", value=TRUE)
    }
  })

  newData <-  reactive({
    vars <- input$cols
    if(is.null(input$cols)){
      newData0 <- data.frame(factorData()[,"response"])
      names(newData0) <- "response"
    }else{
newData0 <- data.frame(factorData()[,c(vars, "response")])
}
newData0
})
  
  new.lm <- reactive({
    if(is.null(input$cols)){
      lm(response~1, data=newData())
    }
    if(is.null(input$cols)==FALSE && input$yint==FALSE){
      lm(response~.-1, data=newData())
    }
    else{
      lm(response~., data=newData())
    }
  })

output$summary <- renderPrint({
summary(test.lm())
})

  test.lm <- reactive({
   test <- switch(input$mod,
           full=full.lm(),
           other=new.lm()
           ) 
   test
    })

testData <- reactive({
 data <- switch(input$mod,
         full=factorData(),
         other=newData()
  )
  data.frame(data)
})

output$warn <- renderUI({
  if(input$mod == "other" && is.null(input$cols)==TRUE){
    p(strong("Please select an alternative model on the Model Selection tab."),  style = "color:red")
  }
  else{
    return(NULL)
  }
})

output$corrPlot <- renderPlot({
  plot(testData()[sapply(testData(), is.numeric)], pch = 16)
})

output$corrTable <- renderTable({
  cor(testData()[sapply(testData(), is.numeric)])
})

output$vif <- renderPrint({
  vif(test.lm())
})

output$av <- renderPlot({
  avPlots(test.lm(), pch = 16)
})

output$hat <- renderTable({
  hatinf <- as.numeric(which(hatvalues(test.lm())> (2*ncol(testData()))/nrow(testData())))
  data.frame(testData()[hatinf,], Hat=hatvalues(test.lm())[hatinf])
})

output$cooks <- renderTable({
  cooks <- as.numeric(which(cooks.distance(test.lm())>1))
  data.frame(testData()[cooks,], Cooks=cooks.distance(test.lm())[cooks])
})

output$dffits <- renderTable({
  fits <- as.numeric(which(dffits(test.lm())>1))
  data.frame(testData()[fits,], DFFITS=dffits(test.lm())[fits])
})

output$dfbetas <- renderTable({
  betas <- as.numeric(which(dfbetas(test.lm())>1))
  data.frame(testData()[betas,], DFBETAS=dfbetas(test.lm())[betas])
  
})

output$bp <- renderPrint({
  ncvTest(test.lm())
})

output$qq <- renderPlot({
  qqPlot(test.lm(), dist = "norm", pch = 16)
})

})