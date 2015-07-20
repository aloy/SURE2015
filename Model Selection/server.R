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
  
  observe({
    data <- theData()
    qvars <- colnames(data)[sapply(data,is.numeric)]
    updateSelectInput(session, 'response', choices = qvars)
  })
  
  filteredData <-reactive({
    data<-isolate(theData())
    response0 <- input$response
    if(input$response=="response"){
      if(is.null(data)){
        data<-data.frame(response=0)
      }
    }else{
      colnames(data)[colnames(data) == response0] <- "response"
    }
    data.frame(data)
  })
  
  full.lm <- reactive({
    lm(response~., data=filteredData())
  })
  
  null.lm <- reactive({
    lm(response~1, data=filteredData())
  })
  
  output$fullModel <- renderPrint({
    summary(full.lm())
  })
  
  output$anovaNull <- renderTable({
    anova(null.lm(), full.lm())
  })
  
  output$coefCI <- renderTable({
  confint(full.lm(), level = input$level)
  })
  
  output$select <- renderUI({
    cols <- colnames(subset(filteredData(), select=-response))
    checkboxGroupInput("cols", "Choose Variables", cols)
  })
  
  newData <-  reactive({
    vars <- input$cols
    if(is.null(input$cols)){
      newData0 <- data.frame(filteredData())
    }else{
newdata0 <- data.frame(filteredData()[,c(vars, "response")])
}
switch(input$mod,
       full=filteredData(),
       other=newdata0
) 
})
  
  new.lm <- reactive({
    if(is.null(input$cols)){
      lm(response~1, data=filteredData())
    }
    else{
      lm(response~., data=newData())
    }
  })
  
output$summary <- renderPrint({
  summary(new.lm())
})  

  test.lm <- reactive({
    switch(input$mod,
           full=full.lm(),
           other=new.lm()
           ) 
    })

output$corrPlot <- renderPlot({
  plot(newData(), pch = 16)
})

output$corrTable <- renderTable({
  cor(newData())
})

output$vif <- renderPrint({
  vif(test.lm())
})

output$av <- renderPlot({
  avPlots(test.lm(), pch = 16)
})

output$hat <- renderTable({
  hatinf <- as.numeric(which(hatvalues(test.lm())> (2*ncol(newData()))/nrow(newData())))
  data.frame(newData()[hatinf,], Hat=hatvalues(test.lm())[hatinf])
})

output$cooks <- renderTable({
  cooks <- as.numeric(which(cooks.distance(test.lm())>1))
  data.frame(newData()[cooks,], Cooks=cooks.distance(test.lm())[cooks])
})

output$dffits <- renderTable({
  fits <- as.numeric(which(dffits(test.lm())>1))
  data.frame(newData()[fits,], DFFITS=dffits(test.lm())[fits])
})

output$dfbetas <- renderTable({
  betas <- as.numeric(which(dfbetas(test.lm())>1))
  data.frame(newData()[betas,], DFBETAS=dfbetas(test.lm())[betas])
  
})

output$bp <- renderPrint({
  ncvTest(test.lm())
})

output$qq <- renderPlot({
  qqPlot(test.lm(), dist = "norm", pch = 16)
})

})