library(shiny)
library(shinyjs)
library(Lock5Data)
library(car)
library(leaps)
data(BodyFat)

#Model Selection

shinyServer(function(input,output, session){
  
  theData <- reactive({
    if(input$chooseData=="uploadYes"){
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      return(      
        read.csv(inFile1$datapath, header=input$header, sep=input$sep, quote=input$quote, 
                 row.names=input$rownames)
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
    if(input$firstCol==TRUE && is.factor(theData()[,1])==TRUE){
      data <- data[,-1]
    }
    data
  })
  
  output$rowNames <- renderUI({
    if(is.factor(theData()[,1])==TRUE){
      checkboxInput("firstCol", "Remove first column?", value=FALSE)
    }
    else{
      return(NULL)
    }
  })
  
  factorData <- reactive({
    if(is.null(input$factCols)==FALSE){
      data <- isolate(filteredData())
      factCols0 <- c(input$factCols)
      facts <- data.frame(apply(filteredData()[which(colnames(filteredData())== factCols0)], 2, 
                                function(x) as.factor(x)))
      nums <- subset(filteredData(), select=which(colnames(filteredData()) != factCols0))
      data.frame(nums, facts)
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
  
  output$selectVar <- renderUI({
    cols <- colnames(subset(factorData(), select=-response))
    checkboxGroupInput("cols", "Choose Variables", cols)
  })
  
  output$factorSelect <- renderUI({  
    sub <- subset(filteredData(), select=-response)
    qvars <- names(sub[sapply(sub, is.numeric)])
    checkboxGroupInput("factCols", "Select Numeric Variables as Categorical", qvars)
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
  
  output$summary <- renderTable({
    summary(test.lm())
  })
  
  test.lm <- reactive({
    test <- switch(input$mod,
                   full=full.lm(),
                   other=new.lm(),
                   selected= select.lm()
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
  
  
  select.lm <- reactive({
    lm <- switch(input$select,
     back= step(full.lm(), scope = list(lower = ~ 1), direction = "backward"),
    bic.back=step(full.lm(), scope = list(lower = ~ 1), direction = "backward",  
                 k = log(nrow(factorData()))),
    all= regsubsets(response ~ ., data = factorData(),
                       method = "exhaustive", nvmax = ncol(factorData()), nbest = 1)
    )
    lm
  })
#   back= step(full.lm(), scope = list(lower = ~ 1), direction = "backward"),
#   bic.back = step(full.lm(), scope = list(lower = ~ 1), direction = "backward",  
#                   k = log(nrow(factorData()))),
#   all = regsubsets(response ~ ., data = factorData(),
#                    method = "exhaustive", nvmax = ncol(factorData()), nbest = 1)
#   
  output$checkPlot <- renderPlot({
    
    switch(input$plot,
           adjr2=  plot(select.lm(), scale="adjr2"),
           cp= plot(select.lm(), scale="Cp"),
           bic.plot=plot(select.lm(), scale="bic")
    )
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
  if(ncol(testData())<10){
    avPlots(test.lm(), pch = 16)
  }
  else{
    return(NULL)
  }
  })
  
output$residPlot <- renderPlot({
  switch(input$resid,
         student= residualPlot(test.lm(), pch = 16, type = "rstudent"),
          std= residualPlot(test.lm(), pch = 16, type = "rstandard"),
          raw= residualPlot(test.lm(), pch=16)
         )
})

  output$hat <- renderTable({
    n <- input$inflPercent
    hatinf <- as.numeric(which(hatvalues(test.lm())>(2*(ncol(testData()+1)))/nrow(testData())))
    hatData <- data.frame(testData()[hatinf,], Hat=hatvalues(test.lm())[hatinf])
    if(is.data.frame(hatData)==TRUE && nrow(hatData)==0){
      allHat <- data.frame(hatvalues(test.lm()))
      ordern <- order(allHat, decreasing = T)[1:(nrow(allHat)*n)]
      data.frame(testData()[ordern,], "Largest Cooks"=allCook[ordern,])
    }
    else{
      data.frame(hatData)
    }
  })
  
  output$cooks <- renderTable({
    n <- input$inflPercent
    cooks <- as.numeric(which(cooks.distance(test.lm())>1))
    cooksData <- data.frame(testData()[cooks,], Cooks=cooks.distance(test.lm())[cooks])
    if(is.data.frame(cooksData)==TRUE && nrow(cooksData)==0){
      allCook <- data.frame(cooks.distance(test.lm()))
      ordern <- order(allCook, decreasing = T)[1:(nrow(allCook)*n)]
      data.frame(testData()[ordern,], "Largest Cooks"=allCook[ordern,])
    }
    else{
      data.frame(cooksData)
    }
  })
  
  output$dffits <- renderTable({
    n <- input$inflPercent
    fits <- as.numeric(which(dffits(test.lm())>1))
    fitsData <- data.frame(testData()[fits,], DFFITS=dffits(test.lm())[fits])
    if(is.data.frame(fitsData)==TRUE && nrow(fitsData)==0){
      allFits <- data.frame(dffits(test.lm()))
      ordern <- order(allFits, decreasing = T)[1:(nrow(allFits)*n)]
      data.frame(testData()[ordern,], "Largest DFFITS"=allFits[ordern,])
    }
    else{
      data.frame(fitsData)
    }
  })
  
  output$dfbetas <- renderTable({
    n <- input$inflPercent
    betas <- which(dfbetas(test.lm()) >1, arr.ind=TRUE)
    betasData <- data.frame(testData()[betas[,1],], DFBETAS=dfbetas(test.lm())[betas]) 
#     if(is.data.frame(betasData)==TRUE && nrow(betasData)==0){
#       allBetas <- data.frame(dfbetas(test.lm()))
#       ordern <- order(allBetas, decreasing = T)[1:(nrow(allBetas)*n)]
#       data.frame(allBetas[ordern,])
#       }else{
        data.frame(betasData)
#       }
  })

output$bp <- renderPrint({
  ncvTest(test.lm())
})

output$qq <- renderPlot({
  qqPlot(test.lm(), dist = "norm", pch = 16)
})

index <- reactive({
sample(1:nrow(testData()), size = 0.2 * nrow(testData()))  
})

train.data <- reactive({
  testData()[-index(),]
})

test.data <- reactive({
  testData()[index(),]
})

})
