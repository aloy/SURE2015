library(Lock5Data)
library(shinyjs)
library(ggplot2)
library(stats)
library(car)
library(productplots)
library(gridExtra)
library(nullabor)
library(dplyr)
data(RestaurantTips)

# Visual Inference

shinyServer(function(input, output, session) {
  source("qqplots.r")
  
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
      data.frame(RestaurantTips)
  })
  
  shinyjs::onclick("hideDataOptions",
                   shinyjs::toggle(id = "dataOptions", anim = TRUE))
  shinyjs::onclick("trueData",
                   shinyjs::toggle(id = "plotPos", anim = TRUE))
  
  shiny::observe({
    toggle(id = "warning", condition = input$x==input$y && input$plot!="qq" 
           &&input$plot!="box"&&input$plot!="den"&&input$plot!="mosaic")
    })
  
  shiny::observe({
    toggle(id = "warning2", condition = input$x2==input$y2 && input$plot!="scatter" 
           &&input$plot!="scatterSmooth"&&input$plot!="resid"&&input$plot!="residSmooth")
  })
  
  shiny::observe({
    toggle(id = "warning3", condition = input$x2==input$y3 && input$plot=="mosaic")
  })
  
  output$contents <- renderDataTable(theData(), options = list(pageLength = 10))

  observe({
  qvars <- colnames(theData())[sapply(theData(),is.numeric)]
  testFactor <- function(x){
    f <- colnames(x)[sapply(x,is.factor)]
    i <- colnames(x)[sapply(x, is.integer)]
    c <- colnames(x)[sapply(x, is.character)]
    return(c(f, i, c))
  }
  updateSelectInput(session,"x", choices=qvars)
  updateSelectInput(session,"y", choices=qvars)
  updateSelectInput(session,"x2", choices=testFactor(theData()))
  updateSelectInput(session,"y2", choices=qvars)
  updateSelectInput(session,"y3", choices=testFactor(theData()))
  
  if(input$plot!="scatter"){
    updateSelectInput(session, "x", selected=isolate(input$x))
    updateSelectInput(session, "y",selected=isolate(input$y))
  }
  })

  filteredData <-reactive({
    data<-isolate(theData())
    if(input$x=="x"&&input$y=="y"&&input$x2=="x2"&&input$y2=="y2"&&input$y3=="y3"){
      if(is.null(data)){
        data<-data.frame(x = rep(0, 10), y = rep(0, 10))
      }
    }
    else{
      data <- switch(input$plot,
                     scatter=data[,c(input$x,input$y)],
                     scatterSmooth=data[,c(input$x,input$y)],
                     box=data[,c(input$x2,input$y2)],
                     den=data[,c(input$x2,input$y2)],
                     qq=data[,c(input$x,input$y)],
                     resid=data[,c(input$x,input$y)],
                     residSmooth=data[,c(input$x,input$y)],
                     mosaic=data[,c(input$x2,input$y3)],
                     )
    }
    names(data)<-c("x","y")
    data.frame(data)
  })

  qqPos <- reactive({
    autoInvalidate()
    n <- input$num
    sample(n,1)
  })
  autoInvalidate <- reactive({
    input$go
    })
  
  qqPlot <- reactive({
    n <- input$num
    r <- qqPos()
    w <- nrow(filteredData())
    sim <- sim_lineup(n=w, nplots=input$num)
    x <- scale(filteredData()$x)
    true <- data.frame(qq_plot_info(x=x), .sample=rep(r, w))
    startrow <- function(x){w*(x-1)} #replace one of the samples with the actual data
    nextrow <- function(x){(w*(x))+1}
    endrow <- function(x){w*n}
    if(r!=1){
    new.df <- data.frame(rbind(sim[1:startrow(r),], true, sim[nextrow(r):endrow(n),]))
    }else{
      new.df <- data.frame(rbind(true, sim[nextrow(r):endrow(n),]))
    }
    new.df <- new.df[complete.cases(new.df),]
    autoInvalidate()
    if(input$plot=="qq"&&input$qqAdj=="std"){
     plot <- switch(input$qqBand,
            none=ctrl_lineup(new.df),
            dh=std_lineup(new.df),
            ts=std_ts_lineup(new.df)
            )
    }
    if(input$plot=="qq"&&input$qqAdj=="adj"){
      plot <- switch(input$qqBand,
             none=rot2_none(new.df),
             dh=rot2_lineup(new.df),
             ts=rot2_ts_lineup(new.df)
             )
    }
    if(input$plot=="qq"&&input$qqAdj=="ord"){
      plot <- switch(input$qqBand,
             none=rot_none(new.df),
             dh=rot_lineup(new.df),
             ts=rot_ts_lineup(new.df)
        )
    }
    plot
  })
  
  mosaicPlot <- reactive({
    n <- input$num
    w <- nrow(filteredData())
    mosaicLineup <- function(x){data.frame(x=sample(filteredData()$x, size=w, replace=FALSE), y=filteredData()$y)}
    samples <- plyr::rdply(n,mosaicLineup(filteredData()))
    r <- qqPos()
    origMosaic <- data.frame(.n=rep(as.numeric(r), w),x=filteredData()$x, y=filteredData()$y)
    startrow <- function(x){w*(x-1)} 
    nextrow <- function(x){(w*(x))+1}
    endrow <- function(x){(w*n)}
    if(r!=1){
      new.df <- data.frame(rbind(samples[1:startrow(r),], origMosaic, samples[nextrow(r):endrow(n),]))
      
    }else{
      new.df <- data.frame(rbind(origMosaic, samples[nextrow(r):endrow(n),]))
    }
    new.df <- new.df[complete.cases(new.df),]

    p <- list()
    for(i in 1:n){
      df1 <- 1+((i-1)*w)
      df2 <-w*i
      p[[i]] <- prodplot(new.df[df1:df2,], ~y+x) + aes(fill=x) + ggtitle(paste(i))+
      scale_fill_brewer("", palette="Set2")  + theme(axis.text.x=element_blank(), 
       axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
       axis.title.y=element_blank(),legend.position="none", plot.margin=unit(c(0, 0, 0, 0), "cm"))
    }
    autoInvalidate()
      plist <- p
      n <- length(plist)
      nCol <- floor(sqrt(n))
      do.call("grid.arrange", c(plist, ncol=nCol))
  })
  
  autoInvalidate2 <- reactive({
    input$go2
  })
  
  nullSwitch <- reactive({
    autoInvalidate2()
    nullDF <- data.frame(x=rnorm(1000, mean = 0, sd = 1), y=rnorm(1000, mean = 0, sd = 1))
    mosaicDF <- data.frame(x=c(1, 2), y=sample(1:10, 1000, replace=TRUE))
    names(nullDF) <- c("x","y")
    model2 <- lm(y~x, data=nullDF)
    resid.df <- data.frame(x=nullDF$x, y=nullDF$y,.resid=residuals(model2), .fitted=fitted(model2))
    switch(input$plot,
           scatter= ggplot(nullDF, aes(x=x, y=y))+geom_point()+theme(axis.text.x=element_blank(), 
               axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
               axis.title.y=element_blank()),
           scatterSmooth = ggplot(nullDF, aes(x, y)) +  geom_point() +
             geom_smooth(method="lm", se=FALSE) + theme(axis.text.x=element_blank(), 
              axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
              axis.title.y=element_blank()),
           box=ggplot(nullDF, aes(x=x, y=y, fill="green")) + geom_boxplot(aes(alpha=0.6)) + scale_fill_brewer("", palette="Set2") 
             + theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
                axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
              legend.position="none"),
           den=ggplot(nullDF, aes(x=x))+ geom_density(aes(alpha=0.6, fill="green")) 
              + scale_fill_brewer("", palette="Set2")+theme(axis.text.x=element_blank(), 
                 axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), 
                axis.title.y=element_blank(), legend.position="none"),
           qq = qplot(sample=nullDF$x, stat="qq") + geom_abline(slope=1, intercept=0)
           + theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
                   legend.position="none"),
           resid=ggplot(resid.df, aes(x=x, y=.resid)) +geom_point()
              + theme(axis.text.x=element_blank(), 
              axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
                 axis.title.y=element_blank()),
           residSmooth = ggplot(resid.df, aes(x=x, y=.resid)) %+%
             lineup(null_lm(y~x, method='boot'), n=2, pos=2, resid.df) +geom_point() +  
             geom_smooth(method="lm", se=FALSE) + theme(axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
              axis.title.y=element_blank()),
           mosaic= prodplot(mosaicDF, ~x+y) + aes(fill=x) + 
            scale_fill_brewer("", palette="Set2")  + theme(axis.text.x=element_blank(), 
            axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
            axis.title.y=element_blank(),legend.position="none")
    )
  })
  
  
  lineupPlot <- reactive({
    autoInvalidate()
    n <- input$num
    model <- lm(y~x, data=filteredData())
    resid.df <- data.frame(x=filteredData()$x, y=filteredData()$y,
                          .resid=residuals(model), .fitted=fitted(model))
    switch(input$plot,
           scatter= ggplot(filteredData(), aes(x, y)) %+% 
             lineup(null_permute("x"), filteredData(), n=n, pos=sample(n, 1)) 
           + geom_point() + facet_wrap(~.sample, nrow=ceiling(sqrt(n))) +theme(axis.text.x=element_blank(), 
          axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(),
          axis.title.y=element_blank()),
           scatterSmooth = ggplot(filteredData(), aes(x, y)) %+% 
            lineup(null_permute('x'), filteredData(), n=n, pos=sample(n,1)) +  geom_point() +
             geom_smooth(method="lm", se=FALSE) + facet_wrap(~.sample, nrow=ceiling(sqrt(n)))+ theme(axis.text.x=element_blank(), 
            axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
            axis.title.y=element_blank()),
          box=ggplot(filteredData(), aes(x=x, y=y, fill=x)) %+% lineup(null_permute("x"),filteredData(),
          n=n, pos=sample(n,1))  + geom_boxplot(aes(alpha=0.6)) + scale_fill_brewer("", palette="Set2") 
          + facet_wrap(~.sample, nrow=ceiling(sqrt(n)))  + theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), 
          legend.position="none"),
           den=ggplot(filteredData(), aes(x=y, group=x)) %+% lineup(null_permute("x"),
               filteredData(), n=n, pos=sample(n,1)) + geom_density(aes(alpha=0.6, fill=x)) 
          + scale_fill_brewer("", palette="Set2")+facet_wrap(~.sample, nrow=ceiling(sqrt(n)))+ theme(axis.text.x=element_blank(), 
            axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), 
            axis.title.y=element_blank(), legend.position="none"),
           resid=ggplot(resid.df, aes(x=x, y=.resid)) %+%
             lineup(null_lm(y~x, method='boot'), n=n, pos=sample(n,1), resid.df) +geom_point()
             + facet_wrap(~.sample, nrow=ceiling(sqrt(n))) + theme(axis.text.x=element_blank(), 
            axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
            axis.title.y=element_blank()),
          residSmooth = ggplot(resid.df, aes(x=x, y=.resid)) %+%
            lineup(null_lm(y~x, method='boot'), n=n, pos=sample(n,1), resid.df) +geom_point() +  
            geom_smooth(method="lm", se=FALSE)+ facet_wrap(~.sample, nrow=ceiling(sqrt(n))) + theme(axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(), axis.title.x=element_blank(), 
            axis.title.y=element_blank())
    )
  })
  
  output$nullPlot <- renderPlot({
    nullSwitch()
  })
  
  output$lineup <- renderPlot({
    if(input$plot=="scatter"||input$plot=="scatterSmooth"||input$plot=="box"||
         input$plot=="den"||input$plot=="resid"||input$plot=="residSmooth"){
    lineupPlot()
    }else{
      switch(input$plot,
      qq=qqPlot(),
      mosaic=mosaicPlot()
      )
    }
  })
  
  output$plotPos <- renderText({
    if(input$plot!="qq"&&input$plot!="mosaic"){
    paste("The true data is in plot ",attr(lineupPlot()$data, "pos"),".", sep="")
    }else{
      paste("The true data is in plot ",qqPos(),".", sep="")
    }
  })
})