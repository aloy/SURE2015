library(shiny)
library(shinyjs)
shinyServer(function(input,output, session){
library(mosaic)
library(ggvis)
library(dplyr)
library(Lock5Data)
data("CaffeineTaps")

filedata <- reactive({
  if(input$chooseData=="uploadYes"){
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    return(      
      read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    )
  }
  else
  data.frame(CaffeineTaps)
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
  vars <- colnames(df)[sapply(df,is.factor)]
  names(vars)=vars
  if(input$chooseData=="uploadNo")
    textInput("choose",label="Choose a categorical variable with two groups of equal size:", value="Group")
  else
    selectInput("choose",label="Choose a categorical variable with two groups of equal size:",vars)
})

output$varChoose2 <- renderUI({
  df <- filedata()
  if (is.null(df)) 
    return(NULL)
  vars2 <- colnames(df)[sapply(df,is.numeric)]
  names(vars2)=vars2
  if(input$chooseData=="uploadNo")
    textInput("choose2",label="Choose a quantitative variable to examine between groups:", value="Taps")
  else
    selectInput("choose2",label="Choose a quantitative variable to examine between groups:",vars2)
})


simdata <- reactive({
  catName <- input$choose
  Category <- filedata()[,catName]
  quantName <- input$choose2
  Quantitative <- filedata()[,quantName]
  data.frame(Category, Quantitative)  
})

output$summary <- renderTable({
  favstats(~Quantitative|Category, data = simdata())  
})

observedDiff <- reactive({
grouped<- group_by(simdata(), Category)
summarise(summarise(grouped, mean = mean(Quantitative)), mean.diff=mean[1]-mean[2])
})

output$observedDiff <- renderText({
observedDiff()$mean.diff
})


trials <- reactive({
perms <- do(input$num) * mean(Quantitative~shuffle(Category), data=simdata())
perms$diff <- perms[,1]-perms[,2]
colnames(perms) <- c("Group 1", "Group 2", "diff")
data.frame(perms)
})

output$pval <- renderPrint({
n <- input$num
pvalSwitch <- switch(input$test, 
                   tt = (sum(abs(trials()$diff) <= observedDiff()$mean.diff) +1)/(n+1),
                   lt = (sum(trials()$diff <= observedDiff()$mean.diff) +1)/(n+1),
                   ut = (sum(trials()$diff >= observedDiff()$mean.diff) +1)/(n+1)
)
pvalSwitch
  })

# qqdata <- reactive({
#   n <- input$num
#   probabilities <- (1:n)/(1+n)
#   normal.quantiles <- qnorm(probabilities, mean(trials()[,"diff"], na.rm = T), sd(trials()[,"diff"], na.rm = T))
#   qqdata0 <- data.frame(sort(normal.quantiles), sort(trials()[,"diff"]))
#   colnames(qqdata0) <- c("normal.quantiles", "diffs")
#   data.frame(qqdata0)
#   })

# observe(
# ggSwitch <- switch(input$type,
#   his = trials %>%
#     ggvis(~diff) %>%
#     layer_histograms(width = input_slider(0.1, 2, step=0.1, value=0.6)) %>%
#     add_axis("x", title = "Mean Difference") %>%
#     add_axis("y", title="Count") %>%
#     bind_shiny("trialsHist2", "trialsHist2_ui"),
#   den = trials %>% 
#     ggvis(~diff) %>% 
#     layer_densities() %>%
#     add_axis("x", title = "Mean Difference") %>%
#     add_axis("y", title="Density") %>%
#     bind_shiny("trialsHist2", "trialsHist2_ui"),
#   qq = qqdata %>% 
#     ggvis(~normal.quantiles, ~diffs) %>% 
#     layer_points() %>% 
#     add_axis("x", title="Theoretical") %>%
#     add_axis("y", title="Sample") %>%
#   bind_shiny("trialsHist2", "trialsHist2_ui")
#   )
# )
# 
output$summary2 <- renderTable({
  favstats(trials()$diff) 
})

})