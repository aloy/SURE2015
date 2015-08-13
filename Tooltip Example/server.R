library(shiny)
library(shinyBS)
library(ggvis)
library(htmlwidgets)
data(mtcars)
shinyServer(function(input, output, session) {

#Tooltip in ggvis
  
 base <-  mtcars %>% ggvis(x=~wt, y=~mpg, fill=~factor(cyl)) %>% layer_points() 
  
 all_values <- function(x) {
   if(is.null(x)) return(NULL)
   paste0(names(x), ": ", format(x), collapse = "<br />")
 }
   
 base %>% 
   handle_hover(function(data, ...) str(data)) %>% 
#    add_tooltip(all_values, "click") %>%
   bind_shiny("visplot")

#Prints location to console–might be some way to get that output to print?

data <- reactive({
  if(is.na(exclude()[1,1])){
  data.frame(mtcars)
  }
  else{
    n <- which(rownames(mtcars)==rownames(exclude()))
       data <- data.frame(mtcars[-n,])
  }
})

output$ggplot <- renderPlot({
  ggplot(data(), aes(x=wt, y=mpg)) + geom_point(aes(colour=factor(cyl)))
})


exclude <- reactive({
 x <- data.frame(nearPoints(mtcars, input$plot_click, xvar="wt", yvar="mpg"))
 x
})

output$info <- renderPrint({
exclude()
})

#Only briefly excludes data points

})