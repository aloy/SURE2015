library(shiny)
library(shinyBS)
library(ggvis)
data(mtcars)
shinyServer(function(input, output, session) {

#Tooltip in ggvis
  
 base <-  mtcars %>% ggvis(x=~wt, y=~mpg, fill=~factor(cyl)) %>% layer_points() %>% handle_hover(function(data, ...) str(data))
  
 all_values <- function(x) {
   if(is.null(x)) return(NULL)
   paste0(names(x), ": ", format(x), collapse = "<br />")
 }
   
 base %>% 
   bind_shiny("distPlot")

#Prints location to console–might be some way to get that output to print?

output$test <- renderPrint({
  capture.output(base)
})

})