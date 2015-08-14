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

# lb  <- linked_brush(keys = 1:nrow(mtReactive()), "red")
lb <-  reactive({
 linked_brush(keys = 1:nrow(mtReactive()), "red")
})

mtReactive <- reactive({
mtReactive0 <- data.frame(mtcars)
#   if(input$exclude >0){
#     mtReactive0 <-mtcars[!lb()$selected(),]
#   }
# if(input$reset>0){
#   mtReactive0 <- data.frame(mtcars)
# }
mtReactive0
})

# Change the colour of the points
observe({
mtReactive() %>%
  ggvis(~disp, ~mpg) %>%
  layer_points(fill := lb()$fill, size.brush := 400) %>%
  lb()$input() %>%
  bind_shiny("linked1")

# Display one layer with all points and another layer with selected points
mtReactive() %>%
  ggvis(~disp, ~mpg) %>%
  layer_points(size.brush := 400) %>%
  lb()$input() %>%
  layer_points(fill := "red", data = reactive(mtReactive()[lb()$selected(), ])) %>%
  bind_shiny("linked2")
})

output$lbtest <- renderPrint({
lb()$selected()
})

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