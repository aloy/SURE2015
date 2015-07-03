library("ggvis")
library("Lock5Data")
data("CaffeineTaps")
library(mosaic)

shinyServer(function(input, output, session) {
  # A reactive subset of mtcars
  trials <- reactive({
    perms <- do(input$n) * diff(mean(Taps ~ shuffle(Group), data = CaffeineTaps))
    colnames(perms) <- "perms"
    data.frame(perms)
  })
  
  input_width <- reactive(input$w)
  
  # A simple visualisation. In shiny apps, need to register observers
  # and tell shiny where to put the controls
  trials %>%
    ggvis(~perms) %>%
    layer_histograms(width = input_width) %>%
    bind_shiny("plot", "plot_ui")
  
  output$stats <- renderTable({
    favstats(~perms, data = trials())
  })
})