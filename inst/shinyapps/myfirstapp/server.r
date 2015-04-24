
shiny::shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot

  output$distPlot1 <- shiny::renderPlot({

    media <- input$media
    dest <- input$dest

    set.seed(1)
    x <- rnorm(1000, media, dest)

    # draw the histogram with the specified number of bins
    hist(x, breaks = 20, col = 'darkgray', border = 'white',
         xlim = c(-15, 15), main = "Histograma")
  })

  output$distPlot2 <- shiny::renderPlot({

    media <- input$media
    dest <- input$dest

    set.seed(1)
    x <- rnorm(1000, media, dest)

    # draw the histogram with the specified number of bins
    boxplot(x, col = 'darkgray', ylim = c(-15,15),
         main = "Boxplot", horizontal = T)
  })

})
