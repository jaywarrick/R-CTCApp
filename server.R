shinyServer(function(input, output) {

     output$distPlot <- renderPlot({
          x    <- rnorm(100)  # Old Faithful Geyser data
          bins <- seq(min(x), max(x), length.out = 30)

          # draw the histogram with the specified number of bins
          hist(x, breaks = bins, col = 'darkgray', border = 'white')
     })

})