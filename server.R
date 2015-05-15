rm(list=ls())
source("/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/CTCApp_HelperFunctions.R")
library(foreign)

shinyServer(function(input, output) {

     theFile <- reactive({})
     theTable <- reactive({
          if(is.null(input$data))
          {
               data.frame(x=numeric(0), y=numeric(0))
          }
          else
          {
               path <- input$data$datapath[1]
               data.frame(reorganizeTable(read.arff(path)))
          }
     })
     theNumerics <- reactive({
          sapply(theTable(), is.numeric)
     })
     theNames <- reactive({names(theTable())})
     theMads <- reactive({getMads(theTable())})
     x1Thresh <- reactive({
          if(is.null(input$x1) || (input$x1 == 'None'))
          {
               0
          }
          else
          {
               if(input$x1Tail==2)
               {
                    as.numeric(median(theTable()[,input$x1], na.rm=T) + 3*theMads()[input$x1])
               }
               else
               {
                    as.numeric(median(theTable()[,input$x1], na.rm=T) - 3*theMads()[input$x1])
               }
          }
     })
     y1Thresh <- reactive({
          if(is.null(input$y1) || (input$y1 == 'None'))
          {
               0
          }
          else
          {
               if(input$y1Tail==2)
               {
                    as.numeric(median(theTable()[,input$y1], na.rm=T) + 3*theMads()[input$y1])
               }
               else
               {
                    as.numeric(median(theTable()[,input$y1], na.rm=T) - 3*theMads()[input$y1])
               }
          }
     })
     theGood <- reactive({
          # Grab either the + or minus population based on user choice and call it good and the other bad
          if(input$x1Auto==1)
          {
               x1Good <- (theTable()[,input$x1] > x1Thresh()) == (input$x1Choice == 1)
          }
          else
          {
               x1Good <- ((theTable()[,input$x1] > 10^input$x1Range[1]) & (theTable()[,input$x1] < 10^input$x1Range[2]))  == (input$x1Choice == 1)
          }
          if(input$y1Auto==1)
          {
               y1Good <- (theTable()[,input$y1] > y1Thresh()) == (input$y1Choice == 1)
          }
          else
          {
               y1Good <- ((theTable()[,input$y1] > 10^input$y1Range[1]) & (theTable()[,input$y1] < 10^input$y1Range[2])) == (input$y1Choice == 1)
          }
          x1Good & y1Good
     })

     output$x1Threshold <- renderText({
          if(input$x1Auto == 1)
          {
               nums <- format(round(x1Thresh(), 1), nsmall = 1)
               txt <- paste(nums, collapse='')
               paste("Threshold: ", txt, sep='')
          }
          else
          {
               nums <- format(round(10^input$x1Range, 1), nsmall = 1)
               txt <- paste(nums, collapse=' - ')
               paste("Thresholds: ", txt, sep='')
          }
     })
     output$y1Threshold <- renderText({
          if(input$y1Auto == 1)
          {
               nums <- format(round(y1Thresh(), 1), nsmall = 1)
               txt <- paste(nums, collapse='')
               paste("Threshold: ", txt, sep='')
          }
          else
          {
               nums <- format(round(10^input$y1Range, 1), nsmall = 1)
               txt <- paste(nums, collapse=' - ')
               paste("Thresholds: ", txt, sep='')
          }

     })

     output$plot1 <- renderPlot({

          if((is.null(input$x1) && is.null(input$y1)) || ((input$x1 == 'None') && (input$y1 == 'None')))
          {
               # Do nothing
               NULL
          }
          else if(is.null(input$x1) || (input$x1 == 'None'))
          {
               hist(theTable()[,input$y1], breaks = 30, col = 'darkgray', border = 'white')
          }
          else if(is.null(input$y1) || (input$y1 == 'None'))
          {
               hist(theTable()[,input$x1], breaks = 30, col = 'darkgray', border = 'white')
          }
          else
          {
               good <- theGood()
               bad <- !good

               # Make the main plot
               plot(theTable()[bad,input$x1], theTable()[bad,input$y1], pch=20, col=rgb(0,0,0,0.25), bg=rgb(0,0,0,0.25), xlim=range(theTable()[,input$x1]), ylim=range(theTable()[,input$y1]), xlab=input$x1, ylab=input$y1, log='xy')
               points(theTable()[good,input$x1], theTable()[good,input$y1], pch=20, col=rgb(1,0,0,0.25), bg=rgb(1,0,0,0.25))

               # Plot threshold lines
               if(input$x1Auto == 1)
               {
                    abline(v=x1Thresh())
               }
               else
               {
                    abline(v=10^input$x1Range)
               }
               if(input$y1Auto == 1)
               {
                    abline(h=y1Thresh())
               }
               else
               {
                    abline(h=10^input$y1Range)
               }
          }
     })

     output$x1Slider <- renderUI({
          if (!is.null(input$y1) && input$x1Auto==2)
          {
               temp <- range(theTable()[theTable()[,input$x1] > 0, input$x1])
               temp <- log10(temp)
               sliderInput(inputId="x1Range",label="log10(x)", min=temp[1], max=temp[2], value=c(temp[1],temp[2]))
          }
     })

     output$y1Slider <- renderUI({
          if (!is.null(input$y1) && input$y1Auto==2)
          {
               temp <- range(theTable()[theTable()[,input$y1] > 0, input$y1])
               temp <- log10(temp)
               sliderInput(inputId="y1Range",label="log10(y)", min=temp[1], max=temp[2], value=c(temp[1],temp[2]))
          }
     })

     output$x1Select <- renderUI({
          selectInput(inputId="x1",
                      label = h4("X axis"),
                      choices = c("None", theNames()[theNumerics()]),
                      selected = "None")
     })

     output$y1Select <- renderUI({
          selectInput(inputId="y1",
                      label = h4("Y axis"),
                      choices = c("None", theNames()[theNumerics()]),
                      selected = "None")
     })
})