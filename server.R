rm(list=ls())
source("/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/CTCApp_HelperFunctions.R")
library(foreign)

shinyServer(function(input, output) {

     ##### General reactives #####
     theTable <- reactive({
          if(is.null(input$data))
          {
               temp <- data.frame(x=numeric(0), y=numeric(0))
          }
          else
          {
               path <- input$data$datapath[1]
               temp <- data.frame(reorganizeTable(read.arff(path)))
          }
          translateNames(temp)
     })
     theNumerics <- reactive({
          sapply(theTable(), is.numeric)
     })
     theNames <- reactive({names(theTable())})
     theMads <- reactive({getMads(theTable())})
     theLogMads <- reactive({getLogMads(theTable())})
     theRandoms <- reactive({runif(nrow(theTable()), min=1.5, max=2.5)})

     ##### Filter 1 #####
     x1Thresh <- reactive({
          if(is.null(input$x1) || (input$x1 == 'None'))
          {
               0
          }
          else
          {
               if(input$x1Tail==2)
               {
                    if(input$x1Log == 1)
                    {
                         as.numeric(10^(log10(median(theTable()[,input$x1], na.rm=T)) + 3*theLogMads()[input$x1]))
                    }
                    else
                    {
                         as.numeric(median(theTable()[,input$x1], na.rm=T) + 3*theMads()[input$x1])
                    }
               }
               else
               {
                    if(input$x1Log == 1)
                    {
                         as.numeric(10^(log10(median(theTable()[,input$x1], na.rm=T)) - 3*theLogMads()[input$x1]))
                    }
                    else
                    {
                         as.numeric(median(theTable()[,input$x1], na.rm=T) - 3*theMads()[input$x1])
                    }
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
                    if(input$y1Log == 1)
                    {
                         as.numeric(10^(log10(median(theTable()[,input$y1], na.rm=T)) + 3*theLogMads()[input$y1]))
                    }
                    else
                    {
                         as.numeric(median(theTable()[,input$y1], na.rm=T) + 3*theMads()[input$y1])
                    }
               }
               else
               {
                    if(input$y1Log == 1)
                    {
                         as.numeric(10^(log10(median(theTable()[,input$y1], na.rm=T)) - 3*theLogMads()[input$y1]))
                    }
                    else
                    {
                         as.numeric(median(theTable()[,input$y1], na.rm=T) - 3*theMads()[input$y1])
                    }
               }
          }
     })
     theGood1 <- reactive({
          # Grab either the + or minus population based on user choice and call it good and the other bad
          if(is.null(input$x1) || input$x1 == 'None')
          {
               x1Good <- rep(T, length(nrow(theTable())))
          }
          else if(input$x1Auto==1)
          {
               x1Good <- (theTable()[,input$x1] > x1Thresh()) == (input$x1Choice == 1)
          }
          else
          {
               if(input$x1Log == 1)
               {
                    x1Good <- ((theTable()[,input$x1] > 10^input$x1Range[1]) & (theTable()[,input$x1] < 10^input$x1Range[2]))  == (input$x1Choice == 1)
               }
               else
               {
                    x1Good <- ((theTable()[,input$x1] > input$x1Range[1]) & (theTable()[,input$x1] < input$x1Range[2]))  == (input$x1Choice == 1)
               }
          }
          if(is.null(input$y1) || input$y1 == 'None')
          {
               y1Good <- rep(T, length(nrow(theTable())))
          }
          else if(input$y1Auto==1)
          {
               y1Good <- (theTable()[,input$y1] > y1Thresh()) == (input$y1Choice == 1)
          }
          else
          {
               if(input$y1Log == 1)
               {
                    y1Good <- ((theTable()[,input$y1] > 10^input$y1Range[1]) & (theTable()[,input$y1] < 10^input$y1Range[2]))  == (input$y1Choice == 1)
               }
               else
               {
                    y1Good <- ((theTable()[,input$y1] > input$y1Range[1]) & (theTable()[,input$y1] < input$y1Range[2]))  == (input$y1Choice == 1)
               }
          }
          x1Good & y1Good
     })
     output$x1Threshold <- renderText({
          if(is.null(input$x1) || input$x1 == 'None')
          {
               ''
          }
          else if(input$x1Auto == 1)
          {
               nums <- format(round(x1Thresh(), 1), nsmall = 1)
               txt <- paste(nums, collapse='')
               paste("Threshold: ", txt, sep='')
          }
          else
          {
               if(input$x1Log == 1)
               {
                    nums <- format(round(10^input$x1Range, 1), nsmall = 1)
               }
               else
               {
                    nums <- format(round(input$x1Range, 1), nsmall = 1)
               }
               txt <- paste(nums, collapse=' - ')
               paste("Thresholds: ", txt, sep='')
               txt <- paste(nums, collapse=' - ')
               paste("Thresholds: ", txt, sep='')
          }
     })
     output$y1Threshold <- renderText({
          if(is.null(input$y1) || input$y1 == 'None')
          {
               ''
          }
          else if(input$y1Auto == 1)
          {
               nums <- format(round(y1Thresh(), 1), nsmall = 1)
               txt <- paste(nums, collapse='')
               paste("Threshold: ", txt, sep='')
          }
          else
          {
               if(input$y1Log == 1)
               {
                    nums <- format(round(10^input$y1Range, 1), nsmall = 1)
               }
               else
               {
                    nums <- format(round(input$y1Range, 1), nsmall = 1)
               }
               txt <- paste(nums, collapse=' - ')
               paste("Thresholds: ", txt, sep='')
          }

     })
     logParam1 <- reactive({
          if(input$x1Log == 1 & input$y1Log == 1)
          {
               'xy'
          }
          else if(input$x1Log == 1)
          {
               'x'
          }
          else if(input$y1Log == 1)
          {
               'y'
          }
          else
          {
               ''
          }
     })
     output$plot1 <- renderPlot({

          if((is.null(input$x1) && is.null(input$y1)) || ((input$x1 == 'None') && (input$y1 == 'None')))
          {
               # Do nothing
               NULL
          }
          else
          {
               good <- theGood1()
               bad <- !good

               # Make the main plot
               if(is.null(input$x1) || input$x1 == 'None')
               {
                    xlim <- c(1,3)
                    x1 <- theRandoms()
               }
               else
               {
                    x1 <- theTable()[,input$x1]
                    if(input$x1Log == 1)
                    {
                         xlim <- range(x1[x1 > 0])
                    }
                    else
                    {
                         xlim <- range(x1)
                    }
               }
               if(is.null(input$y1) || input$y1 == 'None')
               {
                    ylim <- c(1,3)
                    y1 <- theRandoms()
               }
               else
               {
                    y1 <- theTable()[,input$y1]
                    if(input$y1Log == 1)
                    {
                         ylim <- range(y1[y1 > 0])
                    }
                    else
                    {
                         ylim <- range(y1)
                    }
               }
               plot(x1[bad], y1[bad], pch=20, col=rgb(0,0,0,0.25), bg=rgb(0,0,0,0.25), xlim=xlim, ylim=ylim, xlab=input$x1, ylab=input$y1, log=logParam1())
               points(x1[good], y1[good], pch=20, col=rgb(1,0,0,0.25), bg=rgb(1,0,0,0.25))

               # Plot threshold lines
               if(input$x1Auto == 1)
               {
                    abline(v=x1Thresh())
               }
               else
               {
                    if(input$x1Log == 1)
                    {
                         abline(v=10^input$x1Range)
                    }
                    else
                    {
                         abline(v=input$x1Range)
                    }
               }
               if(input$y1Auto == 1)
               {
                    abline(h=y1Thresh())
               }
               else
               {
                    if(input$y1Log == 1)
                    {
                         abline(h=10^input$y1Range)
                    }
                    else
                    {
                         abline(h=input$y1Range)
                    }
               }
          }
     })
     output$x1Slider <- renderUI({
          if (!is.null(input$x1)  && input$x1 != 'None' && input$x1Auto==2)
          {
               nums <- theTable()[,input$x1]
               if(input$x1Log == 1)
               {
                    temp <- range(nums[nums > 0])
                    temp <- log10(temp)
                    sliderLabel <- 'log10(x)'
               }
               else
               {
                    temp <- range(nums)
                    sliderLabel <- 'x'
               }
               sliderInput(inputId="x1Range",label=sliderLabel, min=temp[1], max=temp[2], value=c(temp[1],temp[2]))
          }
     })
     output$y1Slider <- renderUI({
          if (!is.null(input$y1) && input$y1 != 'None' && input$y1Auto==2)
          {
               nums <- theTable()[,input$y1]
               if(input$y1Log == 1)
               {
                    temp <- range(nums[nums > 0])
                    temp <- log10(temp)
                    sliderLabel <- 'log10(y)'
               }
               else
               {
                    temp <- range(nums)
                    sliderLabel <- 'y'
               }
               sliderInput(inputId="y1Range",label=sliderLabel, min=temp[1], max=temp[2], value=c(temp[1],temp[2]))
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

     ##### Filter 2 #####

})