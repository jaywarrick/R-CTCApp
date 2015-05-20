rm(list=ls())
source("/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/CTCApp_HelperFunctions.R")
library(foreign)
library(data.table)

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
     theNumerics <- reactive({ sapply(theTable(), is.numeric) })
     theNames <- reactive({ names(theTable()) })
     theMads <- reactive({ getMads(theTable()) })
     theLogMads <- reactive({ getLogMads(theTable()) })
     theRandoms <- reactive({ runif(nrow(theTable()), min=1.5, max=2.5) })

     ##### Filter 1 #####
     #inputs
     output$x1Slider <- renderUI({ getSliderUI(name='x1Range', axis='x', table=theTable(), column=input$x1, auto=input$x1Auto, log=input$x1Log) })
     output$y1Slider <- renderUI({ getSliderUI(name='y1Range', axis='y', table=theTable(), column=input$y1, auto=input$y1Auto, log=input$y1Log) })
     output$x1Select <- renderUI({ selectInput(inputId="x1", label = h4("X axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })
     output$y1Select <- renderUI({ selectInput(inputId="y1", label = h4("Y axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })

     # Reactives
     x1Thresh <- reactive({ getAutoThresh(table=theTable(), column=input$x1, tail=input$x1Tail, log=input$x1Log, mads=theMads(), logMads=theLogMads()) })
     y1Thresh <- reactive({ getAutoThresh(table=theTable(), column=input$y1, tail=input$y1Tail, log=input$y1Log, mads=theMads(), logMads=theLogMads()) })
     theGood1 <- reactive({ getGood(table=theTable(), x=input$x1, y=input$y1, choiceX=input$x1Choice, choiceY=input$y1Choice, autoX=input$x1Auto, autoY=input$y1Auto, threshX=x1Thresh(), threshY=y1Thresh(), logX=input$x1Log, logY=input$y1Log, rangeX=input$x1Range, rangeY=input$y1Range) })

     # outputs
     output$x1Threshold <- renderText({ getThreshText(column=input$x1, thresh=x1Thresh(), auto=input$x1Auto, log=input$x1Log, range=input$x1Range) })
     output$y1Threshold <- renderText({ getThreshText(column=input$y1, thresh=y1Thresh(), auto=input$y1Auto, log=input$y1Log, range=input$y1Range) })
     output$plot1 <- renderPlot({ getPlot(table=theTable(), x=input$x1, y=input$y1, good=theGood1(), logX=input$x1Log, logY=input$y1Log, randoms=theRandoms(), autoX=input$x1Auto, autoY=input$y1Auto, threshX=x1Thresh(), threshY=y1Thresh(), rangeX=input$x1Range, rangeY=input$y1Range) })

     ##### Filter 2 #####

})