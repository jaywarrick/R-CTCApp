rm(list=ls())
source("/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/CTCApp_HelperFunctions.R")
library(foreign)
library(data.table)

shinyServer(function(input, output) {

     ##### General reactives #####
     observe({
          if(input$closeButton > 0)
          {
               stopApp(list(input$n))
          }
     })
     output$downloadReport <- downloadHandler(
          filename = function() {
               paste('CTC App Report', sep = '.', 'docx')
          },

          content = function(file) {
               src <- normalizePath('report.Rmd')
               library(rmarkdown)
               out <- render('report.Rmd', word_document(), runtime='static')
               file.rename(out, file)
          }
     )
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
     thePlot1 <- reactive({ getPlot(table=theTable(), x=input$x1, y=input$y1, goodOld=TRUE, goodNew=theGood1(), logX=input$x1Log, logY=input$y1Log, randoms=theRandoms(), autoX=input$x1Auto, autoY=input$y1Auto, threshX=x1Thresh(), threshY=y1Thresh(), rangeX=input$x1Range, rangeY=input$y1Range) })

     # outputs
     output$x1Threshold <- renderText({ getThreshText(column=input$x1, thresh=x1Thresh(), auto=input$x1Auto, log=input$x1Log, range=input$x1Range) })
     output$y1Threshold <- renderText({ getThreshText(column=input$y1, thresh=y1Thresh(), auto=input$y1Auto, log=input$y1Log, range=input$y1Range) })
     output$plot1 <- renderPlot({ thePlot1() })

     ##### Filter 2 #####
     #inputs
     output$x2Slider <- renderUI({ getSliderUI(name='x2Range', axis='x', table=theTable(), column=input$x2, auto=input$x2Auto, log=input$x2Log) })
     output$y2Slider <- renderUI({ getSliderUI(name='y2Range', axis='y', table=theTable(), column=input$y2, auto=input$y2Auto, log=input$y2Log) })
     output$x2Select <- renderUI({ selectInput(inputId="x2", label = h4("X axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })
     output$y2Select <- renderUI({ selectInput(inputId="y2", label = h4("Y axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })

     # Reactives
     x2Thresh <- reactive({ getAutoThresh(table=theTable(), column=input$x2, tail=input$x2Tail, log=input$x2Log, mads=theMads(), logMads=theLogMads()) })
     y2Thresh <- reactive({ getAutoThresh(table=theTable(), column=input$y2, tail=input$y2Tail, log=input$y2Log, mads=theMads(), logMads=theLogMads()) })
     theGood2 <- reactive({ getGood(table=theTable(), x=input$x2, y=input$y2, choiceX=input$x2Choice, choiceY=input$y2Choice, autoX=input$x2Auto, autoY=input$y2Auto, threshX=x2Thresh(), threshY=y2Thresh(), logX=input$x2Log, logY=input$y2Log, rangeX=input$x2Range, rangeY=input$y2Range) })
     thePlot2 <- reactive({ getPlot(table=theTable(), x=input$x2, y=input$y2, goodOld=theGood1(), goodNew=theGood1() & theGood2(), logX=input$x2Log, logY=input$y2Log, randoms=theRandoms(), autoX=input$x2Auto, autoY=input$y2Auto, threshX=x2Thresh(), threshY=y2Thresh(), rangeX=input$x2Range, rangeY=input$y2Range) })

     # outputs
     output$x2Threshold <- renderText({ getThreshText(column=input$x2, thresh=x2Thresh(), auto=input$x2Auto, log=input$x2Log, range=input$x2Range) })
     output$y2Threshold <- renderText({ getThreshText(column=input$y2, thresh=y2Thresh(), auto=input$y2Auto, log=input$y2Log, range=input$y2Range) })
     output$plot2 <- renderPlot({ thePlot2() })

     ##### Filter 3 #####
     #inputs
     output$x3Slider <- renderUI({ getSliderUI(name='x3Range', axis='x', table=theTable(), column=input$x3, auto=input$x3Auto, log=input$x3Log) })
     output$y3Slider <- renderUI({ getSliderUI(name='y3Range', axis='y', table=theTable(), column=input$y3, auto=input$y3Auto, log=input$y3Log) })
     output$x3Select <- renderUI({ selectInput(inputId="x3", label = h4("X axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })
     output$y3Select <- renderUI({ selectInput(inputId="y3", label = h4("Y axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })

     # Reactives
     x3Thresh <- reactive({ getAutoThresh(table=theTable(), column=input$x3, tail=input$x3Tail, log=input$x3Log, mads=theMads(), logMads=theLogMads()) })
     y3Thresh <- reactive({ getAutoThresh(table=theTable(), column=input$y3, tail=input$y3Tail, log=input$y3Log, mads=theMads(), logMads=theLogMads()) })
     theGood3 <- reactive({ getGood(table=theTable(), x=input$x3, y=input$y3, choiceX=input$x3Choice, choiceY=input$y3Choice, autoX=input$x3Auto, autoY=input$y3Auto, threshX=x3Thresh(), threshY=y3Thresh(), logX=input$x3Log, logY=input$y3Log, rangeX=input$x3Range, rangeY=input$y3Range) })
     thePlot3 <- reactive({ getPlot(table=theTable(), x=input$x3, y=input$y3, goodOld=theGood1() & theGood2(), goodNew=theGood1() & theGood2() & theGood3(), logX=input$x3Log, logY=input$y3Log, randoms=theRandoms(), autoX=input$x3Auto, autoY=input$y3Auto, threshX=x3Thresh(), threshY=y3Thresh(), rangeX=input$x3Range, rangeY=input$y3Range) })

     # outputs
     output$x3Threshold <- renderText({ getThreshText(column=input$x3, thresh=x3Thresh(), auto=input$x3Auto, log=input$x3Log, range=input$x3Range) })
     output$y3Threshold <- renderText({ getThreshText(column=input$y3, thresh=y3Thresh(), auto=input$y3Auto, log=input$y3Log, range=input$y3Range) })
     output$plot3 <- renderPlot({ thePlot3() })

     ##### Final Plot 1 #####
     output$x4Select <- renderUI({ selectInput(inputId="x4", label = h4("X axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })
     output$y4Select <- renderUI({ selectInput(inputId="y4", label = h4("Y axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })
     thePlot4 <- reactive({ getPlot(table=theTable(), x=input$x4, y=input$y4, goodOld=!(theGood1() & theGood2() & theGood3()), goodNew=(theGood1() & theGood2() & theGood3()), logX=input$x4Log, logY=input$y4Log, randoms=theRandoms(), autoX=NULL, autoY=NULL, threshX=NULL, threshY=NULL, rangeX=NULL, rangeY=NULL) })
     output$plot4 <- renderPlot({ thePlot4() })
})