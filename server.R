rm(list=ls())
library(foreign)
library(data.table)
library(EBImage)
library(gridGraphics)
library(foreign)
library(xlsx)

values <- reactiveValues(stateTable=NULL, dataPath=NULL)

savedDataPath <- NULL

imageAdjusters <- lapply(1:6, function(j){
     column(width=12/3, sliderInput(paste0('adjuster',j), width='100%', label=NULL, min=0, max=1, value=c(0,1)))
})

shinyServer(function(input, output, session) {

     ##### General reactives #####
     observe({
          if(input$closeButton > 0)
          {
               stopApp(list(input$n))
          }
     })
     observe({
          if(input$closeButton2 > 0)
          {
               stopApp(list(input$n))
          }
     })

     observe({
          if(!is.null(input$saveButton) && input$saveButton > 0)
          {
               src <- normalizePath('report.Rmd')
               library(rmarkdown)
               isolate({
                    out <- render('report.Rmd', word_document(), runtime='static')
                    file.rename(out, file.path(savedDataPath, paste0(input$reportName,".docx")))
                    temp1 <- theTable()
                    temp1$No.Maybe.Yes <- values$stateTable$No.Maybe.Yes
                    temp1$No.Maybe.Yes <- sub(0, "No", temp1$No.Maybe.Yes)
                    temp1$No.Maybe.Yes <- sub("1", "Maybe", temp1$No.Maybe.Yes)
                    temp1$No.Maybe.Yes <- sub("2", "Yes", temp1$No.Maybe.Yes)
                    temp2 <- subset(temp1, No.Maybe.Yes == "Yes")
                    save.xlsx(file.path(savedDataPath, paste0(input$reportName,".xlsx")), temp1, temp2)
               })
          }
     })
#      output$downloadReport <- downloadHandler(
#           filename = function() {
#                paste('CTC App Report', sep = '.', 'docx')
#           },
#
#           content = function(file) {
#                src <- normalizePath('report.Rmd')
#                library(rmarkdown)
#                out <- render('report.Rmd', word_document(), runtime='static')
#                file.rename(out, file)
#                isolate({
#                     temp1 <- theTable()
#                     temp1$No.Maybe.Yes <- values$stateTable$No.Maybe.Yes
#                     temp1$No.Maybe.Yes <- sub(0, "No", temp1$No.Maybe.Yes)
#                     temp1$No.Maybe.Yes <- sub("1", "Maybe", temp1$No.Maybe.Yes)
#                     temp1$No.Maybe.Yes <- sub("2", "Yes", temp1$No.Maybe.Yes)
#                     temp2 <- subset(temp1, No.Maybe.Yes == "Yes")
#                     save.xlsx(savedDataPath, temp1, temp2)
#                })
#           }
#      )
     observe({
          if(!is.null(input$dataButton))
          {
               if(input$dataButton > 0){
                    isolate({
                         values$dataPath <- fileChoose()
                         savedDataPath <<- dirname(values$dataPath)
                         print(savedDataPath)
                    })
               }
          }
     })
     output$filePath <- renderText({
          if(is.null(values$dataPath))
          {
               return("")
          }
          else
          {
               return(basename(values$dataPath))
          }
     })
     theTable <- reactive({
          if(is.null(values$dataPath))
          {
               temp <- data.frame(x=numeric(0), y=numeric(0))
               return(temp)
          }
          else
          {
               path <- values$dataPath
               temp <- data.frame(reorganizeTable(read.arff(path)))
          }
          # This is done in JEX now
          #           totCols <- grepl("Tot", names(temp))
          #           theNames <- names(temp)[totCols]
          #           theNewNames <- sub("Tot", "Mean", theNames)
          #           for(i in 0:(length(theNames)-1))
          #           {
          #                temp[,theNewNames[i+1]] <- temp[,theNames[i+1]] / temp$n
          #           }
          return(temp)
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
     thePlot1 <- reactive({ getPlot(table=theTable(), x=input$x1, y=input$y1, goodOld=!theGood1(), goodNew=theGood1(), logX=input$x1Log, logY=input$y1Log, randoms=theRandoms(), autoX=input$x1Auto, autoY=input$y1Auto, threshX=x1Thresh(), threshY=y1Thresh(), rangeX=input$x1Range, rangeY=input$y1Range, Id=theId(), stateTable=values$stateTable) })

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
     thePlot2 <- reactive({ getPlot(table=theTable(), x=input$x2, y=input$y2, goodOld=theGood1() & !theGood2(), goodNew=theGood1() & theGood2(), logX=input$x2Log, logY=input$y2Log, randoms=theRandoms(), autoX=input$x2Auto, autoY=input$y2Auto, threshX=x2Thresh(), threshY=y2Thresh(), rangeX=input$x2Range, rangeY=input$y2Range, Id=theId(), stateTable=values$stateTable) })

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
     thePlot3 <- reactive({ getPlot(table=theTable(), x=input$x3, y=input$y3, goodOld=(theGood1() & theGood2()) & !(theGood1() & theGood2() & theGood3()), goodNew=(theGood1() & theGood2() & theGood3()), logX=input$x3Log, logY=input$y3Log, randoms=theRandoms(), autoX=input$x3Auto, autoY=input$y3Auto, threshX=x3Thresh(), threshY=y3Thresh(), rangeX=input$x3Range, rangeY=input$y3Range, Id=theId(), stateTable=values$stateTable) })

     # outputs
     output$x3Threshold <- renderText({ getThreshText(column=input$x3, thresh=x3Thresh(), auto=input$x3Auto, log=input$x3Log, range=input$x3Range) })
     output$y3Threshold <- renderText({ getThreshText(column=input$y3, thresh=y3Thresh(), auto=input$y3Auto, log=input$y3Log, range=input$y3Range) })
     output$plot3 <- renderPlot({ thePlot3() })

     ##### Final Plot 1 #####
     output$x4Select <- renderUI({ selectInput(inputId="x4", label = h4("X axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })
     output$y4Select <- renderUI({ selectInput(inputId="y4", label = h4("Y axis"), choices = c("None", theNames()[theNumerics()]), selected = "None") })
     thePlot4 <- reactive({ getPlot(table=theTable(), x=input$x4, y=input$y4, goodOld=!(theGood1() & theGood2() & theGood3()), goodNew=(theGood1() & theGood2() & theGood3()), logX=input$x4Log, logY=input$y4Log, randoms=theRandoms(), autoX=NULL, autoY=NULL, threshX=NULL, threshY=NULL, rangeX=NULL, rangeY=NULL, Id=theId(), stateTable=values$stateTable) })
     output$plot4 <- renderPlot({ thePlot4() })

     ##### Single Cell Validation #####

     cropsTable <- reactive({
          if(is.null(values$dataPath))
          {
               temp <- NULL
          }
          else
          {
               path <- values$dataPath
               print(values$dataPath)
               folder <- dirname(path)
               cropsPath <- file.path(folder,"CellCrops.arff")
               temp <- read.arff(cropsPath)
               temp$Id <- as.numeric(as.character(temp$Id))
          }
          print("Updating cropsTable")
          return(temp)
     })

     observe({
          print("Updating stateTable")
          values$stateTable <- data.frame(Id=theTable()$Id, No.Maybe.Yes=rep(NA, length.out=length(theTable()$Id)))
     })

     observe({
          if(input$closeButton > 0)
          {
               stopApp()
          }
     })

     observe({
          if(input$prevButton > 0)
          {
               isolate({
                    if(!is.null(input$index))
                    {
                         updateSliderInput(session, "index", value=c(input$index-1))
                    }
               })
          }
     })

     observe({
          if(input$nextButton > 0)
          {
               isolate({
                    if(!is.null(input$index))
                    {
                         updateSliderInput(session, "index", value=c(input$index+1))
                    }
               })
          }
     })

     observe({
          if(!is.null(input$noButton))
          {
               if(input$noButton > 0)
               {
                    isolate({
                         if(!is.null(input$index))
                         {
                              values$stateTable$No.Maybe.Yes[theTable()$Id == theId()] <- 0
                              print(values$stateTable)
                              updateSliderInput(session, "index", value=c(input$index+1))
                         }
                    })
               }
          }
     })

     observe({
          if(!is.null(input$maybeButton))
          {
               if(input$maybeButton > 0){
                    isolate({
                         if(!is.null(input$index))
                         {
                              values$stateTable$No.Maybe.Yes[theTable()$Id == theId()] <- 1
                              print(values$stateTable)
                              updateSliderInput(session, "index", value=c(input$index+1))
                         }
                    })
               }
          }
     })

     observe({
          if(!is.null(input$yesButton))
          {
               if(input$yesButton > 0){
                    isolate({
                         if(!is.null(input$index))
                         {
                              values$stateTable$No.Maybe.Yes[theTable()$Id == theId()] <- 2
                              print(values$stateTable)
                              updateSliderInput(session, "index", value=c(input$index+1))
                         }
                    })
               }
          }
     })

     output$cellIndexer <- renderUI({
          if(is.null(cropsTable()))
          {
               return(NULL)
          }
          print("Updating the cell indexer")
          theGood <- theGood1() & theGood2() & theGood3()
          ids <- unique(theTable()[theGood,'Id'])
          fluidRow(
               column(
                    width=12, align="center",
                    sliderInput("index", "Filtered Cell Index", value=1, min=1, step=1, max=length(ids), width="100%")
               )
          )
     })

     theState <- reactive({
          theId()
          values$stateTable
          isolate({
               if(is.null(theId()))
               {
                    temp <- "NA"
               }
               else
               {
                    temp <- values$stateTable$No.Maybe.Yes[values$stateTable$Id==theId()]
                    if(is.null(input$index) || length(temp)==0 || is.na(temp))
                    {
                         temp <- "NA"
                    }
                    else if(temp == 0)
                    {
                         temp <- "No"
                    }
                    else if(temp == 1)
                    {
                         temp <- "Maybe"
                    }
                    else if(temp == 2)
                    {
                         temp <- "Yes"
                    }
                    else
                    {
                         temp <- "Habba Whah?"
                    }
               }
               list(index=paste0("Index: ", theIndex()), id=paste0("Cell Id: ", theId()), state=paste0("State: ", temp))
               #HTML(paste(, "\nCell Id:", theId(), "\nState: ", temp, sep = '<br/>'))
          })
     })

     output$state <- renderUI({
          fluidRow(
               column(width=4, align="left", renderText(theState()$index)),
               column(width=4, align="left", renderText(theState()$id)),
               column(width=4, align="left", renderText(theState()$state))
          )
     })

     theIndex <- reactive({
          # This should eventually be "which(theGood1 & theGood2 & theGood3)[input$index]
          print("Updating theIndex")
          input$index
     })

     theId <- reactive({
          print("Updating theId")
          theTable()$Id[which(theGood1() & theGood2() & theGood3())[theIndex()]]
     })

     theFiles <- reactive({
          print("Trying to update theFiles")
          if(!is.null(cropsTable()))
          {
               ret <- list()
               temp <- subset(cropsTable(), Id==theId())
               print(theId())
               print(temp)
               if(nrow(temp) >= 1)
               {
                    for(i in 1:nrow(temp))
                    {
                         path <- temp[i,]$Value
                         filename <- basename(path)
                         ret[[filename]] <- path
                    }
                    print("Updated theFiles()")
                    return(ret)
               }
          }
          return(NULL)
     })

     theImages <- reactive({
          if(is.null(theFiles()))
          {
               return(NULL)
          }
          print("Updating theImages")
          lapply(theFiles(), readImage)
     })

     theAdjustedImages <- reactive({
          isolate({n <- z()})
          if(is.null(theImages()))
          {
               return(NULL)
          }
          ret <- theImages()
          for(i in 1:n)
          {
               ret[[i]] <- getAdjustedImage(ret[[i]], input[[paste0('adjuster',i)]])
          }
          print("Updating theAdjustedImages")
          return(ret)
     })

     z <- reactive({
          if(is.null(cropsTable()))
          {
               0
          }
          else
          {
               print("Updating z")
               length(cropsTable()[cropsTable()$Id==cropsTable()$Id[1],'Id'])
          }
     })

     theMontage1 <- reactive({
          i <- min(z(), 3)
          if(i==0)
          {
               return(NULL)
          }
          print("Updating theMontage1")
          getMontage(theAdjustedImages()[1:i])
     })

     theMontage2 <- reactive({
          i <- min(z(), 6)
          if(i < 4)
          {
               return(NULL)
          }
          print("Updating theMontage2")
          getMontage(theAdjustedImages()[4:i])
     })

     output$montage1 <- renderPlot({theMontage1()})

     output$montage2 <- renderPlot({theMontage2()})

     output$adjusters1 <- renderUI({
          z()
          isolate({
               i <- min(z(), 3)
               if(i==0)
               {
                    return(NULL)
               }
               fluidRow(imageAdjusters[1:i])
          })
     })

     output$adjusters2 <- renderUI({
          z()
          isolate({
               i <- min(z(), 6)
               if(i < 4)
               {
                    return(NULL)
               }
               fluidRow(imageAdjusters[4:i])
          })
     })

})