rm(list=ls())
library(shiny)
library(EBImage)
library(gridGraphics)
library(foreign)

getMontage <- function(images)
{
     #images <- list('395 X 425 Nuclear'=readImage('/Users/jaywarrick/Downloads/blobs.png'))

     if(is.null(images) || length(images)==0)
     {
          grid.newpage()
          return
     }

     n <- length(images)

     top.vp <- viewport(layout=grid.layout(2, 3,
                                           widths=unit(rep(1/3, 3), rep("npc",3)),
                                           heights=unit(c(0.2,0.8), rep("npc",2))))

     #i <- 1
     theList <- lapply(1:n, function(i){
          viewport(layout.pos.col = i, layout.pos.row = 1, name = paste0('label',i))
     })

     theList <- append(theList, lapply(1:n, function(i){
          viewport(layout.pos.col = i, layout.pos.row = 2, name = paste0('image',i))
     }))


     grid.newpage()
     montage <- vpTree(top.vp, do.call(vpList, theList))
     pushViewport(montage)


     for(i in 1:n)
     {
          seekViewport(paste0('label',i))
          grid.text(names(images)[i])
     }
     for(i in 1:n)
     {
          seekViewport(paste0('image',i))
          grid.draw(rasterGrob(images[[i]]))
     }
}

getAdjustedImage <- function(image, limits)
{
     if(length(limits)!=2)
     {
          return(image)
     }
     (image-limits[1])/(limits[2]-limits[1])
}

cropsTable <- read.arff('/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/CellCrops.arff')
cropsTable$Id <- as.numeric(as.character(cropsTable$Id))

values <- reactiveValues(stateTable=data.frame(Id=unique(cropsTable$Id), No.Maybe.Yes=rep(0, length.out=length(unique(cropsTable$Id)))))

imageAdjusters <- lapply(1:6, function(j){
     column(width=12/3, sliderInput(paste0('adjuster',j), width='100%', label=NULL, min=0, max=1, value=c(0,1)))
})

# getServer <- function(input, output)
# {
shinyServer(function(input, output, session) {

     observe({
          if(input$closeButton > 0)
          {
               stopApp()
          }
     })

     observe({
          if(input$prevButton > 0)
          {
               isolate(updateSliderInput(session, "index", value=c(input$index-1)))
          }
     })

     observe({
          if(input$nextButton > 0)
          {
               isolate(updateSliderInput(session, "index", value=c(input$index+1)))
          }
     })

     observe({
          if(!is.null(input$noButton))
          {
               if(input$noButton > 0)
               {
                    isolate({
                         values$stateTable$No.Maybe.Yes[theIndex()] <- 0
                         #print(values$stateTable)
                         updateSliderInput(session, "index", value=c(input$index+1))
                    })
               }
          }
     })

     observe({
          if(!is.null(input$maybeButton))
          {
               if(input$maybeButton > 0){
                    isolate({
                         values$stateTable$No.Maybe.Yes[theIndex()] <- 1
                         #print(values$stateTable)
                         updateSliderInput(session, "index", value=c(input$index+1))
                    })
               }
          }
     })

     observe({
          if(!is.null(input$yesButton))
          {
               if(input$yesButton > 0){
                    isolate({
                         values$stateTable$No.Maybe.Yes[theIndex()] <- 2
                         #print(values$stateTable)
                         updateSliderInput(session, "index", value=c(input$index+1))
                    })
               }
          }
     })

     theState <- reactive({
          theId()
          values$stateTable
          isolate({
               temp <- values$stateTable$No.Maybe.Yes[theIndex()]
               if(temp == 0)
               {
                    temp <- "No"
               }
               else if(temp == 1)
               {
                    temp <- "Maybe"
               }
               else
               {
                    temp <- "Yes"
               }
               print("got here")
               print(theId())
               list(index=paste0("Index: ", theIndex()), id=paste0("Cell Id: ", theIndex()), state=paste0("State: ", temp))
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
          input$index
     })

     theId <- reactive({
          isolate(temp <- values$stateTable)
          temp$Id[theIndex()]
     })

     theFiles <- reactive({
          ret <- list()
          temp <- subset(cropsTable, Id==theId())
          for(i in 1:nrow(temp))
          {
               path <- temp[i,]$Value
               filename <- basename(path)
               ret[[filename]] <- path
          }
          return(ret)
     })

     theImages <- reactive({
          lapply(theFiles(), readImage)
     })

     theAdjustedImages <- reactive({
          if(z()==0)
          {
               return(NULL)
          }
          ret <- theImages()
          for(i in 1:z())
          {
               ret[[i]] <- getAdjustedImage(ret[[i]], input[[paste0('adjuster',i)]])
          }
          return(ret)
     })

     z <- reactive({
          if(is.null(theImages()))
          {
               0
          }
          else
          {
               length(theImages())
          }
     })

     theMontage1 <- reactive({
          i <- min(z(), 3)
          if(i==0)
          {
               return(NULL)
          }
          getMontage(theAdjustedImages()[1:i])
     })

     theMontage2 <- reactive({
          i <- min(z(), 6)
          if(i < 4)
          {
               return(NULL)
          }
          getMontage(theAdjustedImages()[4:i])
     })

     output$montage1 <- renderPlot({theMontage1()})

     output$montage2 <- renderPlot({theMontage2()})

     output$adjusters1 <- renderUI({
          #print(nrow(theTable()))
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
          #print(nrow(theTable()))
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
# }

