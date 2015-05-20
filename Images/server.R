library(shiny)
library(EBImage)
library(gridGraphics)

getMontage <- function(images)
{
     #images <- list('395 X 425 Nuclear'=readImage('/Users/jaywarrick/Downloads/blobs.png'))

     if(is.null(images) || length(images)==0)
     {
          return(NULL)
     }

     n <- length(images)

     top.vp <- viewport(layout=grid.layout(2, n,
                                           widths=unit(rep(1/n, n), rep("npc",n)),
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

getServer <- function(input, output)
{
     shinyServer(function(input, output) {

          observe({
               if(input$closeButton > 0){
                    stopApp(list(input$n))
               }
          })
          theFiles <- reactive({
               if(input$n == 0)
               {
                    return(NULL)
               }
               ret <- list()
               for(i in 1:input$n)
               {
                    ret[[paste0('Image ', i)]] <- '/Users/jaywarrick/Downloads/blobs.png'
               }
               return(ret)
          })

          theImages <- reactive({
               lapply(theFiles(), readImage)
          })

          theAdjustedImages <- reactive({
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
               n <- min(z(), 3)
               if(n==0)
               {
                    return(NULL)
               }
               getMontage(theAdjustedImages()[1:n])
          })

          theMontage2 <- reactive({
               n <- min(z(), 6)
               if(n < 4)
               {
                    return(NULL)
               }
               getMontage(theAdjustedImages()[4:n])
          })

          output$montage1 <- renderPlot({theMontage1()})

          output$montage2 <- renderPlot({theMontage2()})

          output$adjusters1 <- renderUI({
               n <- min(z(), 3)
               if(n==0)
               {
                    return(NULL)
               }
               duh <- lapply(1:n, function(i){
                    value <- c(0, 1)
                    if(!is.null(input[[paste0('adjuster',i)]]))
                    {
                         value <- input[[paste0('adjuster',i)]]
                    }
                    column(width=12/(n), sliderInput(paste0('adjuster',i), width='100%', label=NULL, min=0, max=1, value=value))
               })
               fluidRow(duh)
          })

          output$adjusters2 <- renderUI({
               n <- min(z(), 6)
               if(n < 4)
               {
                    return(NULL)
               }
               duh <- lapply(4:n, function(i){
                    value <- c(0, 1)
                    if(!is.null(input[[paste0('adjuster',i)]]))
                    {
                         value <- input[[paste0('adjuster',i)]]
                    }
                    column(width=12/(n-3), sliderInput(paste0('adjuster',i), width='100%', label=NULL, min=0, max=1, value=value))
               })
               fluidRow(duh)
          })

     })
}

