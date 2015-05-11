rm(list=ls())
source("C:/Users/Mengcheng/Documents/GitHub/R-CTC/AnalyzeCTCs_HelperFunctions.R")
library(foreign)

shinyServer(function(input, output) {

      locResults <- reorganizeTable(read.arff("E:/CTC Databases/Testing grounds/TestNewWorkFlow/Cell_x0_y0/File-Colocalization/x0_y0.arff"))
      
      temp <- calculateSummaryVariables(locResults=locResults, dapiColor=1, ckColor=2, cd45Color=3, arColor=4)
      Thresh <- temp$thresholds
      results <- temp$summary
      
      CTCs <- subset(results, !CD45.Flag & AR.Flag)
      nonCTCs <- subset(results, !CD45.Flag & !AR.Flag)
  
     output$distPlot <- renderPlot({
       
            
          # draw the histogram with the specified number of bins
          plot(nonCTCs[,input$x1], nonCTCs[,input$y1], pch=21, col=rgb(0,0,0,0.25), bg=rgb(0,0,0,0.25), xlim=range(nonCTCs[,input$x1], CTCs[,input$x1]), ylim=range(nonCTCs[,input$y1], CTCs[,input$y1]), xlab=paste(input$x1, 'Mean Intensity [au]'), ylab=paste(input$y1, 'Mean Intensity [au]'), log='xy')
     
          if (input$x1cursor & input$y1cursor){
            x1Range_Min <- input$x1Range[1]
            abline(v=x1Range_Min)
            
            x1Range_Max <- input$x1Range[2]
            abline(v=x1Range_Max)
            
            y1Range_Min <- input$y1Range[1]
            abline(h=y1Range_Min)
            
            y1Range_Max <- input$y1Range[2]
            abline(h=y1Range_Max)
            
            highlight <- nonCTCs[nonCTCs[,input$x1]>x1Range_Min & nonCTCs[,input$x1]<x1Range_Max & nonCTCs[,input$y1]>y1Range_Min & nonCTCs[,input$y1]<y1Range_Max,]
            
            points(highlight[,input$x1], highlight[,input$y1], pch=21, col=rgb(1,0,0,0.25), bg=rgb(1,0,0,0.25))
          }
          
          else if (input$x1cursor){
            x1Range_Min <- input$x1Range[1]
            abline(v=x1Range_Min)
            
            x1Range_Max <- input$x1Range[2]
            abline(v=x1Range_Max)
            
            highlight_x <- nonCTCs[nonCTCs[,input$x1]>x1Range_Min & nonCTCs[,input$x1]<x1Range_Max,]
      
            points(highlight_x[,input$x1], highlight_x[,input$y1], pch=21, col=rgb(1,0,0,0.25), bg=rgb(1,0,0,0.25))
          }
          
          else if (input$y1cursor){
            y1Range_Min <- input$y1Range[1]
            abline(h=y1Range_Min)
            
            y1Range_Max <- input$y1Range[2]
            abline(h=y1Range_Max)
            
            highlight_y <- nonCTCs[nonCTCs[,input$y1]>y1Range_Min & nonCTCs[,input$y1]<y1Range_Max,]
            
            points(highlight_y[,input$x1], highlight_y[,input$y1], pch=21, col=rgb(1,0,0,0.25), bg=rgb(1,0,0,0.25))
          }
          
          
          })

})