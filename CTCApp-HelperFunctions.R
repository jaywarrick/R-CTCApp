print("Running the helper functions file")

reorganizeTable <- function(data, baseName=NA, convertToNumeric=TRUE)
{
     library(plyr)
     idCols <- names(data)
     idCols <- idCols[-which(idCols %in% c('Measurement','Value'))]
     newData <- data.frame(stringsAsFactors=FALSE)
     measurements <- unique(data$Measurement)
     #     m <- measurements[1]
     #     browser()
     for(m in measurements)
     {
          if(is.na(baseName))
          {
               newColName <- m
               newColName <- gsub(' ','.',newColName, fixed=TRUE) # Get rid of extraneous spaces
          }else
          {
               newColName <- paste(baseName,'.',m, sep='')
               newColName <- gsub(' ','.',newColName, fixed=TRUE) # Get rid of extraneous spaces
          }

          temp <- subset(data, Measurement==m)
          temp2 <- temp[,c(idCols,'Value')]
          names(temp2)[names(temp2)=="Value"] <- newColName
          if(nrow(newData) == 0)
          {
               newData <- temp2
          }else
          {
               newData <- merge(newData, temp2, by=idCols)
          }
     }

     if(convertToNumeric)
     {
          for(n in idCols)
          {
               newData[,n] <- as.numeric(as.character(newData[,n]))
          }
     }

     return(newData)
}

s <- function(...)
{
     paste0(...)
}

getMads <- function(data)
{
     temp <- data.table(data)
     return(data.frame(temp[,lapply(.SD, mad, na.rm=T), by=NULL]))
}

getLogMads <- function(data)
{
     temp <- data.table(data)
     return(data.frame(temp[,lapply(log10(.SD), mad, na.rm=T), by=NULL]))
}

translateNames <- function(data, newNames=list('395'='Nuc','485'='Cyt','560'='EpCAM','647'='AR','750'='GR'))
{
     temp <- names(data)
     for(name in names(newNames))
     {
          temp <- sub(name, newNames[name], temp)
     }
     names(data) <- temp
     return(data)
}

getAutoThresh <- function(filterNumber, table, column, tail, log, mads, logMads)
{
     if(is.null(column) || (column == 'None'))
     {
          0
     }
     else
     {
          if(tail==2)
          {
               if(log == 1)
               {
                    as.numeric(10^(log10(median(table[,column], na.rm=T)) + 3*logMads[column]))
               }
               else
               {
                    as.numeric(median(table[,column], na.rm=T) + 3*mads[column])
               }
          }
          else
          {
               if(log == 1)
               {
                    as.numeric(10^(log10(median(table[,column], na.rm=T)) - 3*logMads[column]))
               }
               else
               {
                    as.numeric(median(table[,column], na.rm=T) - 3*mads[column])
               }
          }
     }
}

getGood <- function(filterNumber, table, x, y, choiceX, choiceY, autoX, autoY, threshX, threshY, logX, logY, rangeX, rangeY)
{
     # Grab either the + or minus population based on user choice and call it good and the other bad
     if(is.null(x) || x == 'None')
     {
          goodX <- rep(T, nrow(table))
     }
     else if(autoX==1)
     {
          goodX <- (table[,x] >= threshX) == (choiceX == 1)
     }
     else
     {
          if(logX == 1)
          {
               goodX <- ((table[,x] >= 10^rangeX[1]) & (table[,x] <= 10^rangeX[2]))  == (choiceX == 1)
          }
          else
          {
               goodX <- ((table[,x] >= rangeX[1]) & (table[,x] <= rangeX[2]))  == (choiceX == 1)
          }
     }
     if(is.null(y) || y == 'None')
     {
          goodY <- rep(T, nrow(table))
     }
     else if(autoY==1)
     {
          goodY <- (table[,y] >= threshY) == (choiceY == 1)
     }
     else
     {
          if(logY == 1)
          {
               goodY <- ((table[,y] >= 10^rangeY[1]) & (table[,y] <= 10^rangeY[2]))  == (choiceY == 1)
          }
          else
          {
               goodY <- ((table[,y] >= rangeY[1]) & (table[,y] <= rangeY[2]))  == (choiceY == 1)
          }
     }
     goodX & goodY
}

getThreshText = function(column, thresh, auto, log, range)
{
     if(is.null(column) || column == 'None')
     {
          ''
     }
     else if(auto == 1)
     {
          if(log == 1)
          {
               temp <- format(thresh, digits=3, scientific=TRUE)
          }
          else
          {
               temp <- format(round(thresh, 1), nsmall = 1)
          }
          txt <- paste(temp, collapse='')
          paste("Threshold: ", txt, sep='')
     }
     else
     {
          if(log == 1)
          {
               temp <- format(10^range, digits=3, scientific=TRUE)
          }
          else
          {
               temp <- format(round(range, 1), nsmall = 1)
          }
          txt <- paste(temp, collapse=' - ')
          paste("Thresholds: ", txt, sep='')
     }
}

getLogParam <- function(logX, logY)
{
     if(logX == 1 & logY == 1)
     {
          'xy'
     }
     else if(logX == 1)
     {
          'x'
     }
     else if(logY == 1)
     {
          'y'
     }
     else
     {
          ''
     }
}

getPlot <- function(table, x, y, goodOld, goodNew, logX, logY, randoms, autoX, autoY, threshX, threshY, rangeX, rangeY, Id=NULL, stateTable=NULL)
{
     if((is.null(x) && is.null(y)) || ((x == 'None') && (y == 'None')))
     {
          # Do nothing
          NULL
     }
     else
     {
          bad <- !(goodOld | goodNew)

          # Make the main plot
          if(is.null(x) || x == 'None')
          {
               xlim <- c(1,3)
               x1 <- randoms
          }
          else
          {
               x1 <- table[,x]
               if(logX == 1)
               {
                    xlim <- logicle(range(x1))
                    x1 <- logicle(x1)
               }
               else
               {
                    xlim <- range(x1)
               }
          }
          if(is.null(y) || y == 'None')
          {
               ylim <- c(1,3)
               y1 <- randoms
          }
          else
          {
               y1 <- table[,y]
               if(logY == 1)
               {
                    ylim <- logicle(range(y1))
                    y1 <- logicle(y1)
               }
               else
               {
                    ylim <- range(y1)
               }
          }
          plot(c(),c(), xlim=xlim, ylim=ylim, xlab=x, ylab=y, axes=FALSE, log=getLogParam(logX, logY))
          box(col='black',lwd=2)
          #plot(x1[bad], y1[bad], pch=20, col=rgb(0,0,1,0.25), bg=rgb(0,0,1,0.25), xlim=xlim, ylim=ylim, xlab=x, ylab=y, log=)
          points(x1[bad], y1[bad], pch=20, col=rgb(0,0,1,0.25), bg=rgb(0,0,1,0.25))
          points(x1[goodOld], y1[goodOld], pch=20, col=rgb(1,0,0,0.25), bg=rgb(1,0,0,0.25))
          points(x1[goodNew], y1[goodNew], pch=20, col=rgb(0,1,0,0.25), bg=rgb(0,1,0,0.25))
          if(!is.null(Id))
          {
               points(x1[table$Id==Id], y1[table$Id==Id], pch=10, cex=2, col='black')
          }
          if(!is.null(stateTable) && nrow(stateTable) > 0)
          {
               points(x1[stateTable$No.Maybe.Yes==2], y1[stateTable$No.Maybe.Yes==2], pch=4, cex=1, col='black')
               points(x1[stateTable$No.Maybe.Yes==1], y1[stateTable$No.Maybe.Yes==1], pch=1, cex=1, col='black')
          }

          # Draw axes
          if(logX == 1)
          {
               drawLogicleAxis(axisNum=1)
          }
          else
          {
               axis(1)
          }
          if(logY == 1)
          {
               drawLogicleAxis(axisNum=2)
          }
          else
          {
               axis(2)
          }

          # Plot threshold lines
          if(!is.null(autoX))
          {
               if(autoX == 1)
               {
                    if(logX == 1)
                    {
                         abline(v=threshX)
                    }
                    else
                    {
                         abline(v=logicle(threshX))
                    }
               }
               else
               {
                    if(logX == 1)
                    {
                         abline(v=10^rangeX)
                    }
                    else
                    {
                         abline(v=rangeX)
                    }
               }
          }
          if(!is.null(autoY))
          {
               if(autoY == 1)
               {
                    if(logY == 1)
                    {
                         abline(h=threshY)
                    }
                    else
                    {
                         abline(h=logicle(threshY))
                    }
               }
               else
               {
                    if(logY == 1)
                    {
                         abline(h=logicle(10^rangeY))
                    }
                    else
                    {
                         abline(h=rangeY)
                    }
               }
          }
     }
}

logicle <- function(x, transitionPoint=1, linearUnitsPerOrder=100)
{
     linearDifference = x - transitionPoint;
     valsToAdjust <- linearDifference <= 0;
     ordersDifferenceOnDisplay = linearDifference / linearUnitsPerOrder;
     linearDifference[valsToAdjust] <- transitionPoint * 10^(ordersDifferenceOnDisplay[valsToAdjust]);
     x[valsToAdjust] <- transitionPoint*10^(-1*((transitionPoint-x[valsToAdjust])/linearUnitsPerOrder))

     return(x)
}

unlogicle <- function(x, transitionPoint=1, linearUnitsPerOrder=100)
{
     # Just do it for the right indicies
     valsToAdjust <- x <= transitionPoint
     ret <- x
     ret[valsToAdjust] <- transitionPoint + log10(x[valsToAdjust]/transitionPoint)*linearUnitsPerOrder
     return(ret)
}

drawLogicleAxis <- function(axisNum=1, transition=1, linLogRatio=100)
{
     linNums=c(-500,-400,-300,-200,-100,-50,0)
     logNums=c(1,10,100,1000,10000,100000,1000000)

     prettyNums <- c(linNums, logNums)

     logSidePrettyLabels <- parse(text=paste("10^", log10(logNums), sep=""))
     prettyLabels <- c(as.character(linNums), logSidePrettyLabels)
     ticks <- logicle(prettyNums, transition, linLogRatio)
     if(axisNum == 2)
     {
          axis(axisNum, at=ticks, labels=prettyLabels, las=2)
     }
     else
     {
          axis(axisNum, at=ticks, labels=prettyLabels)
     }

}

getFilterUI <- function(i)
{
     list(hr(),

          h3(s('Filter ', i)),

          uiOutput(s('x',i,'Select')),

          fluidRow(
               column(3,
                      radioButtons(s('x',i,'Tail'), label = "Tail",
                                   choices = list("Lower" = 1, "Upper" = 2),selected = 1)),
               column(3,
                      radioButtons(s('x',i,'Choice'), label = "+/-",
                                   choices = list("+" = 1, "-" = 2),selected = 1)),
               column(3,
                      radioButtons(s('x',i,'Auto'), label = "Threshold",
                                   choices = list("auto" = 1, "manual" = 2),selected = 1)),
               column(3,
                      radioButtons(s('x',i,'Log'), label = "Scale",
                                   choices = list("log" = 1, "lin" = 2),selected = 1)
               )
          ),

          fluidRow(
               textOutput(s('x',i,'Threshold')),

               conditionalPanel(s('input.x',i,'Auto == 2'),
                                # Then manual
                                uiOutput(s('x',i,'Slider'))
               )
          ),

          hr(),

          uiOutput(s('y',i,'Select')),

          fluidRow(
               column(3,
                      radioButtons(s('y',i,'Tail'), label = "Tail",
                                   choices = list("Lower" = 1, "Upper" = 2),selected = 1)),
               column(3,
                      radioButtons(s('y',i,'Choice'), label = "+/-",
                                   choices = list("+" = 1, "-" = 2),selected = 1)),
               column(3,
                      radioButtons(s('y',i,'Auto'), label = "Threshold",
                                   choices = list("auto" = 1, "manual" = 2),selected = 1)),
               column(3,
                      radioButtons(s('y',i,'Log'), label = "Scale",
                                   choices = list("log" = 1, "lin" = 2),selected = 1)
               )
          ),

          fluidRow(
               textOutput(s('y',i,'Threshold')),

               conditionalPanel(s('input.y',i,'Auto == 2'),
                                # Then manual
                                uiOutput(s('y',i,'Slider'))
               )
          )
     )
}

getFinalPlotUI <- function(i)
{
     list(hr(),

          h3(s('Final Plot', i-3)),

          uiOutput(paste0('x',i,'Select')),

          fluidRow(
               #                column(3,
               #                       radioButtons(s('x',i,'Tail'), label = "Tail",
               #                                    choices = list("Lower" = 1, "Upper" = 2),selected = 1)),
               #                column(3,
               #                       radioButtons(s('x',i,'Choice'), label = "+/-",
               #                                    choices = list("+" = 1, "-" = 2),selected = 1)),
               #                column(3,
               #                       radioButtons(s('x',i,'Auto'), label = "Threshold",
               #                                    choices = list("auto" = 1, "manual" = 2),selected = 1)),
               column(3,
                      radioButtons(s('x',i,'Log'), label = "Scale",
                                   choices = list("log" = 1, "lin" = 2),selected = 1)
               )
          ),

          #           fluidRow(
          #                textOutput(s('x',i,'Threshold')),
          #
          #                conditionalPanel(s('input.x',i,'Auto == 2'),
          #                                 # Then manual
          #                                 uiOutput(s('x',i,'Slider'))
          #                )
          #           ),

          hr(),

          uiOutput(paste0('y',i,'Select')),

          fluidRow(
               #                column(3,
               #                       radioButtons(s('y',i,'Tail'), label = "Tail",
               #                                    choices = list("Lower" = 1, "Upper" = 2),selected = 1)),
               #                column(3,
               #                       radioButtons(s('y',i,'Choice'), label = "+/-",
               #                                    choices = list("+" = 1, "-" = 2),selected = 1)),
               #                column(3,
               #                       radioButtons(s('y',i,'Auto'), label = "Threshold",
               #                                    choices = list("auto" = 1, "manual" = 2),selected = 1)),
               column(3,
                      radioButtons(s('y',i,'Log'), label = "Scale",
                                   choices = list("log" = 1, "lin" = 2),selected = 1)
               )
          )

          #           fluidRow(
          #                textOutput(s('y',i,'Threshold')),
          #
          #                conditionalPanel(s('input.y',i,'Auto == 2'),
          #                                 # Then manual
          #                                 uiOutput(s('y',i,'Slider'))
          #                )
          #           )
     )
}

getSliderUI <- function(name, axis='x', table, column, auto, log)
{
     if (!is.null(column)  && column != 'None' && auto==2)
     {
          nums <- table[,column]
          if(log == 1)
          {
               nums <- logicle(nums)
               temp <- range(nums)
               temp <- log10(temp)
               sliderLabel <- s('log10(', axis, ')')
          }
          else
          {
               temp <- range(nums)
               sliderLabel <- axis
          }

          myMin <- .99*temp[1]
          myMax <- 1.01*temp[2]
          sliderInput(inputId=name,label=sliderLabel, min=myMin, max=myMax, value=c(myMin,myMax))
     }
}

##%%%%%%%%%%%%%%%%%%%%

frac <- function(x)
{
     return(length(which(x))/length(x))
}

fracLinesTot <- function(frac)
{
     col <- 'black'
     if(frac == 0.5)
     {
          col <- 'red'
     }
     #     ratio <- frac / (1-frac)
     x <- 0:100000
     x[1] <- 0.00001
     y <- (1/frac)*x
     lines(x,y, col=col)
}

fracLinesCyt <- function(frac)
{
     col <- 'black'
     if(frac == 0.5)
     {
          col <- 'red'
     }
     #     ratio <- frac / (1-frac)
     x <- 0:100000
     x[1] <- 0.00001
     y <- (1/frac - 1)*x
     lines(x,y, col=col)

}

totLinesCyt <- function(max)
{
     x <- (max/100)*0:100
     y <- max*sin((pi/2)*((100:0)/100))
     x[1] <- x[1] + 0.0001
     y[length(y)] <- y[length(y)] + 0.0001
     lines(x, y)
}

totLinesTot <- function(max)
{
     abline(h=max)
}

calculateSummaryVariables <- function(locResults, dapiColor, ckColor, cd45Color, arColor)
{
     locResults$Id <- as.numeric(as.character(locResults$Id))
     locResults <- locResults[with(locResults, order(Id)),]

     dapiTot <- paste('Sig_', dapiColor, ckColor, '_Tot', sep ='')
     ckTot <- paste('Sig_', ckColor, dapiColor, '_Tot', sep ='')
     cd45Tot <- paste('Sig_', cd45Color, dapiColor, '_Tot', sep ='')
     arTot <- paste('Sig_', arColor, dapiColor, '_Tot', sep ='')
     arWindow <- paste('Sig_', arColor, dapiColor, '_Window', sep ='')
     locResults$DAPI <- locResults[,dapiTot]/locResults$n
     locResults$CK <- locResults[,ckTot]/locResults$n
     locResults$CD45 <- locResults[,cd45Tot]/locResults$n
     locResults$AR <- locResults[,arTot]/locResults$n
     locResults$AR.Nuc <- locResults[,arWindow]/locResults$n
     locResults$AR.Cyt <- (locResults[,arTot]-locResults[,arWindow])/locResults$n

     locResults$AR.NucPer <- locResults$AR.Nuc/locResults$AR
     locResults$AR.NucPerFlag <- locResults$AR.NucPer > 0.5

     return(locResults = locResults)
}

calculateSummaryVariables_Threshold <- function(locResults, CKThresh_SD, DAPIThresh_SD, CD45Thresh_SD, ARThresh_SD)
{
     CKThresh <- exp(median(log(locResults$CK)) + CKThresh_SD*mad(log(locResults$CK)))
     DAPIThresh <- exp(median(log(locResults$DAPI)) + DAPIThresh_SD*mad(log(locResults$DAPI)))
     CD45Thresh <- exp(median(log(locResults$CD45)) + CD45Thresh_SD*mad(log(locResults$CD45)))
     ARThresh <- exp(median(log(locResults$AR)) + ARThresh_SD*mad(log(locResults$AR)))

     locResults$DAPI.Flag <- locResults$DAPI >= DAPIThresh
     locResults$CK.Flag <- locResults$CK >= CKThresh
     locResults$CD45.Flag <- locResults$CD45 >= CD45Thresh
     locResults$AR.Flag <- locResults$AR >= ARThresh

     summary <- locResults[,c('Id','DAPI','CK','CD45','AR','AR.Nuc','AR.Cyt','AR.NucPer','AR.NucPerFlag','DAPI.Flag','CK.Flag','CD45.Flag','AR.Flag')]

     return(list(thresholds=list(CK=CKThresh, DAPI=DAPIThresh, CD45=CD45Thresh, AR=ARThresh), summary=summary))
}

fileChoose <- function(...) {
     pathname <- NULL;
     tryCatch({
          pathname <- file.choose();
     }, error = function(ex) {
     })
     pathname;
}

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

save.xlsx <- function (file, ...)
{
     require(xlsx, quietly = TRUE)
     if(!is.null(file))
     {
          objects <- list(...)
          fargs <- as.list(match.call(expand.dots = TRUE))
          objnames <- as.character(fargs)[-c(1, 2)]
          nobjects <- length(objects)
          for (i in 1:nobjects) {
               if (i == 1)
                    write.xlsx(objects[[i]], file, sheetName = objnames[i])
               else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                               append = TRUE)
          }
          print(paste("Workbook", file, "has", nobjects, "worksheets."))
     }
     else
     {
          print("Couldn't save. No file specified.")
     }
}

runCTCApp <- function()
{
     dataPath <- fileChoose()
     shinyApp(ui=getUI, server=getServer(dataPath))
}