
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
          goodX <- rep(T, length(nrow(table)))
     }
     else if(autoX==1)
     {
          goodX <- (table[,x] > threshX) == (choiceX == 1)
     }
     else
     {
          if(logX == 1)
          {
               goodX <- ((table[,x] > 10^rangeX[1]) & (table[,x] < 10^rangeX[2]))  == (choiceX == 1)
          }
          else
          {
               goodX <- ((table[,x] > rangeX[1]) & (table[,x] < rangeX[2]))  == (choiceX == 1)
          }
     }
     if(is.null(y) || y == 'None')
     {
          goodY <- rep(T, length(nrow(table)))
     }
     else if(autoY==1)
     {
          goodY <- (table[,y] > threshY) == (choiceY == 1)
     }
     else
     {
          if(logY == 1)
          {
               goodY <- ((table[,y] > 10^rangeY[1]) & (table[,y] < 10^rangeY[2]))  == (choiceY == 1)
          }
          else
          {
               goodY <- ((table[,y] > rangeY[1]) & (table[,y] < rangeY[2]))  == (choiceY == 1)
          }
     }
     goodX & goodY
}

getThreshText <- function(column, thresh, auto, log, range)
{
     if(is.null(column) || column == 'None')
     {
          ''
     }
     else if(auto == 1)
     {
          nums <- format(round(thresh, 1), nsmall = 1)
          txt <- paste(nums, collapse='')
          paste("Threshold: ", txt, sep='')
     }
     else
     {
          if(log == 1)
          {
               nums <- format(round(10^range, 1), nsmall = 1)
          }
          else
          {
               nums <- format(round(range, 1), nsmall = 1)
          }
          txt <- paste(nums, collapse=' - ')
          paste("Thresholds: ", txt, sep='')
          txt <- paste(nums, collapse=' - ')
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

getPlot <- function(table, x, y, good, logX, logY, randoms, autoX, autoY, threshX, threshY, rangeX, rangeY)
{
     if((is.null(x) && is.null(y)) || ((x == 'None') && (y == 'None')))
     {
          # Do nothing
          NULL
     }
     else
     {
          bad <- !good

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
                    xlim <- range(x1[x1 > 0])
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
                    ylim <- range(y1[y1 > 0])
               }
               else
               {
                    ylim <- range(y1)
               }
          }
          plot(x1[bad], y1[bad], pch=20, col=rgb(0,0,0,0.25), bg=rgb(0,0,0,0.25), xlim=xlim, ylim=ylim, xlab=x, ylab=y, log=getLogParam(logX, logY))
          points(x1[good], y1[good], pch=20, col=rgb(1,0,0,0.25), bg=rgb(1,0,0,0.25))

          # Plot threshold lines
          if(autoX == 1)
          {
               abline(v=threshX)
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
          if(autoY == 1)
          {
               abline(h=threshY)
          }
          else
          {
               if(logY == 1)
               {
                    abline(h=10^rangeY)
               }
               else
               {
                    abline(h=rangeY)
               }
          }
     }
}

getSliderUI <- function(name, axis='x', table, column, auto, log)
{
     if (!is.null(column)  && column != 'None' && auto==2)
     {
          nums <- table[,column]
          if(log == 1)
          {
               temp <- range(nums[nums > 0])
               temp <- log10(temp)
               sliderLabel <- s('log10(', axis, ')')
          }
          else
          {
               temp <- range(nums)
               sliderLabel <- axis
          }
          sliderInput(inputId=name,label=sliderLabel, min=temp[1], max=temp[2], value=c(temp[1],temp[2]))
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
