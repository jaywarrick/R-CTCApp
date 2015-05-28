rm(list=ls())
source ('/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/CTCApp_HelperFunctions.R')
file <- "/Users/jaywarrick/Public/DropBox/GitHub/R-CTCApp/DynamicThreshold/Colocalization - x0_y0.arff"
theTable <- data.frame(reorganizeTable(read.arff(file)))
filter <- Filter$new("My Filter")
theRandoms <- runif(nrow(theTable), min=1.5, max=2.5)
filter$setData(newTable=theTable, newRandoms=theRandoms)
filter$setParams(newName="Sig_12_Tot", newTail="1", newChoice="1", newAuto="2", newRange=c(1e-3,2e6), newLog="1", newNSigma=3)
duh <- getPlot(filterX=filter, filterY=filter)
print(getSliderUI(name='x1Range', axis='x', filter=filter))

