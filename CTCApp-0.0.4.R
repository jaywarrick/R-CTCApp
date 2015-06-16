rm(list=ls())
library(shiny)
library(foreign)
library(data.table)
library(EBImage)
library(gridGraphics)
library(foreign)
library(xlsx)

setwd('~/../Desktop/CTCIAP-0.0.5')
source('CTCApp-HelperFunctions.R')
source('server.R')
source('ui.R')

# Now user can type "runCTCApp()" and the app will prompt the user to choose a file and run the app.