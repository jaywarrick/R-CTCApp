library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("CTC App"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="x1",
                  label = h4("X axis"),
                  choices = list("AR", "AR.Nuc", "AR.Cyt", "AR.NucPer"),
                  selected = "AR.Nuc"),
      
      selectInput(inputId="y1",
                  label = h4("y axis"),
                  choices = list("AR", "AR.Nuc", "AR.Cyt", "AR.NucPer"),
                  selected = "AR"),
      
      checkboxInput("x1cursor", "Show x cursors", FALSE),  
      
      uiOutput("xSlider"),
      
      checkboxInput("y1cursor", "Show y cursors", FALSE),
    
      uiOutput("ySlider"),
      
      sliderInput("CK_SD", "CK Standard Deviation",
                  min = 1, max = 4, value = 3),
      
      sliderInput("DPAI_SD", "DPAI Standard Deviation",
                  min = 1, max = 4, value = 3),
      
      sliderInput("CD45_SD", "CD45 Standard Deviation",
                  min = 1, max = 4, value = 3),
      
      sliderInput("AR_SD", "AR Standard Deviation",
                  min = 1, max = 4, value = 3)
   
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))