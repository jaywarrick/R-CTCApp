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
    
       sliderInput("x1Range","X",
                   min = 1,
                   max = 50,
                   value = c(10, 40)),
      
      checkboxInput("y1cursor", "Show y cursors", FALSE),
  
       sliderInput("y1Range", "Y",
                   min = 101, 
                   max = 105, 
                   value = c(101,107))
   
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))