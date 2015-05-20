library(shiny)

# Define UI for application that draws a histogram
getUI <- function()
{
     shinyUI(fluidPage(

          # Application title
          titlePanel("Hello Shiny!"),

          # Sidebar with a slider input for the number of bins
          sidebarLayout(
               sidebarPanel(
                    actionButton("closeButton", "Click HERE to close app properly!"),
                    sliderInput("n", "Number of plots", value=0, min=0, max=6), width=4
               ),

               # Show a plot of the generated distribution
               mainPanel(width=8,
                         fluidRow(column(width=12,plotOutput("montage1"))),
                         uiOutput('adjusters1'),
                         fluidRow(column(width=12,plotOutput("montage2"))),
                         uiOutput('adjusters2')
               )
          )
     ))
}