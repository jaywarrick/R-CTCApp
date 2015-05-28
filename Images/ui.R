library(shiny)

# Define UI for application that draws a histogram
# getUI <- function()
# {
     shinyUI(fluidPage(

          # Application title
          titlePanel("Hello Shiny!"),

          # Sidebar with a slider input for the number of bins
          sidebarLayout(
               sidebarPanel(
                    actionButton("closeButton", "Click HERE to close app properly!"),
                    #########
                    ##########
                    # Change from Id to index
                    ##########
                    ##########
                    fluidRow(
                         column(
                              width=12, align="center",
                              sliderInput("index", "Cell Id", value=1, min=1, step=1, max=length(unique(cropsTable$Id)), width="100%")
                         )
                    ),

                    fluidRow(
                         column(width=6, align="left", actionButton("prevButton", "Prev.")),
                         column(width=6, align="right", actionButton("nextButton", "Next"))
                    ),

                    br(),

                    fluidRow(
                         column(width=4, align="left", actionButton("noButton", "No")),
                         column(width=4, align="center", actionButton("maybeButton", "Maybe")),
                         column(width=4, align="right", actionButton("yesButton", "Yes"))
                    ),

                    uiOutput("state"),

                    width=5
               ),

               # Show a plot of the generated distribution
               mainPanel(
                    #                          textOutput("txt")
                    fluidRow(column(width=12,plotOutput("montage1"))),
                    uiOutput('adjusters1'),
                    fluidRow(column(width=12,plotOutput("montage2"))),
                    uiOutput('adjusters2'),
                    width=7
               )
          )
     ))
# }