shinyUI(fluidPage(

     titlePanel("CTC App"),

     sidebarLayout(

          sidebarPanel(

               width=5,

               actionButton("closeButton", "Click HERE to CLOSE app properly!"),

               fileInput("data", label="Data file"),

               getFilterUI(1),

               getFilterUI(2),

               getFilterUI(3),

               getFinalPlotUI(4),

               hr(),

               downloadButton('downloadReport')

          ),

          mainPanel(
               width=7,
               br(),
               br(),
               br(),
               plotOutput("plot1"),
               br(),
               br(),
               br(),
               br(),
               br(),
               plotOutput("plot2"),
               br(),
               br(),
               br(),
               br(),
               br(),
               plotOutput("plot3"),
               br(),
               plotOutput("plot4")
          )
     )

))