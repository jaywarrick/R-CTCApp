shinyUI(fluidPage(

     titlePanel("CTC App"),

     sidebarLayout(

          sidebarPanel(

               width=5,

               fluidRow(column(width=12, actionButton("closeButton", "Click HERE to CLOSE app properly!"))),

               br(),

               fluidRow(column(width=5, actionButton("dataButton", as.character(tags$small("Choose Data...")))), column(width=7, tags$small(textOutput("filePath")))),

               getFilterUI(1),

               getFilterUI(2),

               getFilterUI(3),

               getFinalPlotUI(4),

               hr(),

               uiOutput("cellIndexer"),

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

               hr(),

               fluidRow(
                    column(width=12, textInput("reportName", label = "Report Name (no file extension)", value = "CTC Report")),
                    column(width=12, actionButton("saveButton", "Save Report and Data"))
               ),

               hr(),

               fluidRow(column(width=12, actionButton("closeButton2", "Click HERE to CLOSE app properly!")))

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
               plotOutput("plot4"),
               hr(),
               fluidRow(column(width=12,plotOutput("montage1"))),
               uiOutput('adjusters1'),
               fluidRow(column(width=12,plotOutput("montage2"))),
               uiOutput('adjusters2')
          )
     )

))