shinyUI(fluidPage(

     titlePanel("CTC App"),

     sidebarLayout(

          sidebarPanel(

               width=5,

               fileInput("data", label="Data file"),

               hr(),

               h3("Filter 1"),

               uiOutput("x1Select"),

               fluidRow(
                    column(3,
                           radioButtons("x1Tail", label = "Tail",
                                        choices = list("Lower" = 1, "Upper" = 2),selected = 1)),
                    column(3,
                           radioButtons("x1Choice", label = "+/-",
                                        choices = list("+" = 1, "-" = 2),selected = 1)),
                    column(3,
                           radioButtons("x1Auto", label = "Threshold",
                                        choices = list("auto" = 1, "manual" = 2),selected = 1)),
                    column(3,
                           radioButtons("x1Log", label = "Scale",
                                        choices = list("log" = 1, "lin" = 2),selected = 1)
                    )
               ),

               fluidRow(
                    textOutput("x1Threshold"),

                    conditionalPanel("input.x1Auto == 2",
                                     # Then manual
                                     uiOutput("x1Slider")
                    )
               ),

               hr(),

               uiOutput("y1Select"),

               fluidRow(
                    column(3,
                           radioButtons("y1Tail", label = "Tail",
                                        choices = list("Lower" = 1, "Upper" = 2),selected = 1)),
                    column(3,
                           radioButtons("y1Choice", label = "+/-",
                                        choices = list("+" = 1, "-" = 2),selected = 1)),
                    column(3,
                           radioButtons("y1Auto", label = "Threshold",
                                        choices = list("auto" = 1, "manual" = 2),selected = 1)),
                    column(3,
                           radioButtons("y1Log", label = "Scale",
                                        choices = list("log" = 1, "lin" = 2),selected = 1)
                    )
               ),

               fluidRow(
                    textOutput("y1Threshold"),

                    conditionalPanel("input.y1Auto == 2",
                                     # Then manual
                                     uiOutput("y1Slider")
                    )
               )

          ),

          mainPanel(
               width=7,
               plotOutput("plot1")
          )
     )

))