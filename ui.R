shinyUI(fluidPage(

     titlePanel("CTC App"),

     sidebarLayout(

          sidebarPanel(

               fileInput("data", label="Data file"),

               hr(),

               h3("Filter 1"),

               selectInput(inputId="x1",
                           label = h4("X axis"),
                           choices = list("None", "Red", "Green", "Blue"),
                           selected = "None"),

               fluidRow(
                    column(3,
                           radioButtons("x1Tail", label = "Tail",
                                        choices = list("Left" = 1, "Right" = 2),selected = 1)),
                    column(3,
                           radioButtons("x1Choice", label = "+/-",
                                        choices = list("+" = 1, "-" = 2),selected = 1)),
                    column(3,
                           radioButtons("x1Auto", label = "Threshold",
                                        choices = list("automatic" = 1, "manual" = 2),selected = 1)
                    )
               ),

               fluidRow(
                    conditionalPanel("input.x1Auto == 1",
                                     "print X thresholds"
                    ),

                    conditionalPanel("input.x1Auto == 2",
                                     # Then manual
                                     sliderInput("x1Range", "X",
                                                 min = 1, max = 1000, value = c(200,500))
                    )
               ),

               selectInput(inputId="y1",
                           label = h4("Y axis"),
                           choices = list("None", "Red", "Green", "Blue"),
                           selected = "None"),

               fluidRow(
                    column(3,
                           radioButtons("y1Tail", label = "Tail",
                                        choices = list("Left" = 1, "Right" = 2),selected = 1)),
                    column(3,
                           radioButtons("y1Choice", label = "+/-",
                                        choices = list("+" = 1, "-" = 2),selected = 1)),
                    column(3,
                           radioButtons("y1Auto", label = "Threshold",
                                        choices = list("automatic" = 1, "manual" = 2),selected = 1)
                    )
               ),

               fluidRow(
                    conditionalPanel("input.y1Auto == 1",
                                     "print Y thresholds"
                    ),

                    conditionalPanel("input.y1Auto == 2",
                                     # Then manual
                                     sliderInput("y1Range", "Y",
                                                 min = 1, max = 1000, value = c(200,500))
                    )
               )

          ),

          mainPanel(
               plotOutput("distPlot")
          )
     )

))