library(shiny)

ui <- fluidPage(
   
   titlePanel("Caida en plano inclinado"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("ang",
                     "Ángulo de caída:",
                     min = 1,
                     max = 90,
                     value = 30),
         sliderInput("kg",
                     "Peso:",
                     min = 1,
                     max = 100,
                     value = 20),
         sliderInput("dist",
                     "Distancia de caída",
                     min = 1,
                     max = 100,
                     value = 50)
      ),
      
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      g <- 9.8
      angrad <- input$ang*pi/180
      m <- sin(angrad)*g/input$kg
      b <- sin(angrad)*input$dist
      tiempo <- (input$dist*2/m)^0.5
      cayo <- cos(angrad)*input$dist

      curve(-m*x+b,
            xlim = c(0, cayo),
            ylim = c(0, b),
            xlab = "Tiempo de caida en segundos",
            ylab = "Altura en metros",
            col = 'darkgray', 
            )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

