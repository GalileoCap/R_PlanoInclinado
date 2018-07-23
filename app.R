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
      g <- 9.8 #Constante de la gravedad
      angrad <- (input$ang*pi)/180  #Paso los grados qe la usuario me pidió, a radianes
      m <- (sin(angrad)*g)/input$kg #Calculo la aceleración de la caída
      b <- sin(angrad)*input$dist #Calculo la altura inicial
      tiempo <- (input$dist*2/m)^0.5 #Calculo el tiempo qe tarda en llegar al piso
      cayo <- cos(angrad)*input$dist #Calculo la base sobre la qe está puesta la rampa

      curve(-m*x^2+b,
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

