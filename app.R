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
      g <- 9.80665 #Constante de la gravedad
      fg <- input$kg*g #Fuerza ejercida en la bola por la gravedad

      angrad <- input$ang*(pi/180)  #Paso los grados qe la usuario me pidió, a radianes
      
      fc <- sin(angrad)*fg #Calculo la fuerza de la caída
      a <- fc/input$kg #Calculo la aceleración de la caída
      tcuad <- (input$dist*2)/a #Calculo el tiempo qe tarda en llegar al piso (separé el raizarlo por si no anda bien)
      t <- tcuad^0.5 #Finalizo el cálculo del tiempo
      
      b <- sin(angrad)*input$dist #Calculo la altura inicial
      cayo <- cos(angrad)*input$dist #Calculo la base sobre la qe está puesta la rampa

      curve(-a*x+b,
            xlim = c(0, t+1),
            ylim = c(0, b),
            xlab = "Tiempo de caida en segundos",
            ylab = "Altura en metros",
            col = 'darkgray', 
            )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

