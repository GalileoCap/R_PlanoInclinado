# INFO: Simular un plano inclinado
# El angulo del plano con la horizontal es alfa [grados]
# La aceleracion de la gravedad es g [m/s^2]
# La aceleracion en la direccion del plano es ag = g * sin(alfa)
# La velocidad en el momento t es v(t) = v0 + ag * t
# La posicion en el momento t es X(t) = X0 + g * sin(alfa) * 1/2 * t^2
# Porqe a = dv/dt = d(dx/dt)/dt
# O sea la aceleracion es la derivada de la velocidad, qe es la derivada de la posicion

library(shiny)

g <- 9.80665 #Constante de la gravedad

ui <- fluidPage(
   
   titlePanel("Caida en plano inclinado"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("alfa",
                     "Ángulo de inclinacion del plano:",
                     min = 1,
                     max = 90,
                     value = 30),
         sliderInput("larg",
                     "Largo de la caída",
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
     
      alfarad <- input$alfa*(pi/180)  #Paso los grados qe la usuario me pidió, a radianes
      ag <- g*sin(alfarad)
      tcuad <- (input$dist*2)/ag #Calculo el tiempo qe tarda en llegar al piso (separé el raizarlo por si no anda bien)
      t <- tcuad^0.5 #Hago la raiz para obtener el tiempo
      
      curve(g*sin(alfarad)*0.5*x^2,
            xlim = c(0, t+1), #Sumo 1segundo para qe el grafico qede mas lindo
            ylim = c(0, input$larg*1.1), #Agrego un 10% extra para qe el grafico qede mas lindo
            xlab = "Tiempo de caida en segundos",
            ylab = "Posicion sobre el plano",
            col = 'darkgray', 
            )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

