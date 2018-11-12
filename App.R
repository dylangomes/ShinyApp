library(shiny)
library(popbio)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Stage Modelling"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Offspring",
                  label = "Number of offspring:",
                  min = 0,
                  max = 12,
                  value = 5),
      sliderInput(inputId = "egg2hatch",
                  label = "Egg hatch success:",
                  min = 0,
                  max = 1,
                  value = .2),
      sliderInput(inputId = "hatch2fledge",
                  label = "Hatchling survival:",
                  min = 0,
                  max = 1,
                  value = .3),
      sliderInput(inputId = "fledge2adult",
                  label = "Fledgling survival:",
                  min = 0,
                  max = 1,
                  value = .67),
      sliderInput(inputId = "adultsurv",
                  label = "Adult survival:",
                  min = 0,
                  max = 1,
                  value = .85)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Matplot ----
      fluidRow(
        splitLayout(cellWidths = c("70%", "30%"), plotOutput(outputId = "matPlot"), plotOutput(outputId = "Elas")))

    )
  )
)




# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$matPlot <- renderPlot({
    
    x    <- 0:12
    
    bird<-matrix(c(0,0,0, input$Offspring,
                   input$egg2hatch,0,0,0,
                   0,input$hatch2fledge,0,0,
                   0,0,input$fledge2adult,input$adultsurv),nrow=4,ncol=4,byrow=T)
    
    #now we will start with an initial population vector:
    initial.vec<-c(100,20,10,5) #100 eggs, 20 fledglings, 10 young adults and 5 adults
    
    timestep<-100
    N<-matrix(0,nrow=length(bird[1,]),ncol=timestep)
    N[,1]<-initial.vec
    
    #now for the for loop.  We want to multiply the population vector by the matrix for each year of the simulation
    for (i in 2:timestep) 
    {N[,i]<-bird%*%N[,i-1]}
    
    
    matplot(t(N),type="l",lty=1,lwd=3,col=1:4,xlab="Time",ylab="Population size")
    legend(40, max(N), c("Eggs","Hatchlings","Fledglings","Adults"), lty=1, lwd=3, col=1:4, cex=1.2)
    
    
  })
  
  output$Elas <- renderPlot({
    x    <- 0:12
    bird<-matrix(c(0,0,0, input$Offspring,
                   input$egg2hatch,0,0,0,
                   0,input$hatch2fledge,0,0,
                   0,0,input$fledge2adult,input$adultsurv),nrow=4,ncol=4,byrow=T)
    image2(elasticity(bird),col=rev(heat.colors(10)))
  })

  
  
}



shinyApp(ui = ui, server = server)