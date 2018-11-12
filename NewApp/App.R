 library(shiny)


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
                  value = 5)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Matplot ----
      plotOutput(outputId = "matPlot")
      
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
    # bins <- seq(min(x), max(x), length.out = 1)
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
    bird<-matrix(c(0,0,0, x,
                   0.2,0,0,0,
                   0,0.3,0,0,
                   0,0,.6,0.85),nrow=4,ncol=4,byrow=T)
    
    #now we will start with an initial population vector:
    initial.vec<-c(100,20,10,5)

    #100 eggs, 20 fledglings, 10 young adults and 5 adults
    
    timestep<-100

    N<-matrix(0,nrow=length(bird[1,]),ncol=timestep)
    
    N[,1]<-initial.vec
    
    #now for the for loop.  We want to multiply the population vector by the matrix for each year of the simulation
    for (i in 2:timestep) 
    {N[,i]<-bird%*%N[,i-1]}
    
    
    matplot(t(N),type="l",lty=1,lwd=3,col=1:4,xlab="Time",ylab="Population size")
    
  })
  
}



shinyApp(ui = ui, server = server)









library(shiny)

# Doesn't work
out1 <- vector("list", 3)
for(i in 1:3) {
  out[[i]] <- reactive(i)
}
isolate(out1[[1]]())

# Still doesn't work
out2 <- lapply(1:3, function(i) reactive(i))
isolate(out2[[1]]())

# Works!
out3 <- lapply(1:3, function(i) {
  force(i)
  reactive(i)
})
isolate(out3[[1]]())


https://code.i-harness.com/en/q/1eddf60

https://www.rstudio.com/resources/webinars/shiny-developer-conference/
  
  https://stackoverflow.com/questions/46118166/for-loop-inside-reactive-function-in-shiny