#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggmap)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rgeos")
library("rnaturalearth")
library("rnaturalearthdata")
library(shiny)

dat<-read.csv("https://raw.githubusercontent.com/dylangomes/ShinyApp/master/NWFSC_WCGBTS/FullDataInput.csv")
dat$Date<-sub("(..)$","20\\1",dat$Date) ## fill in date information
dat$DatStr<-substr(dat$Date,0,5)
dat$Date<-as.Date(dat$Date,format="%m/%d/%Y")
dat$yday<-as.POSIXlt(dat$Date)$yday

JS<-read.csv("https://raw.githubusercontent.com/dylangomes/ShinyApp/master/NWFSC_WCGBTS/Oceanographic%20Trawl%20Data%20-%20Ocean%20Trawl%20Catch%20Data%20Data.csv")
names(JS)
JS<-JS[,c(1,2,7,8,11:15)]
head(JS)
JS<-JS[which(!duplicated(JS)),]
JS$DatStr<-substr(as.character(JS$Sample.Date.Local),6,10)
JS$yday<-as.POSIXlt(JS$Sample.Date.Local, format="%Y-%m-%d")$yday

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NWFSC Groundfish Survey"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("daterange",
                        "Day of year:",
                        min = 137,
                        max = 296,
                        value = c(230,240))
        ,
        sliderInput("Lat",
                    "Latitude:",
                    min = 33,
                    max = 50,
                    value = c(42,48.5))
        ,
        column(12,
               selectInput("species",
                           "species:",
                           c(unique(dat$Common_name)))
        ),
        checkboxInput("check", "JSOES always on", value = T, width = NULL)
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot",  width = "100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        
        
        ## define species of interest
        name<-input$species
        dat2<-dat[dat$Common_name==name,]
        
        ## define lat long
        long=c(-127, -120)
        lati=input$Lat
        
        ## define time period
        min=input$daterange[1]
        max=input$daterange[2]
        days<-seq(from=min,to=max,by=1)
        
        ## JSOES data all or by date
        if(input$check==T){JSdat<-JS}else{JSdat<-JS[JS$yday%in%days,]}
        
        ## turn on later (classic look)
        # world <- ne_countries(scale = "medium", returnclass = "sf")
        # ggplot(data=world)+
        #   geom_sf() +
        #   geom_point(aes(x=Longitude_dd,y=Latitude_dd),data=dat)+
        #   coord_sf(xlim = long, ylim = lati, expand = FALSE)
        
        ggmap(
          get_map(
            location=make_bbox(
              lon=long,
              lat=lati,
              f=.1
            )
          )
        )+
          geom_point(aes(x=Longitude_dd,y=Latitude_dd),
                     data=dat2[dat2$yday%in%days,],
                     alpha=0.1,
                     color="red")+
          geom_point(aes(x=Longitude_dd,y=Latitude_dd,size=cpue_count_km2),
                     data=dat2[dat2$yday%in%days&dat2$cpue_count_km2>0,],
                     alpha=.5,
                     color="black")+
          labs(x="Longitude",y="Latitude",
               title=paste0(name,": ",
                           unique(dat2$DatStr[which(dat2$yday==min)]),
                           " - ",
                           unique(dat2$DatStr[which(dat2$yday==max)])
                           ),
               size=expression("Count per km"^2))+
          theme(
            legend.position="bottom"
          )+
            geom_segment(aes(
                xend = Start.Longitude.Decimal.Degrees.West,
                yend = Start.Latitude.Decimal.Degrees.North,
                x = End.Longitude.Decimal.Degrees.West,
                y = End.Latitude.Decimal.Degrees.North
            ),size=2,color="cyan",data=JSdat)
        
        
        }, height = 650, width = 750)
}

# Run the application 
shinyApp(ui = ui, server = server)
