library(sf)
library(tmap)
library(leaflet)
library(tmaptools)
library(tidyverse)
library(classInt)
library(shiny)
library(RColorBrewer)
library(here)
library(geojsonio)

UK <- geojson_read("https://opendata.arcgis.com/datasets/473aefdcee19418da7e5dbfdeacf7b90_2.geojson", what = "sp")

#switching the geojson to NUTS
UK <- st_as_sf(UK)

#reading in the UK shapefile

#reading in the UK data for regional GVA
UKData <- read.csv("Data/UKdata3.csv", na="n/a")

UKData1 <- UKData

names(UKData1) <- c("NUTS_level", "O1997", "GVA in 1997", "O1998", "GVA in 1998", "O1999", "GVA in 1999", "O2000", "GVA in 2000", "O2001", "GVA in 2001", "O2002", "GVA in 2002", "O2003", "GVA in 2003", "O2004", "GVA in 2004", "O2005", "GVA in 2005", "O2006", "GVA in 2007", "O2008", "GVA in 2008", "O2009", "GVA in 2009", "O2010", "GVA in 2010", "O2011", "GVA in 2011", "O2012", "GVA in 2012", "O2013", "GVA in 2013", "O2014", "GVA in 2014", "O2015", "GVA in 2015", "O2016", "GVA in 2016", "O2017", "GVA in 2017")

#merging the shapefile and the regional data on the basis of the NUTS3 classifications
UKDataMap <- merge(UK,
                   UKData1,
                   by.x = "nuts318cd",
                   by.y = "NUTS_level",
                   no.dups=TRUE)


choice =c("GVA in 1997",  "GVA in 1998", "GVA in 1999", "GVA in 2000", "GVA in 2001",  "GVA in 2002",  "GVA in 2003",  "GVA in 2004",  "GVA in 2005",  "GVA in 2007",  "GVA in 2008",  "GVA in 2009", "GVA in 2010",  "GVA in 2011",  "GVA in 2012", "GVA in 2013", "GVA in 2014",  "GVA in 2015",  "GVA in 2016",  "GVA in 2017")
#setting the column headers to use when running the git

#establishing the user interface for the shiny app
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    #leaflet will call the map from the server function
    leafletOutput("map", width = "100%", height = "100%"),
    #this sets theput panel placement
    #setting an absolute panel as want it to cover the whole of the webpage
    absolutePanel(top = 10, right = 10,
                  #we want them to select the years so we create select input
                  #this is given the id years (to be called in the server) and label it years
                  #the choices that the user can select come from the choices 
                  selectInput("years",
                              label = "year",
                              choices = choice,
                              multiple = FALSE
                  ),
                  #we also want the user to select the colour pallete to see the differences
                  #we set the id to colour brewer pallete and the user will see it as 
                  selectInput("colourbrewerpalette", "Color Scheme",
                              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                  ),
                  #the final slider allows the user to select the regional GVA % they want to see
                  sliderInput("slide","local GVA",
                              #this sets the minimum GVA that will be shown
                              min(as.numeric(UKData$GVA_._2017), na.rm=TRUE),
                              #this shows the maximum GVA that will be shown
                              max(as.numeric(UKData$GVA_._2017), na.rm=TRUE),
                              #this sets the range of the values, we want it to be as big as possible
                              value = range(as.numeric(UKData$GVA_._2017), na.rm=TRUE),
                              #the step of percentages is in a step of 1
                              step = 1,
                              sep = ""
                  ),
                  
    )
)



####################################### server
server <- function(input, output, session) {
    output$map <- renderLeaflet({
        #using leaflet
        #including aspect that won't need to change 
        #setting the view as over the UK
        leaflet(UKDataMap) %>% addTiles() %>% setView(-0.5, 53, zoom = 6)
    })
    #setting the observe function to adjust to sliders
    observe({
        #filter the data according to the adjustable slider
        (UKDataMap2 <- ({UKDataMap[as.numeric(UKDataMap[[input$years]])>=input$slide[1]&as.numeric(UKDataMap[[input$years]]) <= input$slide[2],]}))
        #setting the breaks as fixed breaks
        breaks<-classIntervals(as.numeric(UKDataMap2[[input$years]]), n=8, style= "fixed", fixedBreaks =c(0,55,70,85,100,115,130,145,1500))
        breaks <- breaks$brks
        #observing the pallete using all the data
        pal <- colorBin(palette = input$colourbrewerpalette, 
                        #selecting which domain this pallete adjusts for
                        domain = UKDataMap2[[input$years]],
                        #setting the breaks
                        bins = breaks)
        #map filtered data from the slider
        leafletProxy("map", data=UKDataMap2) %>%
            clearShapes() %>% 
            addPolygons(color="white", 
                        weight = 2,
                        opacity = 1,
                        dashArray = "3",
                        # a popup of region name and %
                        popup = paste(UKDataMap2$nuts318nm,"- %",UKDataMap2[[input$years]]),
                        fillOpacity = 0.5, 
                        fillColor = ~pal(UKDataMap2[[input$years]])
            )
    })
    
    observe({
        # call the filter again for this observer to be able to create the legend
        (UKDataMap2<-({UKDataMap[as.numeric(UKDataMap[[input$years]]) >= input$slide[1] & as.numeric(UKDataMap[[input$years]]) <=
                                     input$slide[2],]}))
        
        breaks<-classIntervals(as.numeric(UKDataMap2[[input$years]]), n=11, style= "fixed", fixedBreaks =c(0,55,70,85,100,115,130,145,1500))
        breaks <- breaks$brks
        
        pal <- colorBin(palette = input$colourbrewerpalette, 
                        domain = UKDataMap2[[input$years]],
                        #create bins using the breaks object from earlier
                        bins = breaks
        )
        #This is the legend
        proxy <- leafletProxy("map", data = UKDataMap2)
        proxy %>% clearControls() %>%
            #putting this in the bottom right hand corner
            addLegend("bottomright", 
                      #the palette is the same as before
                      pal= pal, 
                      #the values come from the input year
                      values = ~UKDataMap2[[input$years]], 
                      #setting the title of the legend
                      title = "GVA as a percentage of UK average (%)", 
                      labFormat = labelFormat(prefix = ""),
                      opacity = 1
            )
    })
}

#intialising the shinyapp using the server and ui created above
shinyApp(ui, server)

