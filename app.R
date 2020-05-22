library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(mosaic)
library(rnoaa)
library(shinyWidgets)
#library(leafpop)
#library(taxize)
#library(testit)

#---------------Only run this section if you want to update ghcnd-stations.txt-----------
# 
# 
# stationsDailyRaw <- read.fwf(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"),
#                              widths = c(11, 9, 11, 7, 2, 31, 5, 10),
#                              header = FALSE, strip.white = TRUE, comment.char = "",
#                              stringsAsFactors = FALSE)
# inventoryDailyRaw <- read.fwf(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"),
#                               widths = c(11, 9, 10, 5, 5, 5),
#                               header = FALSE, strip.white = TRUE, comment.char = "",
#                               stringsAsFactors = FALSE)
# stationColNames <- c("id","latitude", "longitude", "elevation",
#          "state", "name", "gsn_flag", "wmo_id")
# inventoryColNames <- c("id","latitude", "longitude",
#          "element", "first_year", "last_year")
# 
# ghcndStationsDaily <- stats::setNames(stationsDailyRaw, stationColNames)
# ghcndInventoryDaily <- stats::setNames(inventoryDailyRaw, inventoryColNames)
# 
# ghcndStationsDailyComplete <- merge(ghcndStationsDaily, ghcndInventoryDaily[, -c(2, 3)], by = "id")
# 
# sturdyGHCNDStations <- tibble::as_tibble(ghcndStationsDailyComplete[stats::complete.cases(ghcndStationsDailyComplete), ])
# 
# saveRDS(sturdyGHCNDStations, file = "./ghcnd-stations-current.csv")

#--------End Update of GHCND Stations----------

    
#Import seasonality database
AppendixS3_SeasonalityDatabase <- read.csv("./AppendixS3_SeasonalityDatabase.csv", header=TRUE)

#Selecting certain columns and creating mean_* columns 
dfWrangled <-  as.data.frame(AppendixS3_SeasonalityDatabase) %>% 
    select(Species, Species.1, BDT.C, EADDC, lat, lon) %>% 
    group_by(Species.1) %>% 
    mutate(mean_BDT.C = mean(BDT.C, na.rm=TRUE),
           mean_EADDC = mean(EADDC, na.rm=TRUE))

#Remove physiological outliers
dfWrangled = subset(dfWrangled, dfWrangled$BDT.C > -7 & dfWrangled$EADDC < 2000)

#Restrict to dat with lat / lon
dfWrangled = dfWrangled[which(!is.na(dfWrangled$lon) & !is.na(dfWrangled$lat) ),]
dfWrangled$uid <- seq.int(nrow(dfWrangled))

#setwd("../Insect-Phenology-Forecaster")

#Fill ghcnd stations local with necessary columns (not needed anymore i think)
# stations <- ghcndStationsLocal %>% 
#     mutate(last_year = 2020,
#            first_year = 2020,
#            element = 0) 
# colnames(stations)[1] <- "id"
# colnames(stations)[2] <- "latitude"
# colnames(stations)[3] <- "longitude"
# colnames(stations)[4] <- "elevation"
# colnames(stations)[5] <- "name"

#Create necessary datarframe for RNOAA query
latLonDF <- select(dfWrangled, c("Species.1", "uid", "lat", "lon"))
colnames(latLonDF) <- c("Species.1", "id", "latitude", "longitude")

#Shorten database for ease 
latLonDF <- head(latLonDF, 50)

#Turn each row to a dataframe 
pLatLonDF <- latLonDF %>% 
    rowwise %>% 
    do( X = as_data_frame(.) ) %>% 
    ungroup

#read in current local copy of ghcnd stations 
localGHCNDStations <- readRDS(file = "./ghcnd-stations-current.csv")

#Takes in a dataframe containing c("Species.1", "id", "latitude", "longitude") and returns info about nearest weather station 
nearestStat <- function(Y) {meteo_nearby_stations(lat_lon_df = Y,
                                                  station_data = localGHCNDStations,
                                                  var = "TMAX",
                                                  year_min = 2020,
                                                  year_max = 2020,
                                                  radius = 500,
                                                  limit = 1
                                                  )}

#Take every dataframe in pLatLonDF and add a result column containing the RNOAA-station-id of the nearest weather station
stationLatLonDf <- pLatLonDF %>% 
    mutate( result = map(X, nearestStat) ) %>% 
    unnest(cols = c(X, result)) %>% 
    select("Species.1", "id", "result") %>% 
    rename(uid = id) %>% 
    unnest(cols = c(result)) %>% 
    rename (sid = id) %>% 
    select("uid", "sid")
    
#Merge weather dataframe with species dataframe
speciesStationDF <- merge(x = dfWrangled, y = stationLatLonDf, by = "uid")

#Removes observations with no nearby weather stations (<50 miles) as defined by radius argument in nearestStat
speciesStationDF <- speciesStationDF[!(is.na(speciesStationDF$sid) | speciesStationDF$sid==""), ]


#-----Here is the user interface (What the user sees)-----
ui <- fluidPage(
    headerPanel('Insect Phenology Visualization'),
    
    sidebarPanel(
        multiInput('sel_species',
                   'Select species: ',
                   choices = as.vector(unique(speciesStationDF$Species.1)),
                   selected = unique(speciesStationDF$Species.1)),
        actionButton("all", "All"),
        actionButton("none", "None"),
        #actionButton("execute","Execute"),
        verbatimTextOutput(outputId = "pltInf", placeholder = TRUE),
        plotOutput("predPlot", height = 300),
    ),
    
    mainPanel(
        leafletOutput("mymap", height = 600)
    )
    
)


#------Here is the server for the shiny app (How the page becomes responsive)--------
server <- function(input, output, session){
    
    #Create a reactive dataframe which changes based on the selected species from the multi input tool
    lat_long_df <- reactive({
        x <- speciesStationDF %>% 
            filter(Species.1 %in% input$sel_species) 
    })
    
    #Listen for a button click (all or none) and change selections accordingly
    observeEvent(input$all, {
        updateMultiInput(
            session = session,
            inputId = "sel_species",
            selected = unique(speciesStationDF$Species.1)
        )
    })
    observeEvent(input$none, {
        updateMultiInput(
            session = session,
            inputId = "sel_species",
            selected = character(0)
        )
    })
    
    #-------If a user selects a circle marker, the phenology prediction plot for that insect will appear in the sidebar panel.
    observeEvent(input$mymap_marker_click, {
        click<-input$mymap_marker_click
        output$pltInf <- renderPrint(click$id)
        wData <- ncdc(datasetid='GHCND',
                      stationid= paste0('GHCND:', click$id),
                      datatypeid='tmax',
                      startdate = '2020-01-01',
                      enddate = '2020-05-21',
                      limit=500,
                      token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
        if(is_empty(wData$data)){
            output$pltInf <- renderPrint(paste("No data for: ",
                                                click$id))
        }
        output$predPlot <- renderPlot(
                    ncdc_plot(wData,
                              breaks="1 month",
                              dateformat="%m/%d")
        )
    })
    
    #-------Create map, add circle markers and popup-------
    output$mymap <- renderLeaflet({
        df <- lat_long_df()
        
        map <- leaflet(data = df) %>%
            addProviderTiles(providers$OpenTopoMap) %>% 
            #addTiles() %>%
            addCircleMarkers(lng = ~lon,
                             lat = ~lat,
                             radius = 1,
                             layerId = ~sid,
                             popup = paste("<em>",df$Species,"</em>", "<br>",
                                           #sci2comm(df$Species)[[1]][1], "<br>",
                                           "<b> EADDC: </b>", round(df$EADDC, digits=2), "<br>",
                                           "<b> BDT.C: </b>", round(df$BDT.C, digits=2), "<br>",
                                           "<b> SID: </b>", df$sid )) %>% #,
                                           # popupGraph(ncdc_plot(ncdc(datasetid='GHCND',
                                           #                           stationid=paste0('GHCND:', df$sid),
                                           #                           datatypeid='tmax',
                                           #                           startdate = '2020-01-01',
                                           #                           enddate = '2020-05-21',
                                           #                           limit=500,
                                           #                           token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ"),
                                           #                      breaks="1 month",
                                           #                      dateformat="%m/%d"),
                                           #            width = 300,
                                           #            height = 400))) %>% 
            setView(lng=-98.5795, lat=39.8283, zoom=4) #%>% 
        map
    })
}

#--------- Here, the shiny app is being executed--------
shinyApp(ui = ui, server = server)
