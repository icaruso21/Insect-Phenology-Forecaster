library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(mosaic)
library(rnoaa)
library(stringr)
library(shinyWidgets)

#library(gluskr)
#df <- transit_qol_df %>% 

df <- readRDS("../Insect-Phenology-Visualization/final_data.RDA") %>% 
    mutate("latitude" = intptlat, "longitude" = intptlon) %>% 
    group_by(year, msa_id) %>% 
    mutate(sum_year_vrh = sum(per_capita_vrh)) %>% 
    select(-transit_modes) %>% 
    unique() %>% 
    ungroup() %>% 
    select(msa_id, latitude, longitude, msa_name, sum_year_vrh, per_capita_gdp, percent_commuting_msa, pop_estimate_msa) %>% 
    group_by(msa_id) %>% 
    mutate(avg_gdp = mean(per_capita_gdp, na.rm= TRUE),
           avg_pop = mean(pop_estimate_msa),
           avg_commuting = mean(percent_commuting_msa),
           avg_vrh = mean(sum_year_vrh)) %>% 
    select(msa_id, latitude, longitude, msa_name, avg_vrh, avg_gdp, avg_commuting, avg_pop) %>% 
    unique() %>% 
    drop_na()
 
dfMain <-  as.data.frame(AppendixS3_SeasonalityDatabase)
    
dfWrangled <-  as.data.frame(AppendixS3_SeasonalityDatabase) %>% 
    select(Species.1, BDT.C, EADDC, lat, lon) %>% 
    group_by(Species.1) %>% 
    mutate(mean_BDT.C = mean(BDT.C, na.rm=TRUE),
           mean_EADDC = mean(EADDC, na.rm=TRUE))
    
#remove physiological outliers
dfWrangled = subset(dfWrangled, dfWrangled$BDT.C > -7 & dfWrangled$EADDC < 2000)

##Restrict to dat with lat / lon
dfWrangled = dfWrangled[which(!is.na(dfWrangled$lon) & !is.na(dfWrangled$lat) ),]

#setwd("~/Buckley_Lab/Insect-Phenology-Visualization")

#isaacdf <- readRDS("../finaldf.RDA")

#save(dfWrangled, file="finaldf.RDA")
#ddf <- meteo_nearby_stations(dfWrangled,
#                      lat_colname = "lat",
#                      lon_colname = "lon",
#                      station_data = ghcnd_stations(),
#                      var = "all",
#                      year_min = 2018,
#                      year_max = 2020,
#                      radius = 50,
#                      limit = 1
#)

# load station data - takes some minutes

# <- ghcnd_stations(refresh = TRUE)

# add id column for each location (necessary for next function)

#dfWrangled$id <- 1:nrow(dfWrangled)

# retrieve all stations in radius (e.g. 20km) using lapply

#stations <- lapply(1:nrow(dfWrangled),
#                   function(i) meteo_nearby_stations(df[i,],lat_colname = 'lat',lon_colname = 'lon',radius = 20,station_data = station_data)[[1]])

# pull data for nearest stations -  x$id[1] selects ID of closest station

#stations_data <- lapply(stations,function(x)  meteo_pull_monitors(x$id[1]))

#dfLabels <- as.data.frame(AppendixS2_SeasonalityDatabase_Columns)

#dfGHCND <- as.data.frame(ghcnd_stations)


ui <- fluidPage(
    headerPanel('Insect Phenology Visualization'),
    
    sidebarPanel(
        multiInput('sel_species',
                   'Select species: ',
                   choices = unique(dfWrangled$Species.1),
                   selected = unique(dfWrangled$Species.1)),
        actionButton("all", "All"),
        actionButton("none", "None"),
        verbatimTextOutput(outputId = "res")
    ),
    
    mainPanel(
        leafletOutput("mymap", height = 600)
    )
    
)

server <- function(input, output, session){
    
    
    lat_long_df <- reactive({
        x <- dfWrangled %>% 
            filter(Species.1 == input$sel_species) 
        x
    })
    
    output$res <- renderPrint(input$sel_species)
    
    observeEvent(input$all, {
        updateMultiInput(
            session = session,
            inputId = "sel_species",
            selected = unique(dfWrangled$Species.1)
        )
    })
    
    observeEvent(input$none, {
        updateMultiInput(
            session = session,
            inputId = "sel_species",
            selected = character(0)
        )
    })
    
    output$mymap <- renderLeaflet({
        df <- lat_long_df()
        
        map <- leaflet(data = df) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~lon,
                             lat = ~lat,
                             popup = paste("<em>",df$Species.1,"</em>", "<br>",
                                           "<b> BDT.C:  </b>", round(df$BDT.C, digits=2), "hours", "<br>",
                                           "<b> EADDC: </b>", round(df$EADDC, digits=2),
                                           "%", "<br>", "<b> Mean_BDT.C: </b> $", round(df$Mean_BDT.C, digits=2))) %>% 
            setView(lng=-98.5795, lat=39.8283, zoom=4) #%>% 
        #mapview(popup = popupGraph(test_plot(), width = 300, height = 300))
        map
    })
}

shinyApp(ui = ui, server = server)
