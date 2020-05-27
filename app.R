library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(tidyverse)
library(mosaic)
library(rnoaa)
library(shinyWidgets)
library(ggplot2)
library(lubridate)
#library(leafpop)
library(taxize)
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
#latLonDF <- head(latLonDF, 50)

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
                                                  var = c("TMAX", "TMIN"),
                                                  year_min = 2020,
                                                  year_max = 2020,
                                                  radius = 500,
                                                  limit = 1
                                                  )}

#Take every dataframe in pLatLonDF and add a result column containing the RNOAA-station-id of the nearest weather station
stationLatLonDf <- pLatLonDF %>% 
    mutate(result = map(X, nearestStat) ) %>% 
    unnest(cols = c(X, result)) %>% 
    select("Species.1", "id", "result") %>% 
    rename(uid = id) %>% 
    unnest(cols = c(result)) %>% 
    rename (sid = id) %>% 
    select("uid", "sid")
    
#Merge weather dataframe with species dataframe
speciesStationDF <- merge(x = dfWrangled, y = stationLatLonDf, by = "uid")

#Removes observations with no nearby weather stations (<500 miles) as defined by radius argument in nearestStat
speciesStationDF <- speciesStationDF[!(is.na(speciesStationDF$sid) | speciesStationDF$sid==""), ]

#Check have TMIN and TMAX dat (Potentially need in different location later)
# stations= stations[which(stations$Var=="TMAX" | stations$Var=="TMIN"),]
# stations= spread(stations,Var, Var)
# stations= stations[which(stations$TMAX=="TMAX" & stations$TMIN=="TMIN"),]

#DEGREE DAYS CALCULATION 
#Single sine wave approximation from Baskerville & Emin 1969
#(see http://www.ipm.ucdavis.edu/WEATHER/ddss_tbl.html)
#Input:
#Tdat: 2 column matrix with Tmin followed by Tmax
#LDT:lower developmental threshold
#------AM I CONFUSED? I thought Degree Days couldn't be negative... 
degree.days.mat=function(Tmin, Tmax, LDT){
    
    # entirely above LDT
    if(Tmin>=LDT) {dd = (Tmax+Tmin)/2-LDT}
    
    # intercepted by LDT
    ## for single sine wave approximation
    if(Tmin<LDT && Tmax>LDT){
        alpha=(Tmax-Tmin)/2
        theta1=asin(((LDT-(Tmax+Tmin))/alpha)*pi/180)
        dd=1/pi*(((Tmax+Tmin)/2-LDT)*(pi/2-theta1)+alpha*cos(theta1))
        if(!is.na(dd))if(dd<0){dd=0}
    } #matches online calculation
    
    # entirely below LDT
    if(Tmax <= LDT){dd = 0}
    
    return(dd)
}

# cumsum with reset adapted from @jgilfillan on github, many thanks! 
cumsum_with_reset <- function(x, threshold) {
    cumsum <- 0
    group <- 1
    result <- numeric()
    
    for (i in 1:length(x)) {
        if (cumsum == threshold){
            group <- group + 1
            cumsum <- 0
        }
        
        cumsum <- cumsum + x[i]
        
        if (cumsum > threshold) {
            cumsum <- threshold
        }
        
        result = c(result, cumsum)
        
    }
    
    return (result)
}

#-----Graphing Helper Functions---------
dd_plot <- function(tMax, tMin, BDT, EADDC, breaks = NULL, dateformat='%d/%m/%y') {
    UseMethod("dd_plot")
}


dd_plot.ncdc_data <- function(tMax, tMin, BDT, EADDC, breaks = NULL, dateformat='%d/%m/%y') {
    #Pulling data from tMax RNOAA object into dfTMAX
    dfTMAX <- list(tMax)[[1]]$data %>% 
        rename(TMAX = value)
    dfTMIN <- list(tMin)[[1]]$data %>% 
        rename(TMIN = value)
    
    #Cleaning up dates 
    dfTMAX$date <- ymd(sub('T00:00:00\\.000|T00:00:00', '', as.character(dfTMAX$date)))
    dfTMIN$date <- ymd(sub('T00:00:00\\.000|T00:00:00', '', as.character(dfTMIN$date)))
    
#    value = NULL
    
    #Joining TMIN and TMAX data into dfTEMP
    dfTEMP <- full_join(dfTMAX, dfTMIN[ , c("date", "TMIN")], by = 'date')
    
    #Matching units and removing errors
    dfTEMP$TMAX[which(dfTEMP$TMAX==-9999)]= NA
    dfTEMP$TMAX= dfTEMP$TMAX/10 #correct for tenths of degrees or mm
    dfTEMP$TMIN[which(dfTEMP$TMIN==-9999)]= NA
    dfTEMP$TMIN= dfTEMP$TMIN/10 #correct for tenths of degrees or mm
    
    #Calculating degree days in a new column in dDays
    # dDays <- dfTEMP %>% 
    #     mutate (dd = degree.days.mat(TMIN, TMAX, BDT)) %>% 
    #     na.omit()
    dfTEMP <- na.omit(dfTEMP)
    dfTEMP$dd <- NA
    for (i in 1:dim(dfTEMP)[1]) {
      dd = degree.days.mat(dfTEMP$TMIN[i], dfTEMP$TMAX[i], BDT)
      dfTEMP$dd[i] <- dd
    }
   # dDays = dfTEMP %>% na.omit()
    dDays <- dfTEMP
    #Adding a csum column which sums degree days and resets after reaching threshold (EADDC)
    dDays$csum <- cumsum_with_reset(dDays$dd, EADDC)
    
    #Plot csum vs date
    ggplot(dDays, aes(date, csum)) +
        plot_template(df, breaks, dateformat) +
        ncdc_theme() +
        geom_hline(yintercept = EADDC, linetype = "dashed", color = "red") #+
        #scale_y_continuous(breaks = sort(c(ggplot_build(plot1)$layout$panel_ranges[[1]]$y.major_source, h)))
}


dd_plot.default <- function(tMax, tMin, BDT, EADDC, breaks = NULL, dateformat = '%d/%m/%y') {
    stop("No method for ", class(list(tMax)[[1]]), call. = FALSE)
}

plot_template <- function(df, breaks, dateformat) {
    tt <- list(
        theme_bw(base_size = 18),
        geom_line(size = 2),
        labs(y = "Cumulative Degree Days", x = "Date")
    )
    if (!is.null(breaks)) {
        c(tt, scale_x_date(date_breaks = breaks, date_labels = dateformat))
    } else {
        tt
    }
}
#--------End graphing helper functions--------------------

#Fetch a common name for a species or return "No available common name." if no results found
safeSci2Com <- function(df) {
  com <- sci2comm(df, db = "eol", simplify = TRUE) %>% 
    flatten()
  if(identical(com, list())){
    com <- "No available common name."
  } else {
    #fcom <- flatten(com) 
    com <- com[[1]] 
  }
  return(com)
}


#-----It's the user interface! (What the user sees)-------
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
      # actionButton("updateDateRange", "View"),
    #  hr(),
      verbatimTextOutput(outputId = "pltInf", placeholder = FALSE),
      dateRangeInput(inputId = "dateRange", 
                     label = "Change date range: ",
                     start  = "2020-01-01",
                     min    = "1900-01-01",
                     format = "mm/dd/yy",
                     separator = " - "),
      plotOutput("predPlot", height = 300)
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
    
    timeRange <- reactive({
      x <- input$dateRange
    })
    
    #If a user selects a circle marker, the phenology prediction plot for that insect will appear in the sidebar panel.
    #If the user changes timeRange(), and has already selected an observation from the map. The plot will update with new data.
    observeEvent({
      input$mymap_marker_click
      timeRange()
    }, {
      click<-input$mymap_marker_click
      uid <- click$id
      if(!is.null(uid)){
        time <- timeRange()
        output$pltInf <- renderPrint(safeSci2Com(speciesStationDF$Species[uid]))

        tMax <- ncdc(datasetid='GHCND',
                     stationid= paste0('GHCND:', speciesStationDF$sid[uid]),
                     datatypeid= "TMAX",
                     startdate = time[1],
                     enddate = time[2],
                     limit=500,
                     token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
        tMin <- ncdc(datasetid='GHCND',
                     stationid= paste0('GHCND:', speciesStationDF$sid[uid]),
                     datatypeid= "TMIN",
                     startdate = time[1],
                     enddate = time[2],
                     limit=500,
                     token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
        
        output$predPlot <- renderPlot(
          dd_plot(tMax, 
                  tMin, 
                  speciesStationDF$BDT.C[uid],
                  speciesStationDF$EADDC[uid],
                  breaks="1 month",
                  dateformat="%m/%d")
        )} else {
          output$pltInf <- renderPrint("Select an observation from the map.")
        }
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
                             layerId = ~uid,
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
