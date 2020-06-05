library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(mosaic)
library(rnoaa)
library(shinyWidgets)
library(ggplot2)
library(lubridate)
#library(leafpop)
library(taxize)
library(AOI)
library(climateR)
#library(rgeos)
#library(sf)
library(raster)
library(rasterVis)
#library(sp)
library(tidyverse)
#library(rgdal)
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
dfWrangled <-  as_tibble(AppendixS3_SeasonalityDatabase) %>% 
    dplyr::select(Species, Species.1, BDT.C, EADDC, lat, lon) %>% 
    group_by(Species.1) %>% 
    mutate(mean_BDT.C = mean(BDT.C, na.rm=TRUE),
           mean_EADDC = mean(EADDC, na.rm=TRUE))

#Remove physiological outliers
dfWrangled = subset(dfWrangled, dfWrangled$BDT.C > -7 & dfWrangled$EADDC < 2000)

#Restrict to dat with lat / lon
dfWrangled = dfWrangled[which(!is.na(dfWrangled$lon) & !is.na(dfWrangled$lat) ),]
dfWrangled$uid <- seq.int(nrow(dfWrangled))  
  
#dfWrangled = filter(dfWrangled, Species.1 == 'pomonella')
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
latLonDF <- dplyr::select(dfWrangled, c("Species.1", "uid", "lat", "lon"))
colnames(latLonDF) <- c("Species.1", "id", "latitude", "longitude")

#Shorten database for ease 
num_spec = 65
latLonDF <- head(latLonDF, num_spec)

#Turn each row to a dataframe 
pLatLonDF <- latLonDF %>% 
    rowwise %>% 
    do( X = as_tibble(.) ) %>% 
    ungroup

#read in current local copy of ghcnd stations 
localGHCNDStations <- readRDS(file = "./ghcnd-stations-current.csv")

#Takes in a dataframe containing c("Species.1", "id", "latitude", "longitude") and returns info about nearest weather station 
nearestStat <- function(Y) {meteo_nearby_stations(lat_lon_df = Y,
                                                  station_data = localGHCNDStations,
                                                  var = c("TMAX", "TMIN"),
                                                  year_min = 2000,
                                                  year_max = 2020,
                                                  radius = 500,
                                                  limit = 2
                                                  )}

#Take every dataframe in pLatLonDF and add a result column containing the RNOAA-station-id of the nearest weather station
stationLatLonDf <- pLatLonDF %>% 
    mutate(result = map(X, nearestStat)) %>% 
    unnest(cols = c(X, result)) %>%
    dplyr::select("Species.1", "id", "result") %>% 
    rename(uid = id) %>% 
    unnest(cols = c(result)) %>% 
    rename(sid = id) %>% 
    dplyr::select("uid", "sid")
stationLatLonDf$rank <- rep(c(1,2), length.out = nrow(stationLatLonDf))
stationLatLonDf <- spread(stationLatLonDf, rank, sid)
colnames(stationLatLonDf) <- c("uid", "sid1", "sid2")

#Merge weather dataframe with species dataframe
speciesStationDF <- merge(x = dfWrangled, y = stationLatLonDf, by = "uid")

#Removes observations with no nearby weather stations (<500 miles) as defined by radius argument in nearestStat
speciesStationDF <- speciesStationDF[!(is.na(speciesStationDF$sid1) || speciesStationDF$sid1==""), ]

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
#------Adapted from Lauren Buckley (no longer allows for negative DDs and accepts NA values)
degree.days.mat=function(Tmin, Tmax, LDT){
  
  
  # entirely above LDT
  #|| is.null(Tmin) || is.null(Tmax)
  if(is.na(Tmin) || is.na(Tmax) || is.null(Tmin) || is.null(Tmax)){dd = NA}
  else{
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
    if(Tmax <= LDT){dd = 0}}
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
dd_plot <- function(tMax1, tMax2, tMin1, tMin2, BDT, EADDC, startTime, breaks = NULL, dateformat='%d/%m/%y') {
    UseMethod("dd_plot")
}


dd_plot.tbl_df <- function(tMax1, tMax2, tMin1, tMin2, BDT, EADDC, startTime, breaks = NULL, dateformat='%d/%m/%y') {
  
  #Making a new dataframe that has all of tMax1 and tMax2 for missing dates
  if(!is.na(tMax1) && !is.na(tMax2) && !is.na(tMin1) && !is.na(tMin2)) {
    dfTMAX <- rbind(tMax1, tMax2[!tMax2$date %in% tMax1$date,])
    dfTMIN <- rbind(tMin1, tMin2[!tMin2$date %in% tMin1$date,])
  } else {
    dfTMAX <- rbind(tMax1, tMax2)
    dfTMIN <- rbind(tMin1, tMin2)
  }
  #Cleaning up dates 
  dfTMAX$date <- ymd(sub('T00:00:00\\.000|T00:00:00', '', as.character(dfTMAX$date)))
  dfTMIN$date <- ymd(sub('T00:00:00\\.000|T00:00:00', '', as.character(dfTMIN$date)))
  
  #Order dataframe by date
  dfTMAX <- dfTMAX[order(as.Date(dfTMAX$date, format = "%y%m%d")),]
  dfTMIN <- dfTMIN[order(as.Date(dfTMIN$date, format = "%y%m%d")),]
  #value = NULL
  
  #Joining TMIN and TMAX data into dfTEMP
  dfTEMP <- full_join(dfTMAX, dfTMIN[ , c("date", "TMIN")], by = 'date')
  
  #Matching units and removing errors
  dfTEMP$TMAX[which(dfTEMP$TMAX==-9999)]= NA
  dfTEMP$TMAX= dfTEMP$TMAX/10 #correct for tenths of degrees or mm
  dfTEMP$TMIN[which(dfTEMP$TMIN==-9999)]= NA
  dfTEMP$TMIN= dfTEMP$TMIN/10 #correct for tenths of degrees or mm
  #Catch other NA values
  #Comment back in ============================
  dfTEMP$TMAX[which(dfTEMP$TMAX > 200)] = NA
  dfTEMP$TMIN[which(dfTEMP$TMIN > 200)] = NA
  dfTEMP$TMAX[which(dfTEMP$TMAX < -200)] = NA
  dfTEMP$TMIN[which(dfTEMP$TMIN < -200)] = NA
  #===============================================
  #Calculating degree days in a new column in dDays
  # dDays <- dfTEMP %>% 
  #     mutate (dd = degree.days.mat(TMIN, TMAX, BDT)) %>% 
  
  #split year 
#  date= as.Date(dfTEMP$date, "%Y-%m-$d")
#  dfTEMP$year=as.numeric(format(date, "%Y"))
  
  ## FIND YEARS WITH NEARLY COMPLETE DATA
   # dat.agg= aggregate(dfTEMP, list(dfTEMP$year),FUN=count)  ### PROBLEM IF RASTER LOADED
   # years= dat.agg$Group.1[which(dat.agg$TMAX>50)]
   # dfTEMP= dfTEMP[which(dfTEMP$year %in% years),]

    #na.omit()
  dfTEMP <- na.omit(dfTEMP)
  dfTEMP$dd <- NA
  for (i in 1:nrow(dfTEMP)) {
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
    geom_hline(aes(yintercept = EADDC), linetype = "dashed", color = "green") +
    geom_text(aes( startTime, EADDC, label = "EADDC", vjust = +1.5, hjust = -0.1), size = 3)
  #scale_y_continuous(breaks = sort(c(ggplot_build(plot1)$layout$panel_ranges[[1]]$y.major_source, h)))
}


dd_plot.default <- function(tMax1, tMax2, tMin1, tMin2, BDT, EADDC, startTime, breaks = NULL, dateformat = '%d/%m/%y') {
    stop("No method for ", class(list(tMax1)[[1]]), call. = FALSE)
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

#-----Get raster accumulated DD for current year----------
#--Note: degree.days.mat(tmin, tmax, BDT) must be declared prior to execution
accumulateDD <- function(start_date, end_date = Sys.Date() -2, BDT = 9.65, EADDC = 607.6, cum_DD = NULL){
  #Define area of interest 
  if(!is.Date(start_date)){start_date <- as.Date(start_date)}
  if(!is.Date(end_date)){end_date <- as.Date(end_date)}
  
  AOI = aoi_get(state = "conus")
  #Get temp raster stack for start_date
  #raster::plot(AOI)
  print(start_date)
  p = getGridMET(AOI, param = c('tmax','tmin'), startDate = start_date)
  r = raster::stack(p$tmax, p$tmin)
  names(r) = c('tmax', 'tmin')
  #print("not above here")
  #Initialize cum_DD to DD values for start_date
  if(is.null(cum_DD)){
    #print("initializing")
    cum_DD <- calc(r, fun = function(x){
      #print(value(x[2]))
      degree.days.mat(value(x[2]) -273.15, value(x[1]) -273.15, BDT)})
  }
  print(cum_DD)
  # cum_DD <- calc(r, fun = function(x){
  #   degree.days.mat(x[2] / 10, x[1] / 10, BDT)})
  #Set current day to the next start day
  week <- 1
  current_date = start_date + 1
  the_stack <- NULL
  #Accumulate Degree Days from current_date to end_date, inclusive.
  while(current_date <= end_date){
    #Get raster stack of tmin and tmax for current day
    temps = getGridMET(AOI, param = c('tmax','tmin'), startDate = current_date)
    tstack = raster::stack(temps$tmax, temps$tmin)
    names(tstack) = c('tmax', 'tmin')
    print(current_date)
    #Calculate todays DD values
    current_DD <- calc(tstack, fun = function(x){degree.days.mat(value(x[2]) -273.15, value(x[1]) -273.15, BDT)})
    #print(identical(current_DD, cum_DD))
    names(current_DD) = c(current_date)
    #Add cumulative DD values to current_date DD values (current_DD)
    cum_DD <- cum_DD + current_DD
    #Reset cum_DD values greater than EADDC to 0
    cum_DD <- calc(cum_DD, fun = function(cumul){
      if (!is.na(cumul[1]) && (cumul[1] >= EADDC)){
        return(as.vector(EADDC))} 
      else {
        return(cumul[1])}})
    #Increment current_date
    names(cum_DD) = c(current_date)
    week <- week + 1
    current_date = current_date + 1
    if(week == 7){
      the_stack <- raster::stack(cum_DD) 
      print(the_stack)
      print(names(the_stack))
    }
    if(week == 14){
      #if(is.null(the_stack)){the_stack = raster::stack("holdraster.grd")}
      the_stack <- raster::stack(the_stack, cum_DD)
      #writeRaster(the_stack, "holdraster.grd", overwrite=TRUE)
      print(the_stack)
      print(names(the_stack))
      #the_stack <- NULL
      week <- 7
    }
  }
  #if(is.null(the_stack)){the_stack = raster::stack("holdraster.grd")}
  writeRaster(the_stack, str_c(current_date, ".grd"), overwrite=TRUE)
  return(the_stack)
  #raster::plot(newR)
}

accumulateDDPart <- function(start_date, end_date = Sys.Date() -2, BDT = 9.65, EADDC = 607.6, cum_DD = NULL){
  #Define area of interest 
  if(!is.Date(start_date)){start_date <- as.Date(start_date)}
  if(!is.Date(end_date)){end_date <- as.Date(end_date)}
  
  AOI = aoi_get(state = "conus")
  #Get temp raster stack for start_date
  #raster::plot(AOI)
  print(start_date)
  p = getGridMET(AOI, param = c('tmax','tmin'), startDate = start_date)
  r = raster::stack(p$tmax, p$tmin)
  names(r) = c('tmax', 'tmin')
  #print("not above here")
  #Initialize cum_DD to DD values for start_date
  if(is.null(cum_DD)){
    #print("initializing")
    cum_DD <- calc(r, fun = function(x){
      #print(value(x[2]))
      degree.days.mat(value(x[2]) -273.15, value(x[1]) -273.15, BDT)})
  }
  print(cum_DD)
  # cum_DD <- calc(r, fun = function(x){
  #   degree.days.mat(x[2] / 10, x[1] / 10, BDT)})
  #Set current day to the next start day
  week <- 1
  current_date = start_date + 1
  #Accumulate Degree Days from current_date to end_date, inclusive.
  while(current_date <= end_date){
    #Get raster stack of tmin and tmax for current day
    temps = getGridMET(AOI, param = c('tmax','tmin'), startDate = current_date)
    tstack = raster::stack(temps$tmax, temps$tmin)
    names(tstack) = c('tmax', 'tmin')
    print(current_date)
    #Calculate todays DD values
    current_DD <- calc(tstack, fun = function(x){degree.days.mat(value(x[2]) -273.15, value(x[1]) -273.15, BDT)})
    #print(identical(current_DD, cum_DD))
    names(current_DD) = c(current_date)
    #Add cumulative DD values to current_date DD values (current_DD)
    cum_DD <- cum_DD + current_DD
    #Reset cum_DD values greater than EADDC to 0
    cum_DD <- calc(cum_DD, fun = function(cumul){
      if (!is.na(cumul[1]) && (cumul[1] >= EADDC)){
        return(as.vector(EADDC))} 
      else {
        return(cumul[1])}})
    #Increment current_date
    names(cum_DD) = c(current_date)
    week <- week + 1
    current_date = current_date + 1
  }
  #if(is.null(the_stack)){the_stack = raster::stack("holdraster.grd")}
  #writeRaster(the_stack, str_c(current_date, ".grd"), overwrite=TRUE)
  return(cum_DD)
  #raster::plot(newR)
}
#raster::plot(jan15$, col = hcl.colors(5, palette = "RdYlGn"))
# pomonella2020 <- raster::stack(pomonellaJ2020, pomonellaF2020, pomonellaM2020, pomonellaA2020, pomonellaMAY2020)
# # writeRaster(pomonella2020, "pomonella2020.grd")
# makeYear <- function(current_date, 
#                      end_date = Sys.Date()-2, 
#                      BDT = 9.65, 
#                      EADDC = 607.6){#,
#                      #yesterday_cumDD = NA){
#   # if(!is.na(yesterday_cumDD)){
#   #   ydd = accumulateDD(current_date, current_date + 6, BDT, EADDC, yesterday_cumDD)}
#   # else{
#     ydd = accumulateDD(current_date, current_date + 6, BDT, EADDC)#}
#   print(current_date)
#   current_date = current_date + 7
#   print(current_date)
#   pomo2020 <- raster::stack(ydd)
# while(end_date >= current_date){
#   tdd <- accumulateDD(current_date, current_date + 6, BDT, EADDC, ydd)
#   names(tdd) = c(current_date)
#   pomo2020 <- raster::stack(pomo2020, tdd)
#   ydd <- tdd
#   current_date = current_date + 7
#   print(current_date)
# }
#   writeRaster(pomo2020, "pomo2020.grd", overwrite=TRUE)
#   return(pomo2020)}

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
    radioButtons(inputId = "computeDD",
                 label = "Compute DD for date?",
                 choiceNames = c("Compute", "Closest Available"),
                 choiceValues = c(TRUE, FALSE),
                 selected = FALSE),
    dateInput(inputId = "phenoDate", 
                   label = "Change layer date: ",
                   value  = Sys.Date()-2,
                   min    = as.Date(str_c(year(Sys.Date()), '-01-01')),
                   format = "mm/dd/yy"),
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
        output$pltInf <- renderPrint(tryCatch(safeSci2Com(speciesStationDF$Species[uid]), 
                                              error = function(e){return("Common Name Dataset Not Available.")}))

        tMax1 <- ncdc(datasetid='GHCND',
                     stationid= paste0('GHCND:', speciesStationDF$sid1[uid]),
                     datatypeid= "TMAX",
                     startdate = time[1],
                     enddate = time[2],
                     limit=500,
                     token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
        
        tMax2 <- ncdc(datasetid='GHCND',
                      stationid= paste0('GHCND:', speciesStationDF$sid2[uid]),
                      datatypeid= "TMAX",
                      startdate = time[1],
                      enddate = time[2],
                      limit=500,
                      token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
        
        ifelse(nrow(tMax1$data)==0,
               tMax1 <- NA,
               tMax1 <- tMax1$data %>% dplyr::select(date, value) %>% rename(TMAX = value)
        )
        ifelse(nrow(tMax2$data)==0,
               tMax2 <- NA,
               tMax2 <- tMax2$data %>% dplyr::select(date, value) %>% rename(TMAX = value)
        )
        
        tMin1 <- ncdc(datasetid='GHCND',
                      stationid= paste0('GHCND:', speciesStationDF$sid1[uid]),
                      datatypeid= "TMIN",
                      startdate = time[1],
                      enddate = time[2],
                      limit=500,
                      token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
        
        tMin2 <- ncdc(datasetid='GHCND',
                      stationid= paste0('GHCND:', speciesStationDF$sid2[uid]),
                      datatypeid= "TMIN",
                      startdate = time[1],
                      enddate = time[2],
                      limit=500,
                      token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
        
        ifelse(nrow(tMin1$data)==0,
               tMin1 <- NA,
               tMin1 <- tMin1$data %>% dplyr::select(date, value) %>% rename(TMIN = value)
        )
        ifelse(nrow(tMin2$data)==0,
               tMin2 <- NA,
               tMin2 <- tMin2$data %>% dplyr::select(date, value) %>% rename(TMIN = value)
        )
        
        if(!is.na(tMax1) && !is.na(tMax2))
        {output$predPlot <- renderPlot(
          dd_plot(tMax1, 
                  tMax2, 
                  tMin1, 
                  tMin2,
                  speciesStationDF$BDT.C[uid],
                  speciesStationDF$EADDC[uid],
                  time[1],
                  breaks="1 month",
                  dateformat="%m/%d"))}
        else {output$pltInf <- renderPrint("No current RNOAA data available here.")}
      } else {
        output$pltInf <- renderPrint("Select an observation from the map.")
      }
    })
    
    #-------Create map, add circle markers and popup-------
    
    # AOI = aoi_get(state = "conus")
    # 
    # p = getGridMET(AOI, param = c('tmax','tmin'), startDate = Sys.Date()-2)
    # #q = getGridMET(AOI, param = c('tmin'), startDate = "2018-8-15")
    # 
    # r = raster::stack(p$tmax, p$tmin)
    # names(r) = c('tmax', 'tmin')
    # ddMap <- calc(r, fun = function(x){degree.days.mat(x[2] / 10, x[1] / 10, 15)})
    #raster::plot(newR)
    pomonella2020 <- raster::stack("pomonella2020.grd")
    print(names(pomonella2020))
    # pal <- colorNumeric("RdYlGn", c(0, 610),
    #                     na.color = "transparent")
    pal <- colorBin(c('transparent', '#ff7729', '#ffc324', '#59711b', '#4376c7'), 
                    c(0, 610), 
                    bins = c(0, 150, 300, 500, 607, 700),
                    na.color = "transparent")
    # a = raster(r)
    #rasterVis::levelplot(r)
    phenDate <- reactive({
      x <- input$phenoDate
    })
    
    output$mymap <- renderLeaflet({
      df <- lat_long_df()
      map <- leaflet(data = df) %>%
        addProviderTiles(providers$OpenTopoMap) %>% 
        addLayersControl(baseGroups = c("Phen", "Obs")) %>% #, 
                         #options = layersControlOptions(collapsed = F)) %>% 
        #addTiles() %>%
        #print(type(phenDate))
           
        # addRasterImage(pomonella2020$layer.1, colors = pal, group = "Phen") %>%
        # addRasterImage(pomonella2020$layer.2, colors = pal, group = "Feb") %>%
        # addRasterImage(pomonella2020$layer.3, colors = pal, group = "Mar") %>%
        # addRasterImage(pomonella2020$layer.4, colors = pal, group = "Apr") %>%
        # addRasterImage(pomonella2020$layer.5, colors = pal, group = "May") %>%
        
        #addRasterImage(ddMap, colors = pal, group = "DD") %>%
        addLegend(pal = pal,
                  values = c(0, 610),
                  group = "Phen",
                  position = "bottomright",
                  title = "Cumulative Degree Days") %>% 
        #addLegend(pal = pal, values = values(r$tmax), group = "tmax", title = "Min Daily Temp") %>% 
        addCircleMarkers(lng = ~lon,
                         lat = ~lat,
                         radius = 1,
                         group = "Obs",
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
      setView(lng=-98.5795, lat=39.8283, zoom=4)  
      #%>% 
      dateR <- phenDate()
      print(dateR)
      if(input$computeDD){if(reduce(names(pomonella2020) %in% str_c('X', gsub('-', '.', dateR)), sum) == 1){
        output$pltInf <- renderPrint("Image rendering...")
        toView <- raster(pomonella2020, layer = which(names(pomonella2020) %in% str_c('X', gsub('-', '.', dateR))))
        map <- addRasterImage(map, toView, colors = pal, group = "Phen", opacity = 0.6) 
        output$pltInf <- renderPrint(str_c("DD for: ", dateR))
      }else{
          tempDate <- dateR
        while(reduce(names(pomonella2020) %in% str_c('X', gsub('-', '.', tempDate)), sum) != 1){
          tempDate <- tempDate - 1}
          output$pltInf <- renderPrint(str_c("Accumulating from: ", tempDate))
          toAccum <- raster(pomonella2020, layer = which(names(pomonella2020) %in% str_c('X', gsub('-', '.', tempDate))))
          toView <- accumulateDDPart(tempDate, dateR, cum_DD = toAccum)
          map <- addRasterImage(map, toView, colors = pal, group = "Phen", opacity = 0.6) 
          output$pltInf <- renderPrint(str_c("DD for: ", dateR))
      }}else{
        tempDate <- dateR
        while(reduce(names(pomonella2020) %in% str_c('X', gsub('-', '.', tempDate)), sum) != 1){
          tempDate <- tempDate - 1}
        toView <- raster(pomonella2020, layer = which(names(pomonella2020) %in% str_c('X', gsub('-', '.', tempDate))))
        map <- addRasterImage(map, toView, colors = pal, group = "Phen", opacity = 0.6) 
        output$pltInf <- renderPrint(str_c("DD for: ", tempDate))
          }
      map
    })
}

#--------- Here, the shiny app is being executed--------
shinyApp(ui = ui, server = server)
