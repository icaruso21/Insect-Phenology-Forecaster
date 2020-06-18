#Just importing packages from CRAN
cranbraries <- c('shiny', 'leaflet', 'mosaic', 'rnoaa', 'shinyWidgets', 
                 'lubridate', 'taxize', 'raster', 'rasterVis', 'tidyverse', 
                 'hash', 'shinycssloaders', 'rgdal', 'shinytoastr', 'shinyalert',
                 'plotly', 'shinyglide', 'cicerone', 'glouton')
lapply(cranbraries, library, character.only = TRUE)


#Now importing packages from Github
# gitlibs_repos <- c('dreamRs/shinypop', 'carlganz/shinyCleave', 'mikejohnson51/AOI', 'mikejohnson51/climateR', 'ColinFay/glouton')
# lapply(gitlibs_repos, devtools::install_github)
gitlibs_names <- c('shinypop', 'shinyCleave', 'AOI', 'climateR', 'glouton')
lapply(gitlibs_names, library, character.only = TRUE)

js <- "$(document).on('shiny:connected', function(event) {
  Shiny.onInputChange('loaded', true)
});"


##Change this if you want the heatmap to automatically update every week.
autoUpdateHeatmap = FALSE

#library(rgdal)
#library(testit)
#DO NOT FORGET TO ADD RESET FOR EVERY YEAR (first day reset to 0).

get_dfWrangled <- function(){
  #Import seasonality database
  AppendixS3_SeasonalityDatabase <- read.csv("./dat/AppendixS3_SeasonalityDatabase.csv", header=TRUE)
  
  #Selecting certain columns and creating mean_* columns 
  dfWrangled <-  as_tibble(AppendixS3_SeasonalityDatabase) %>% 
    dplyr::select(Species, Species.1, BDT.C, EADDC, lat, lon, Author, Year, Journal, Location, quality) %>% 
    group_by(Species.1) %>% 
    mutate(mean_BDT.C = mean(BDT.C, na.rm=TRUE),
           mean_EADDC = mean(EADDC, na.rm=TRUE))
  
  #Remove physiological outliers
  dfWrangled = subset(dfWrangled, dfWrangled$BDT.C > -7 & dfWrangled$EADDC < 2000)
  
  #Restrict to dat with lat / lon
  dfWrangled = dfWrangled[which(!is.na(dfWrangled$lon) & !is.na(dfWrangled$lat) ),]
  dfWrangled$uid <- seq.int(nrow(dfWrangled))  
  return(dfWrangled)}

dfWrangled <- get_dfWrangled()

#Create necessary datarframe for RNOAA query
# latLonDF <- dplyr::select(dfWrangled, c("Species.1", "uid", "lat", "lon"))
# colnames(latLonDF) <- c("Species.1", "id", "latitude", "longitude")
# 
# #Shorten database for ease 
# num_spec = 65
# latLonDF <- head(latLonDF, num_spec)

#Turn each row to a dataframe 
# pLatLonDF <- latLonDF %>% 
#   rowwise %>% 
#   do( X = as_tibble(.) ) %>% 
#   ungroup



#---------------Only run the following if you want to update ghcnd-stations.txt------------
#This will query NOAA for the most up to date info on weather stations in the GHCND network
#The output is saved to a file called ghcnd-stations-current.csv, in the current working directory
updateGHCNDStations <- function(){
  print("Getting ghcnd-stations.txt from NOAA...")
  stationsDailyRaw <- read.fwf(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"),
                               widths = c(11, 9, 11, 7, 2, 31, 5, 10),
                               header = FALSE, strip.white = TRUE, comment.char = "",
                               stringsAsFactors = FALSE)
  print("Getting ghcnd-inventory.txt from NOAA...")
  inventoryDailyRaw <- read.fwf(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-inventory.txt"),
                                widths = c(11, 9, 10, 5, 5, 5),
                                header = FALSE, strip.white = TRUE, comment.char = "",
                                stringsAsFactors = FALSE)
  stationColNames <- c("id","latitude", "longitude", "elevation",
                       "state", "name", "gsn_flag", "wmo_id")
  inventoryColNames <- c("id","latitude", "longitude",
                         "element", "first_year", "last_year")
  
  ghcndStationsDaily <- stats::setNames(stationsDailyRaw, stationColNames)
  ghcndInventoryDaily <- stats::setNames(inventoryDailyRaw, inventoryColNames)
  
  ghcndStationsDailyComplete <- merge(ghcndStationsDaily, ghcndInventoryDaily[, -c(2, 3)], by = "id")
  
  sturdyGHCNDStations <- tibble::as_tibble(ghcndStationsDailyComplete[stats::complete.cases(ghcndStationsDailyComplete), ])
  
  saveRDS(sturdyGHCNDStations, file = "./dat/ghcnd-stations-current.csv")
  
  return(sturdyGHCNDStations)}

#--------End Update of GHCND Stations----------

#read in current local copy of ghcnd stations 
localGHCNDStations <- readRDS(file = "./dat/ghcnd-stations-current.csv")

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
# stationLatLonDf <- pLatLonDF %>% 
#   mutate(result = map(X, nearestStat)) %>% 
#   unnest(cols = c(X, result)) %>%
#   dplyr::select("Species.1", "id", "result") %>% 
#   rename(uid = id) %>% 
#   unnest(cols = c(result)) %>% 
#   rename(sid = id) %>% 
#   dplyr::select("uid", "sid")
# stationLatLonDf$rank <- rep(c(1,2), length.out = nrow(stationLatLonDf))
# stationLatLonDf <- spread(stationLatLonDf, rank, sid)
# colnames(stationLatLonDf) <- c("uid", "sid1", "sid2")

#Merge weather dataframe with species dataframe
#speciesStationDF <- merge(x = dfWrangled, y = stationLatLonDf, by = "uid")

#Removes observations with no nearby weather stations (<500 miles) as defined by radius argument in nearestStat
#speciesStationDF <- speciesStationDF[!(is.na(speciesStationDF$sid1) || speciesStationDF$sid1==""), ]

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
  if(is.na(Tmin) || is.na(Tmax) || is.na(Tmin) || is.na(Tmax)){dd = NA}
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
dd_plot <- function(tMax1, tMax2, 
                    tMin1, tMin2, 
                    BDT, EADDC, 
                    startTime, endTime = Sys.Date(),
                    species, 
                    lat = NULL, lon = NULL, 
                    breaks = NULL, 
                    dateformat='%m/%y',
                    locality = NULL) {
  UseMethod("dd_plot")
}

dd_plot.default <- function(tMax1, tMax2, tMin1, tMin2, BDT, EADDC, startTime, endTime, species, lat = NULL, lon = NULL, breaks = NULL, dateformat='%m/%y', locality = NULL) {
  
  #Making a new dataframe that has all of tMax1 and tMax2 for missing dates
  if(!is.null(tMax1) && !is.null(tMax2) && !is.null(tMin1) && !is.null(tMin2)) {
    dfTMAX <- rbind(tMax1, tMax2[!tMax2$date %in% tMax1$date,])
    dfTMIN <- rbind(tMin1, tMin2[!tMin2$date %in% tMin1$date,])
  } else {
    # dfTMAX <- rbind(tMax1, tMax2)
    # dfTMIN <- rbind(tMin1, tMin2)
    if(!is.null(tMax1) && !is.null(tMin1)){
      dfTMAX <- tMax1 
      dfTMIN <- tMin1
    }else{
      dfTMAX <- tMax2 
      dfTMIN <- tMin2
    }
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

  dfTEMP$TMAX[which(dfTEMP$TMAX > 200)] = NA
  dfTEMP$TMIN[which(dfTEMP$TMIN > 200)] = NA
  dfTEMP$TMAX[which(dfTEMP$TMAX < -200)] = NA
  dfTEMP$TMIN[which(dfTEMP$TMIN < -200)] = NA

  

  dfTEMP <- na.omit(dfTEMP)
  dfTEMP$dd <- NA
  for (i in 1:nrow(dfTEMP)) {
    dd = degree.days.mat(dfTEMP$TMIN[i], dfTEMP$TMAX[i], BDT)
    dfTEMP$dd[i] <- dd
  }

  dDays <- dfTEMP
  #Adding a csum column which sums degree days and resets after reaching threshold (EADDC)
  dDays$csum <- cumsum_with_reset(dDays$dd, EADDC)
  
  #Plot csum vs date
  # plot <-  ggplot(dDays, aes(date, csum)) +
  #   plot_template(df, breaks, dateformat) +
  #   #ncdc_theme() +
  #   geom_hline(aes(yintercept = EADDC), linetype = "dashed", color = "#ff7729", size = 1) +
  #   geom_text(aes(startTime, EADDC, label = "EADDC", vjust = +1.5, hjust = -0.1), size = 4) 
  
  

  if(!is.null(locality)){
    species_locality <- locality
  }
  
  #------------------
  if(!is.null(lat) && !is.null(lon)){
  observationLocationLookup <- geocode_rev(c(lat, lon))
  species_locality <- str_c(observationLocationLookup$county, ", ", observationLocationLookup$state, ", ", toupper(observationLocationLookup$country_code))
  }
  
  #Start the plot and add the three variables to be plotted (date, csum, dd)
  fig <- plot_ly(dDays, x = ~date) %>% 
    add_lines(y = ~csum, name = "Accumulated Degree Days") %>% 
    add_lines(y = ~dd, name = "Individual Degree Days", fill = 'tozeroy')
  
  #Add generation dashed line
  datesAdulthoodReached <- dDays$date[which(dDays$csum == EADDC)]
  if(length(datesAdulthoodReached) != 0){
    fig <- fig %>% 
      add_segments(x = datesAdulthoodReached +1, 
                   xend = datesAdulthoodReached +1, 
                   y = 0, 
                   yend = EADDC*1.05, 
                   name = "Eggs Reached Adulthood",
                   line = list(dash = "dash"))
    
    #Add horizontal dashed line at EADDC
    fig <- fig %>% 
      add_segments(x = startTime, 
                   xend = endTime, 
                   y = EADDC, 
                   yend = EADDC, 
                   name = "EADDC",
                   line = list(dash = "dash"))}
  
  #Beautify and add date selectors
  fig <- fig %>% layout(
    title = str_c(paste(species, collapse = ', '), " development in ", species_locality),
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 1,
            label = "1 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 mo",
            step = "mo",
            stepmode = "backward"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"))),
      
      rangeslider = list(type = "date")),
  
    yaxis = list(title = "Degree Days (\u00B0C)")
    
    # annotations = 
    #   list(x = 1, y = -0.1, text = "Source: data I found somewhere.", 
    #        showarrow = F, xref='paper', yref='paper', 
    #        xanchor='right', yanchor='bottom', xshift=0, yshift=0,
    #        font=list(size=15, color="red"))
    )
  #------------------
  
  return(fig)
  #scale_y_continuous(breaks = sort(c(ggplot_build(plot1)$layout$panel_ranges[[1]]$y.major_source, h)))
}


# dd_plot.default <- function(tMax1, tMax2, tMin1, tMin2, BDT, EADDC, startTime, breaks = NULL, dateformat = '%d/%m/%y') {
#   stop("No method for ", class(list(tMax1)[[1]]), call. = FALSE)
# }

# plot_template <- function(df, breaks, dateformat) {
#   tt <- list(
#     theme_minimal(base_size = 18),
#     geom_line(size = 1),
#     labs(y = "Accumulated Degree Days", x = "Date (MM/YY)")
#   )
#   if (!is.null(breaks)) {
#     c(tt, scale_x_date(date_breaks = breaks, date_labels = dateformat))
#   } else {
#     tt
#   }
# }
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

#-----Get rasterStack of accumulated DD for current year, by week------------
#-----Also saves copy to dat folder with name species_name.grd or end_date if no species is provided
#--Note: degree.days.mat(tmin, tmax, BDT) must be declared prior to execution
#Optional arguments:
# Note: BDT and EADDC arguments must be specified if species is not specified 
#       - start_date: a date to begin accumulation at
#       - end_date: a date to stop accumulation at (default: two days ago)
#       - BDT: either an integer BDT value or a vector of values to be averaged    
#       - EADDC: either an integer EADDC value or a vector of values to be averaged    
#       - cum_DD: a rasterLayer containing cumulative Degree Day values on the start date
#                 (Degree days will begin accumulating from here, otherwise they start at 0)
#       - species: a string species name that will be queried to get mean BDT and EADDC values from ./dat/AppendixS3_SeasonalityDatabase.csv
accumulateDD <- function(start_date = as.Date(str_c(year(Sys.Date()), '-01-01')), end_date = Sys.Date() -2, BDT = NULL, EADDC = NULL, cum_DD = NULL, species = NULL){
  #Define area of interest 
  print(cum_DD)
  if(!is.null(cum_DD)){
    print("Matching start date with layer")
    start_date <- str_replace_all(sub('.', '', last(names(cum_DD))), "[/.]", "-")
  }
  if(!is.Date(start_date)){start_date <- as.Date(start_date)}
  if(!is.Date(end_date)){end_date <- as.Date(end_date)}
  print(start_date)
  
  if((is.null(BDT) || is.null(EADDC)) && is.null(species)){return("Please provide BDT and EADDC arguments, or a species to query")}
  if(!is.null(species)){
    toAccumulate <- get_dfWrangled() %>% filter(Species == species)
    print(str_c("Species selected: ", species))
    print("BDT Values Found: ")
    print(toAccumulate$BDT.C)
    BDT <- mean(toAccumulate$BDT.C)
    print(str_c("Average BDT: ", BDT))
    print("EADDC Values Found: ")
    print(toAccumulate$EADDC)
    EADDC <- mean(toAccumulate$EADDC)
    print(str_c("Average EADDC: ", EADDC))}
  #Find means of BDT and EADDC if vector of either is passed in
  if(length(BDT) > 1){
    print(str_c("Averaging BDTs: ", BDT))
    BDT <- mean(BDT)
    print(str_c("Average BDT: ", BDT))}
  if(length(EADDC) > 1){
    print(str_c("Averaging EADDCs: ", EADDC))
    EADDC <- mean(EADDC)
    print(str_c("Average EADDC: ", EADDC))}
  print(str_c("BDT: ", BDT, ", EADDC: ", EADDC))
  AOI = aoi_get(state = "conus")
  #Get temp raster stack for start_date
  #raster::plot(AOI)
  p = getGridMET(AOI, param = c('tmax','tmin'), startDate = start_date)
  r = raster::stack(p$tmax, p$tmin)
  names(r) = c('tmax', 'tmin')
  #print("not above here")
  #Initialize cum_DD to DD values for start_date
  if(is.null(cum_DD)){
    #print("initializing")
    pastStack <- NULL
    cum_DD <- calc(r, fun = function(x){
      #print(value(x[2]))
      degree.days.mat(value(x[2]) -273.15, value(x[1]) -273.15, BDT)})
  }else{
    pastStack <- cum_DD
    cum_DD <- raster(pastStack, layer = nlayers(pastStack))}
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
      if(!is_null(pastStack)){
        the_stack <- raster::stack(pastStack, cum_DD)
      }else{
      the_stack <- raster::stack(cum_DD)}
      print(str_c("the_stack: ",the_stack))
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
  
  #Update ./dat/availablePhenoSpecies.csv and ./dat/phenoSpeciesEADDC.csv with new species entry
  if(!is.null(species)){
    filePath <- str_c("./dat/", make.names(species), ".grd")
    availablePhenoSpecies <- read_rds("./dat/availablePhenoSpecies.csv")
    if(!is.null(availablePhenoSpecies[[species]])){availablePhenoSpecies[[species]] <- NULL}
    else{
      speciesEADDC_Dict <- read_rds("./dat/phenoSpeciesEADDC.csv")
      speciesEADDC_Dict[[filePath]] <- EADDC
      write_rds(speciesEADDC_Dict, "./dat/phenoSpeciesEADDC.csv")
      
      speciesBDT_Dict <- read_rds("./dat/phenoSpeciesBDT.csv")
      speciesBDT_Dict[[filePath]] <- BDT
      write_rds(speciesBDT_Dict, "./dat/phenoSpeciesBDT.csv")}
    #Add species to available list
    availablePhenoSpecies <- append(availablePhenoSpecies, filePath)
    print(str_c("Saving species: ", species))
    names(availablePhenoSpecies)[length(availablePhenoSpecies)] <- str_c(species)
    write_rds(availablePhenoSpecies, "./dat/availablePhenoSpecies.csv")}
  else{filePath <- str_c("./dat/", make.names(current_date), ".grd")}
  
  #Save the raster to the dat folder
  print(str_c("Writing raster: ", the_stack))
  saveRDS(the_stack, filePath)
  
  
  
  return(the_stack)
  #raster::plot(newR)
}


#If user selects week in day, here's the update function to accumulate more degree days
accumulateDDPart <- function(start_date, end_date = Sys.Date() -2, BDT, EADDC, cum_DD = NULL){
  #Define area of interest 
  if(!is.Date(start_date)){start_date <- as.Date(start_date)}
  if(!is.Date(end_date)){end_date <- as.Date(end_date)}
  
  AOI = aoi_get(state = "conus")
  #Get temp raster stack for start_date
  #raster::plot(AOI)
  print(str_c("Accumulating degree days starting at " , start_date, " until ", end_date, "..."))
  print(str_c("BDT: ", BDT, ", EADDC: ", EADDC))
  
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
  print("Initial raster layer: ")
  print(cum_DD)

  #Set current day to the next start day
  week <- 1
  current_date = start_date + 1
  #Accumulate Degree Days from current_date to end_date, inclusive.
  while(current_date <= end_date){
    #Get raster stack of tmin and tmax for current day
    temps = getGridMET(AOI, param = c('tmax','tmin'), startDate = current_date)
    tstack = raster::stack(temps$tmax, temps$tmin)
    names(tstack) = c('tmax', 'tmin')
    print(str_c("Accumulating ", current_date))
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

#-----Define species to visualize phenology with, locations of species' rasterStacks, and EADDC/BDT values used in computation of rasterStack
speciesEADDC_Dict <- read_rds("./dat/phenoSpeciesEADDC.csv")
speciesBDT_Dict <- read_rds("./dat/phenoSpeciesBDT.csv")
#Species name and corresponding filename
availablePhenoSpecies <- read_rds("./dat/availablePhenoSpecies.csv")

#--This function checks the age of the phenology maps for species we have and updates them if they are over a week old.
#----availableSpecies: an optional list of species names and locations of phenology grids for corresponding species, it defaults to reading the current availablePhenoSpecies list.
updatePhenology <- function(availableSpecies = read_rds("./dat/availablePhenoSpecies.csv")){
  lapply(seq_along(availableSpecies), function(i){
    #Get name and filePath values from availableSpecies list at index i
    name <- names(availableSpecies)[[i]]
    filePath <- availableSpecies[[i]]
    toUpdate <- readRDS(availableSpecies[[i]])
    last_update <- as.Date(str_replace_all(sub('.', '', last(names(toUpdate))), "[/.]", "-"))
    currentDay <- Sys.Date() -2
    #Calculate file age (in days) after fetching the last date it was modified
    #last_update <- as.Date(file.info(availableSpecies[[i]])$mtime)
    print(last_update)
    file_age <- as.double.difftime(currentDay - last_update)
    
    thisYear <- format(Sys.Date(), '%Y')
    
    #If the file is from the previous year and we have gridMET data from this year, delete the file and start a new one.
    if(!(format(last_update, '%Y') == thisYear) && (Sys.Date() -2 >= as.Date(str_c(thisYear, "-01-01")))){
      print(str_c("Creating a new phenology record for ", name, " in ", thisYear))
      
      accumulateDD(as.Date(str_c(thisYear, "-01-01")), 
                   Sys.Date() - 2, 
                   species = name)
      
    }else{
      #If the file is at least a week old and we have gridMET data for this year, update it
      if((file_age >= 7) && (Sys.Date() -2 >= as.Date(str_c(thisYear, "-01-01")))){
        print(str_c("Updating ", name))
        accumulateDD(end_date = Sys.Date() - 2, 
                     species = name,
                     cum_DD = toUpdate)
        return(str_c(name, " was ", file_age, " days old. It is now 2 days old, due to gridMET restrictions."))
      } else return(str_c(name, " was modified less than a week ago (", file_age, " days). It will be updated in ", (7 - file_age), " days."))
    }})}

#Execute function every time the app starts up to make sure files are up to date: 

if(autoUpdateHeatmap) print(updatePhenology())
#--------------------------------------------------------------------------------

#----------Guided walkthrough------------------
guide <- Cicerone$
  new(allow_close = TRUE)$
  step(
    "viz-wrapper",
    "Visualization Tool",
    "Welcome to our insect phenology visualization tool. There are many ways you can interact with our model. Click next to get started!"
  )$
  step(
    "mymap",
    "The Phenopause Heatmap",
    "The colored layer on the map represents current insect development in the US. 
    For more information on how we modeled this check out the introduction above. Now, let's continue to see what the layer is actually displaying."
  )$
  step(
    "heatmap-species-wrapper",
    "Selecting a species",
    "This tells you what species is currently depicted on the heatmap. Viewing a different species' heatmap is as simple as selecting it here."
  )$
  step(
    "heatmap-date-wrapper",
    "Changing the date",
    "Visualizing the development of your selected species at a different date (this year), can easily be accomplished here."
  )$
  step(
    "phnInf",
    "Heatmap layer info",
    "Here we see the values of the two ecological parameters used in the modeling of this species' springtime phenology. 
    We also see the current date of the layer being displayed. You may notice that the date is not the date you selected in the last step. More info on that next."
  )$
  step(
    "heatmap-resolution-wrapper",
    "Heatmap date accuracy",
    "The colors displayed on the map are computed weekly, and a record of each week is stored for quick viewing. 
    By default, when a date is selected the data that is displayed is that of the closest available weekly data. 
    If you are after greater modeling accuracy, changing this will recalculate heatmap values to the day."
  )$
  step(
    "subspecies",
    "Species descriptions",
    "A description of the current species can be found here. 
    Now, let's continue to learn about plotting phenology data for over 650 insect species."
  )$
  step(
    "mymap",
    "Species observation markers",
    "Our thermal ecology dataset currently contains 1,493 observations of 678 unique insect species. 
    Let's explore it! First, select the 'Observations' layer checkbox in the top-right corner of the map.
    Each appearing blue marker represents a location where an insect species was observed. 
    Click a blue marker to see the species' name and its unique thermal parameters, experimentally determined by researchers at that location."

  )$
  step(
    "observation-filter-wrapper",
    "Adding and removing markers",
    "You can use the tool in this tab to control which species have visible observation markers. Now, continue to view the phenology plot for our selected observation."
  )$
  step(
    "observation-plot-wrapper",
    "Species observation plots",
    "Woah, a new tab appeared! This is the current insect development plot for your selected insect observation. 
    We use weather data from stations in the GHCND network near the location where the insect was observed in conjunction with observed thermal parameters (BDT and EADDC).
    The result is a plot depicting accumulated degree days (a proxy for development) as well as individual degree days, which is summed to produce accumulated degree days.
    An insect is expected to reach adulthood when accumulated degree days equals their experimentally determined EADDC threshold (red line). 
    Green lines mark the beginning of a new generation, and the assumption was made that this happens immediately after a species reaches adulthood."
  )$
  step(
    "observation-dates-wrapper",
    "Changing the date range",
    "This date range field makes it simple to change the x-axis date range. 
    Degree days are summed from the beginning of the date range, which defaults to January 1st of this year (winter in the Northern Hemisphere)."
  )$
  step(
    "plotting-assistant-wrapper",
    "Phenology plotting tool",
    "Congratulations, you're ready to start using our visualization. As a final note, I'd like to highlight our new phenology plotting tool. 
    It makes visualizing the current, local development of any species in our database simple for any United States zipcode. "
  )
#---------------------------------------------


#-----It's the user interface! (What the user sees)-------
ui <- fluidPage(
  useToastr(),
  useShinyalert(),
  use_glouton(),
  use_cicerone(),
  tags$script(js),
  tags$head(tags$style(".modal-dialog{ width:80%}")),
  verticalLayout(
    includeMarkdown('intro.md'),
    div(
      id = "plotting-assistant-wrapper",
      hr(),
      p("New Feature: The phenology plotting tool provides a current/historical spring phenology plot for an insect species of your choice at a desired location (USA only), give it a try: "),
      actionBttn("miniPlotter", "Phenology Plotting Assistant", style = "fill", color = "primary"),
      p(""),
      hr()),
    includeMarkdown('intro2.md'),
    #headerPanel('Insect Phenology Visualization'),
    div(
      id = "viz-wrapper",
      sidebarLayout(
      tabsetPanel(id = "tabset",
                  tabPanel("Phenology Heatmap Controls", value = "Phen", sidebarPanel(
                    div(
                      id = "heatmap-species-wrapper",
                      tags$b("Select a species"),
                      helpText("Change the species shown on the heatmap."),
                      selectInput(inputId = "phenoSpecies",
                                  label = NULL,
                                  choices = availablePhenoSpecies,
                                  multiple = FALSE,
                                  selectize = TRUE)),
                    div(
                      id = "heatmap-date-wrapper",
                      tags$b("Change heatmap date"),
                      helpText("Change the date shown on the heatmap, up to two days ago. The map depicts accumulated degree days from the first day of this year until this date."),
                      dateInput(inputId = "phenoDate", 
                                label = NULL,
                                value  = Sys.Date()-2,
                                min    = as.Date(str_c(year(Sys.Date()), '-01-01')),
                                max = Sys.Date()-2,
                                format = "mm/dd/yy")),
                    verbatimTextOutput(outputId = "phnInf", placeholder = FALSE),
                    div(
                      id = "heatmap-resolution-wrapper",
                      tags$b("Advanced: Alter heatmap resolution"),
                      helpText("Change the accuracy of the heatmap. This may take several minutes to load if changed. 'Week' (default) will accumulate degree days to within a week of the selected date, while 'Day of Week' accumulates to the day."),
                      radioButtons(inputId = "computeDD",
                                   label = NULL,
                                   choiceNames = c("Day of Week", "Week"),
                                   choiceValues = c(TRUE, FALSE),
                                   selected = FALSE)),
                    helpText("More information about the species in this heatmap is available in the 'Heatmap Species Info' tab below."),
                    tags$b("If you're stuck... "),
                    actionBttn("tour", "Take a Tour!", style = "float", color = "primary", block = TRUE))),
                  tabPanel("Filter species observation markers", value = "Obs", sidebarPanel(
                    div(
                      id = "observation-filter-wrapper",
                      tags$b('Add or remove species observation markers from the map'),
                      helpText("Click on a species to add or remove its corresponding observation marker from the map. All records are visible by default."),
                      multiInput('sel_species',
                                 NULL,
                                 choices = as.vector(unique(dfWrangled$Species)),
                                 selected = unique(dfWrangled$Species)),
                      actionButton("all", "All"),
                      actionButton("none", "None"))
                    #actionBttn("miniPlotter", "Phenology Plotting Assistant", style = "fill", color = "primary"),
                    # dateRangeInput(inputId = "dateRange", 
                    #                label = "Change date range for plot: ",
                    #                start  = floor_date(Sys.Date(), "year"),
                    #                min    = "1970-01-01",
                    #                format = "mm/dd/yy",
                    #                separator = " - ")
                    #verbatimTextOutput(outputId = "obsInf", placeholder = FALSE) %>% withSpinner(color = "#228B22")))
                  ))),
      mainPanel(
        leafletOutput("mymap", height = 600) %>% withSpinner(color = "#228B22"),
        helpText("Map markers from the toggleable species 'Observations' layer can be clicked. When selected, a phenology plot is rendered in the 'Observation Plot' tab below.")
      ))),
    hr(),
    tabsetPanel(id = "tabsetSupport", 
                tabPanel(title = "Heatmap Species Info", 
                         value = "Phen2",
                         htmlOutput(outputId = "subspecies")),
                tabPanel(title = "Observation Plot", 
                         value = "Obs2",
                         div(
                           id = "observation-plot-wrapper",
                           helpText("If the plot is not loaded, click a marker on the map to load that species' phenology plot below."),
                           plotlyOutput("predPlot") %>% withSpinner(color = "#228B22"),
                           textOutput("obsPltInf"),
                           div(
                             id = "observation-dates-wrapper",
                             dateRangeInput(inputId = "dateRange", 
                                            label = "Change x-axis date range: ",
                                            start  = floor_date(Sys.Date(), "year"),
                                            min    = "1970-01-01",
                                            format = "mm/dd/yy",
                                            separator = " - ")),
                           helpText("Note: To plot springtime phenology for an observation located in the Southern Hemisphere, change the starting date to 6/1. This makes visualizing Southern Hemisphere springtime development possible, and is needed due to differences in the timing of spring.")))), 
    br(),
    hr()
  )
)



#------Here is the server for the shiny app (How the page becomes responsive)--------
server <- function(input, output, session){
  
  #If its your first time on the website, we offer you a guided walkthrough (use cookies to check)
  observeEvent(input$loaded, {
    # get cookie
    visited <- fetch_cookies()
    print(visited$visited_site)
    
    #Uncomment next line to debug new user welcome alert
    #visited$visited_site <- NULL
    
    # if null set cookie
    # otherwise show guide
    if(is.null(visited$visited_site)){
      add_cookie("visited_site", "yes")
      shinyalert(
        title = "Welcome!",
        text = "Welcome to the Trench Project's springtime insect phenology visualization tool. It looks like this is your first visit, would you like a walkthrough of the tool? ",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "Take the Tour!",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "No Thanks!",
        timer = 0,
        imageUrl = "https://www.festivalclaca.cat/imgfv/b/537-5374275_bee-insect-drawing-flying-wings-isolated-honeybee-free.png",
        imageWidth = 100,
        imageHeight = 100,
        animation = TRUE, 
        callbackR = function(x){if(x != FALSE){guide$init()$start()}})
    }
    
  })
  
  observeEvent(input$tour, guide$init()$start())
  
  
  #Create a reactive dataframe which changes based on the selected species from the multi input tool
  lat_long_df <- reactive({
    x <- dfWrangled %>% 
      filter(Species %in% input$sel_species) 
  })
  
  #Listen for a button click (all or none) and change selections accordingly
  observeEvent(input$all, {
    updateMultiInput(
      session = session,
      inputId = "sel_species",
      selected = unique(dfWrangled$Species)
    )
  })
  observeEvent(input$none, {
    updateMultiInput(
      session = session,
      inputId = "sel_species",
      selected = character(0)
    )
  })
  
  #observe(print(input$zipcode))
  
  gatherWeatherData <- function(timeRange, station1 = NULL, station2 = NULL){
    time <- timeRange
    
    # if(!is.null(uid) && is.null(station1) && is.null(station2)){
    #   station1 = speciesStationDF$sid1[uid]
    #   station2 = speciesStationDF$sid2[uid]}
    
    tMax1 <- ncdc(datasetid='GHCND',
                  stationid= paste0('GHCND:', station1),
                  datatypeid= "TMAX",
                  startdate = time[1],
                  enddate = time[2],
                  limit=500,
                  token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
    
    tMax2 <- ncdc(datasetid='GHCND',
                  stationid= paste0('GHCND:', station2),
                  datatypeid= "TMAX",
                  startdate = time[1],
                  enddate = time[2],
                  limit=500,
                  token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
    if(nrow(tMax1$data)==0){tMax1 <- NULL}else{tMax1 <- tMax1$data %>% dplyr::select(date, value) %>% rename(TMAX = value)}
    
    if(nrow(tMax2$data)==0){tMax2 <- NULL}else{tMax2 <- tMax2$data %>% dplyr::select(date, value) %>% rename(TMAX = value)}
    
    tMin1 <- ncdc(datasetid='GHCND',
                  stationid= paste0('GHCND:', station1),
                  datatypeid= "TMIN",
                  startdate = time[1],
                  enddate = time[2],
                  limit=500,
                  token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
    
    tMin2 <- ncdc(datasetid='GHCND',
                  stationid= paste0('GHCND:', station2),
                  datatypeid= "TMIN",
                  startdate = time[1],
                  enddate = time[2],
                  limit=500,
                  token="HnmvXmMXFNeHpkLROUmJndwOyDPXATFJ")
    if(nrow(tMin1$data)==0){tMin1 <- NULL}else{tMin1 <- tMin1$data %>% dplyr::select(date, value) %>% rename(TMIN = value)}
    
    if(nrow(tMin2$data)==0){tMin2 <- NULL}else{tMin2 <- tMin2$data %>% dplyr::select(date, value) %>% rename(TMIN = value)}
    
    return(list("tMin1" = tMin1, "tMin2" = tMin2, "tMax1" = tMax1, "tMax2" = tMax2))
  }
  
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
      updateTabsetPanel(session, "tabset", selected = "Obs")
      updateTabsetPanel(session, "tabsetSupport", selected = "Obs2")
      
      time <- timeRange()
      output$obsInf <- renderPrint(tryCatch(safeSci2Com(dfWrangled$Species[uid]), 
                                            error = function(e){
                                              toastr_warning("No common name detected")
                                              return(dfWrangled$Species[uid])}))
      speciesInfo <- dfWrangled[uid, ]
      
      speciesWeather <- speciesInfo %>% 
        ungroup() %>% 
        select(uid, lat, lon) %>% 
        rename(latitude = lat,
                 longitude = lon,
                 id = uid) 
        
      speciesWeather <- nearestStat(speciesWeather)[[1]]
      
      weather <- gatherWeatherData(timeRange = time, station1 = speciesWeather$id[1], station2 = speciesWeather$id[2])
      
      #weather <- gatherWeatherData(uid, time)
      #speciesStationDF <- speciesStationDF[!(is.na(speciesStationDF$sid1) || speciesStationDF$sid1==""), ]
      
      if((!is.null(weather$tMax1) && !is.null(weather$tMin1)) || (!is.null(weather$tMax2) && !is.null(weather$tMin2))){
        output$predPlot <- renderPlotly({
        dd_plot(tMax1 = weather$tMax1, 
                tMax2 = weather$tMax2, 
                tMin1 = weather$tMin1, 
                tMin2 = weather$tMin2, 
                dfWrangled$BDT.C[uid],
                dfWrangled$EADDC[uid],
                time[1],
                time[2],
                species = dfWrangled$Species[uid],
                lat = dfWrangled$lat[uid],
                lon = dfWrangled$lon[uid],
                breaks="1 month",
                dateformat="%m/%y")})
        output$obsPltInf <- renderText(str_c("Source of thermal data for ", dfWrangled$Species[uid], ": ", dfWrangled$Author[uid], ", ", dfWrangled$Year[uid], ", ", dfWrangled$Journal[uid]))}
      else {
        toastr_error("No weather data found", "Error Plotting", timeOut = 10000)
        toastr_info("Try choosing a different observation or date range", timeOut = 10000)
        output$obsInf <- renderPrint("No current RNOAA data available here.")}
    } else {
      output$obsInf <- renderPrint("Select an observation from the map.")
    }
  })
  
  #-------Create map, add circle markers and popup-------
  phenDate <- reactive({
    x <- input$phenoDate
  })
  
  #Render the correct map for viewing
  output$mymap <- renderLeaflet({
    df <- lat_long_df()
    map <- leaflet(data = df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addLayersControl(#baseGroups = c("Heatmap"),
                       overlayGroups = c("Observations"),
                       options = layersControlOptions(collapsed = FALSE, 
                                                      autoZIndex = TRUE)) %>% 
      hideGroup("Observations") %>% 
      addCircleMarkers(lng = ~lon,
                       lat = ~lat,
                       radius = 2.5,
                       group = "Observations",
                       layerId = ~uid,
                       popup = paste("<em>",df$Species,"</em>", "<br>",
                                     #sci2comm(df$Species)[[1]][1], "<br>",
                                     "<b> EADDC: </b>", round(df$EADDC, digits=2), "<br>",
                                     "<b> BDT.C: </b>", round(df$BDT.C, digits=2))) %>% #,
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
    
    
    dateR <- phenDate()
    
    # getLegendLabels <- function(type, ...){
    #   switch(type, numeric = (function(cuts) {
    #     paste0(prefix, formatNum(cuts), suffix)
    #   })(...), bin = (function(cuts) {
    #     n <- length(cuts)
    #     labels <- paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), 
    #            suffix)
    #   })(...), factor = (function(cuts) {
    #     paste0(prefix, as.character(transform(cuts)), suffix)
    #   })(...))
    #   
    #   print(labels)
    #   return(labels)
    # }
    
    #Add the correct raster, based on dateR (the user selected date), calculating and displaying the daily raster if desired
    addSpeciesRaster <- function(speciesPhenStack, EADDC, BDT){
      updateTabsetPanel(session, "tabset", selected = "Phn")
      updateTabsetPanel(session, "tabsetSupport", selected = "Phn2")
      
      if(is.wholenumber(EADDC)){EADDC = EADDC + 0.001}
      pal <- colorBin(c('transparent', '#4376c7', '#59711b', '#ffc324', '#ff7729'), 
                      c(0, EADDC), 
                      bins = c(0, round(0.25 * EADDC), round(0.5 * EADDC), round(0.75 * EADDC), floor(EADDC), ceiling(EADDC)),
                      na.color = "transparent")
      
      if(input$computeDD){if(reduce(names(speciesPhenStack) %in% str_c('X', gsub('-', '.', dateR)), sum) == 1){
        #output$phnInf <- renderPrint("Image rendering...")
        toastr_info("Heatmap rendering...")
        toView <- raster(speciesPhenStack, layer = which(names(speciesPhenStack) %in% str_c('X', gsub('-', '.', dateR))))
        map <- addRasterImage(map, toView, colors = pal, group = "Heatmap", opacity = 0.6) %>% 
          addLegend(pal = pal,
                    values = c(0, EADDC),
                    group = "Heatmap",
                    # labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 99%", ">99%"),
                    position = "bottomright",
                    title = "Cumulative Degree Days") #%>% 
          # addLayersControl(baseGroups = c("Heatmap", "Observations"),
          #                  options = layersControlOptions(collapsed = FALSE))
        output$phnInf <- renderText(paste(str_c("Current heatmap date: ", dateR), 
                                          str_c("BDT: ", BDT),
                                          str_c("EADDC: ", EADDC),
                                          sep = "\n"))
        toastr_success("Heatmap layer rendered successfully")
      }else{
        shinyalert(
          title = "Caution",
          text = "This may take several minutes. Are you sure you want to proceed?",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Proceed",
          confirmButtonCol = "#AEDEF4",
          cancelButtonText = "Cancel",
          timer = 0,
          imageUrl = "",
          animation = TRUE, 
          callbackR = function(x){if(x != FALSE){
            toastr_warning("Computing image...", timeOut = 20000)
            tempDate <- dateR
            while(reduce(names(speciesPhenStack) %in% str_c('X', gsub('-', '.', tempDate)), sum) != 1){
              tempDate <- tempDate - 1}
            output$phnInf <- renderPrint(str_c("Accumulating from: ", tempDate))
            toAccum <- raster(speciesPhenStack, layer = which(names(speciesPhenStack) %in% str_c('X', gsub('-', '.', tempDate))))
            if(Sys.Date() -2 < dateR){dateR <- Sys.Date() -2} 
            toView <- accumulateDDPart(tempDate, dateR, BDT = BDT, EADDC = EADDC, cum_DD = toAccum)
            map <- addRasterImage(map, toView, colors = pal, group = "Heatmap", opacity = 0.6) %>% 
              addLegend(pal = pal,
                        values = c(0, EADDC),
                        # labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 99%", ">99%"),
                        group = "Heatmap",
                        position = "bottomright",
                        title = "Cumulative Degree Days") #%>% 
            # addLayersControl(baseGroups = c("Heatmap", "Observations"),
            #                  options = layersControlOptions(collapsed = FALSE))
            output$phnInf <- renderText(paste(str_c("Current heatmap date: ", dateR), 
                                              str_c("BDT: ", BDT),
                                              str_c("EADDC: ", EADDC),
                                              sep = "\n"))
            toastr_success("Image computed successfully")
          }else{
            updateRadioButtons(session, "computeDD", selected = FALSE)
            }})
      }}else{
        toastr_info("Heatmap rendering...")
        tempDate <- dateR
        while(reduce(names(speciesPhenStack) %in% str_c('X', gsub('-', '.', tempDate)), sum) != 1){
          tempDate <- tempDate - 1}
        toView <- raster(speciesPhenStack, layer = which(names(speciesPhenStack) %in% str_c('X', gsub('-', '.', tempDate))))
        map <- addRasterImage(map, toView, colors = pal, group = "Heatmap", opacity = 0.6)  %>% 
          #Experiment here... 
          addLegend(pal = pal,
                    values = c(0, EADDC),
                    # labels = c("0 - 25%", "25 - 50%", "50 - 75%", "75 - 99%", ">99%"),
                    labFormat = labelFormat(),
                    group = "Heatmap",
                    position = "bottomright",
                    title = "Cumulative Degree Days") #%>% 
        # addLayersControl(baseGroups = c("Heatmap", "Observations"),
        #                  options = layersControlOptions(collapsed = FALSE))
        output$phnInf <- renderText(paste(str_c("Current heatmap date: ", tempDate), 
                                          str_c("BDT: ", BDT),
                                          str_c("EADDC: ", EADDC),
                                          sep = "\n"))
        toastr_success("Heatmap rendered successfully")
        
      }
      return(map)}
    
    #Add the species raster to the map (calls addRasterImage based on selected species and computes current day if chosen)
    map <- addSpeciesRaster(speciesPhenStack = raster::stack(readRDS(input$phenoSpecies)), 
                            EADDC = speciesEADDC_Dict[[input$phenoSpecies]],
                            BDT = speciesBDT_Dict[[input$phenoSpecies]])
    map
  })
  
  #Render text about selected species
  observe({
    selected_species <- names(availablePhenoSpecies)[availablePhenoSpecies == input$phenoSpecies]
    filepath <- str_c("./dat/species-overviews/", make.names(selected_species), ".md")
    #wp_content <- page_content("en", "wikipedia", page_name = "Codling moth")$parse
    #output$subspecies <- renderText(wp_content$text)
    output$subspecies <- renderText(includeMarkdown(filepath))
  })
  
  #Show UI controls for selected map group
  observe({
    selected_group <- req(input$mymap_groups)
    
    if("Observations" %in% selected_group){
      showTab("tabset", "Obs")
      showTab("tabsetSupport", "Obs2")
    }else{
      hideTab("tabset", "Obs")
      hideTab("tabsetSupport", "Obs2")}

    })
  #Mini plotting popup for user specified location
  modal_controls <- glideControls(
    list(
      prevButton(),
      firstButton(
        class = "btn btn-danger",
        `data-dismiss`="modal",
        "Close!"
      )
    ),
    list(
      nextButton(),
      lastButton(
        class = "btn btn-success",
        `data-dismiss`="modal",
        "Done"
      )
    )
  )
  
  #----BEGIN PLOTTING ASSISTANT
  timeRangeUser <- reactive({
    x <- input$userDateRange
  })
  
  #Give users a choice of observations
  dfPresent <- dfWrangled %>% ungroup() %>%
    select(Species, BDT.C, EADDC, Location, Author, Year, Journal) 

  output$speciesSearch = DT::renderDataTable(dfPresent, selection = 'multiple', options = list(pageLength = 5), server = TRUE)

  # print the selected indices
  output$speciesSearchResults = renderPrint({
    s = input$speciesSearch_rows_selected
    if (length(s)) {
      cat('Using the following values for selected observations:\n\n')
      cat(' BDT: ')
      cat(mean(dfPresent$BDT.C[s]))
      cat('\n\n EADDC: ')
      cat(mean(dfPresent$EADDC[s]))
    }
  })
  output$speciesResults <- renderPrint(input$searchSpecies_search)
  
  #Watch for go and then make plot
  observeEvent({
    input$goButton
    }, {
      #print("attempting a run")
      toastr_info("Looking for weather data", progressBar = TRUE, timeOut = 5000)
      
      zipcode <- input$zipcode
      zipcodes <- read_delim("./dat/us-zip-code-latitude-and-longitude.csv", delim = ";")
      
      
        zipLocation <- zipcodes %>% 
          filter(Zip == zipcode) %>% 
          select(City, State, geopoint)
        
        print(str_c("Zip location: ", zipLocation$geopoint))
        if(!identical(zipLocation$geopoint, character(0))){
          
        latLon <- list("id" = zipcode,
                       "latitude" = unlist(strsplit(zipLocation$geopoint, ","))[1],
                       "longitude" = unlist(strsplit(zipLocation$geopoint, ","))[2])
        
        zipStations <- nearestStat(latLon)[[1]]
        
        zipPresent <-   zipStations %>% select(id, name, distance)
        
        output$zipResults <- renderPrint(dplyr::select(zipLocation, City, State)[1]) 
        
        output$zipWeather <- renderPrint(zipPresent)
        #output$zipAdvice <- renderPrint("Go to the next page to see the plot... ")
        
        print(input$zipcode)
        uid <- input$speciesSearch_rows_selected
        
        time <- timeRangeUser()
        
        weather <- gatherWeatherData(timeRange = time, station1 = zipStations$id[1], station2 = zipStations$id[2])
        print(str_c("UID: ", uid))
        if((!is.null(uid)) && ((!is.null(weather$tMax1) && !is.null(weather$tMin1)) || (!is.null(weather$tMax2) && !is.null(weather$tMin2)))){
          print("plotting")
          output$userPredPlot <- renderPlotly({
            dd_plot(tMax1 = weather$tMax1, 
                    tMax2 = weather$tMax2, 
                    tMin1 = weather$tMin1, 
                    tMin2 = weather$tMin2, 
                    mean(dfPresent$BDT.C[uid]),
                    mean(dfPresent$EADDC[uid]),
                    time[1],
                    time[2],
                    species = unique(dfPresent$Species[uid]),
                    breaks="1 month",
                    dateformat="%m/%y",
                    locality = str_c(zipLocation$City, ", ", zipLocation$State))})
          print("Plot rendered")
          toastr_success("Zip code found", timeOut = 10000)
          toastr_success("Plot rendered successfully", timeOut = 10000)
          toastr_info("Click 'View plot!' to view/save rendered plot", timeOut = 20000)
          }
        else {
          toastr_error("Insufficient inputs for plotting")
          output$zipAdvice <- renderPrint("Either there is no current RNOAA (weather) data available at this zip code, or you have not selected a species from the last page.")}
      }else{
        output$zipAdvice <- renderPrint("Zip code not found. Please try a different zip code.")
        toastr_error("Zip code not found")}})
  
  #output$zipResults <- renderPrint(input$zipcode)
  
  glide_modal <- modalDialog(
    title = "Phenology plotting assistant",
    easyClose = FALSE,
    footer = NULL,
    size = 'l',
    glide(
      custom_controls = modal_controls,
      screen(
        next_label = 'Continue! <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
        #next_condition = "!(length(input.speciesSearch_rows_selected) == 0)",
        p("First, we will need to get some values. Specifically, we will need to know the insect's baseline developmental threshold \u00B0C (BDT.C) to calculate degree days \u00B0C. 
          Also, we will need to know the egg to adult degree days \u00B0C (EADDC) value, which determines how many degree days an insect egg must accumulate before reaching 'adulthood'."),
        p("You may search for your favorite insect species in our database: "),
        DT::dataTableOutput('speciesSearch'),
        helpText("To select: Click on one or multiple observations in the table for use in plotting (if multiple are selected BDT.C and EADDC values are averaged)"),
        verbatimTextOutput('speciesSearchResults'),
        p("Now, click continue to pick a location...")
        #next_condition = "!is.empty(input.speciesSearch_rows_selected)"
      ),
      screen(
        #next_condition = "as.numeric(input.zipCode) > 0",
        next_label = 'View plot! <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
        p("Ok, now that we have BDT.C and EADDC values, to calculate degree days we simply need daily temperature data."),
        p("Enter a zip code in the US to gather weather data: "),
        zipInput("zipcode", "Search for a zip code: "),
        #numericInput("mean_modal", "Mean", value = 0)
        verbatimTextOutput('zipResults'),
        p("To view phenology for a different year, you can change the date range."),
        dateRangeInput(inputId = "userDateRange", 
                       label = "Change date range: ",
                       start  = floor_date(Sys.Date(), "year"),
                       min    = "1970-01-01",
                       format = "mm/dd/yy",
                       separator = " - "),
        actionButton("goButton", "Find weather in date range for zip code and update plot!"),
        verbatimTextOutput('zipWeather'),
        verbatimTextOutput("zipAdvice")
        ),
      screen(
        #p("Thanks, we're all set !"),
        plotlyOutput("userPredPlot") %>% withSpinner(color = "#228B22"),
        helpText("\n If the plot is not loading, please ensure at least one species is selected, 
                 you have entered a US zip code, 
                 and clicked the search button to find weather data."))))

  
  observeEvent(input$miniPlotter, {
  showModal(glide_modal)})
}

#--------- Here, the shiny app is being executed--------
shinyApp(ui = ui, server = server)
