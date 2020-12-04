#Sys.setenv(BASH_ENV="~/.bashrc")
#Sys.getenv()
setwd("/home/rstudio/Insect-Phenology-Forecaster")
library(tidyverse)
library(mosaic)
library(leaflet)
library(lubridate)
library(sp)
library(raster)
library(AOI)
library(climateR)

print(str_c("Starting update at ", Sys.time()))
print(loadedNamespaces())
#library(aws.s3)
#print(loadedNamespaces())

##How many species heatmaps can be updated when a user begins a session
max_heatmap_updates_per_session = 15
#print(rstudioapi::isAvailable())
#s3_data <- "shiny-insect-forecaster"

#DEGREE DAYS CALCULATION 
#Single sine wave approximation from Baskerville & Emin 1969
#(see http://www.ipm.ucdavis.edu/WEATHER/ddss_tbl.html)
#Input:
#Tdat: 2 column matrix with Tmin followed by Tmax
#LDT:lower developmental threshold
#------Adapted from Lauren Buckley (no longer allows for negative DDs and accepts NA values)
degree.days.mat <- function(Tmin, Tmax, LDT){
  
  # entirely above LDT
  #|| is.null(Tmin) || is.null(Tmax)
  if(is.na(Tmin) || is.na(Tmax)){dd = NA}
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
accumulateDD <- function(start_date = as.Date(str_c(year(Sys.Date()), '-01-01')),
                         end_date = Sys.Date() -2, 
                         BDT = NULL, 
                         EADDC = NULL, 
                         cum_DD = NULL, 
                         species = NULL){
  #Define area of interest 
  print(cum_DD)
  if(!is.null(cum_DD)){
    print("Matching start date with layer")
    start_date <- str_replace_all(sub('.', '', last(names(cum_DD))), "[/.]", "-")
  }
  if(!is.Date(start_date)){start_date <- as.Date(start_date)}
  if(!is.Date(end_date)){end_date <- as.Date(end_date)}
  print(str_c("Start date: ",start_date))
  
  if((is.null(BDT) || is.null(EADDC)) && is.null(species)){return("Please provide T0 and G arguments, or a species to query")}
  if(!is.null(species)){
    toAccumulate <- get_dfWrangled() %>% filter(Species == species)
    print(str_c("Species selected: ", species))
    print("T0 Values Found: ")
    print(toAccumulate$BDT.C)
    BDT <- mean(toAccumulate$BDT.C)
    print(str_c("Average T0: ", BDT))
    print("G Values Found: ")
    print(toAccumulate$EADDC)
    EADDC <- mean(toAccumulate$EADDC)
    print(str_c("Average G: ", EADDC))}
  #Find means of BDT and EADDC if vector of either is passed in
  if(length(BDT) > 1){
    print(str_c("Averaging T0s: ", BDT))
    BDT <- mean(BDT)
    print(str_c("Average T0: ", BDT))}
  if(length(EADDC) > 1){
    print(str_c("Averaging G: ", EADDC))
    EADDC <- mean(EADDC)
    print(str_c("Average G: ", EADDC))}
  print(str_c("T0: ", BDT, ", G: ", EADDC))
  AOI = aoi_get(state = "conus")
  #Get temp raster stack for start_date
  #raster::plot(AOI)
  #print("not above here")
  #Initialize cum_DD to DD values for start_date
  if(is.null(cum_DD)){
    #print("initializing")
    p = getGridMET(AOI, param = c('tmax','tmin'), startDate = start_date)
    r = raster::brick(p)
    #r = raster::brick(p$tmax[[1]], p$tmin[[1]])
    names(r) = c('tmin', 'tmax')
    
    pastStack <- NULL
    cum_DD <- overlay(r, fun = function(x){degree.days.mat(mosaic::value(x[1]) -273.15, mosaic::value(x[2]) -273.15, BDT)})
    p <- NULL
    r <- NULL
  }else{
    pastStack <- cum_DD
    # hold_temps <- tempfile(fileext = ".grd")
    # writeRaster(tstack, filename = hold_temps)
    cum_DD <- raster(pastStack, layer = nlayers(pastStack))}
  print(cum_DD)
  # cum_DD <- calc(r, fun = function(x){
  #   degree.days.mat(x[2] / 10, x[1] / 10, BDT)})
  #Set current day to the next start day
  
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
    names(availablePhenoSpecies)[length(availablePhenoSpecies)] <- str_c(species)
    write_rds(availablePhenoSpecies, "./dat/availablePhenoSpecies.csv")}
  else{filePath <- str_c("./dat/", make.names(current_date), ".grd")}
  
  print(str_c("Filepath: ", filePath))
  
  week <- 1
  current_date = start_date + 1
  the_stack <- NULL
  #Accumulate Degree Days from current_date to end_date, inclusive.
  while(current_date <= end_date){
    #Get raster stack of tmin and tmax for current day
    temps = getGridMET(AOI, param = c('tmax','tmin'), startDate = current_date)
    tstack = raster::brick(temps)
    names(tstack) = c('tmin', 'tmax')
    current_DD <- overlay(tstack, fun = function(x){degree.days.mat(mosaic::value(x[1]) -273.15, mosaic::value(x[2]) -273.15, BDT)})
    
    #print(current_date)
    # hold_temps <- tempfile(fileext = ".grd")
    # writeRaster(tstack, filename = hold_temps)
    #Calculate todays DD values
    #current_DD <- calc(tstack, fun = function(x){degree.days.mat(value(x[2]) -273.15, value(x[1]) -273.15, BDT)})
    
    temps <- NULL
    tstack <- NULL
    
    
    print(str_c("Calculated DDs for ", current_date))
    #print(identical(current_DD, cum_DD))
    #names(current_DD) = c(current_date)
    #Add cumulative DD values to current_date DD values (current_DD)
    cum_DD <- cum_DD + current_DD
    #Reset cum_DD values greater than EADDC to 0
    cum_DD <- overlay(cum_DD, fun = function(cumul){
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
        #hold_brick <- tempfile(fileext = ".grd")
        #writeRaster(the_stack, filename = hold_brick)
        #writeRaster(brick(hold_brick), filePath, overwrite=TRUE)
        writeRaster(the_stack, filePath, overwrite=TRUE)
        
        pastStack <- NULL
      }else{
        the_stack <- raster::stack(cum_DD)}
      print(str_c("the_stack: ",the_stack))
      #hold_brick <- tempfile(fileext = ".grd")
      #writeRaster(the_stack, filename = hold_brick)
      #writeRaster(brick(hold_brick), filePath, overwrite=TRUE)
      writeRaster(the_stack, filePath, overwrite=TRUE)
      print(names(the_stack))
    }
    if(week == 14){
      #if(is.null(the_stack)){the_stack = raster::stack("holdraster.grd")}
      the_stack <- raster::stack(the_stack, cum_DD)
      #hold_brick <- tempfile(fileext = ".grd")
      #writeRaster(the_stack, filename = hold_brick)
      #writeRaster(brick(hold_brick), filePath, overwrite=TRUE)
      writeRaster(the_stack, filePath, overwrite=TRUE)
      print(the_stack)
      print(names(the_stack))
      #the_stack <- NULL
      week <- 7
    }
  }
  
  
  #Save the raster to the dat folder
  print(str_c("Writing raster: ", the_stack))
  #saveRDS(the_stack, filePath)
  
  #hold_brick <- tempfile(fileext = ".grd")
  #writeRaster(the_stack, filename = hold_brick)
  #writeRaster(brick(hold_brick), filePath, overwrite=TRUE)
  
  return(the_stack)#brick(hold_brick))
  #raster::plot(newR)
}





#--This function checks the age of the phenology maps for species we have and updates them if they are over a week old.
#----availableSpecies: an optional list of species names and locations of phenology grids for corresponding species, it defaults to reading the current availablePhenoSpecies list.
updatePhenology <- function(availableSpecies = readr::read_rds("./dat/availablePhenoSpecies.csv"), maxUpdates = 1){
  
  if(!is.null(max_heatmap_updates_per_session)) {maxUpdates = max_heatmap_updates_per_session}
  
  updates_remaining <- maxUpdates
  #lapply(seq_along(availableSpecies), function(i){
  i <- 1
  
  while(updates_remaining >= 1 && i <= length(availableSpecies)){
    #Get name and filePath values from availableSpecies list at index i
    #print("triggered")
    # print(max_updates)
    # max_updates = max_updates -1
    # print(max_updates)
    name <- names(availableSpecies)[[i]]
    filePath <- availableSpecies[[i]]
    toUpdate <- raster::brick(availableSpecies[[i]])
    last_update <- as.Date(str_replace_all(sub('.', '', last(names(toUpdate))), "[/.]", "-"))
    currentDay <- Sys.Date() -2
    #Calculate file age (in days) after fetching the last date it was modified
    #last_update <- as.Date(file.info(availableSpecies[[i]])$mtime)
    print(str_c("Layer last updated on ", last_update))
    file_age <- as.double.difftime(currentDay - last_update)
    print(str_c("Data currently available for ", currentDay))
    
    thisYear <- format(Sys.Date(), '%Y')
    #If the file is from the previous year and we have gridMET data from this year, delete the file and start a new one.
    if(!(format(last_update, '%Y') == thisYear) && (Sys.Date() -2 >= as.Date(str_c(thisYear, "-01-01")))){
      print(str_c("Creating a new phenology record for ", name, " in ", thisYear))
      #toastr_info("Please be patient, a new degree day map is being created for this year.")
      dir.create('./dat/tmp', showWarnings = FALSE)
      accumulateDD(as.Date(str_c(thisYear, "-01-01")), 
                   Sys.Date() - 2, 
                   species = name)
      unlink("./dat/tmp", recursive=TRUE)
      #max_updates <- max_updates -1
      updates_remaining <- updates_remaining -1
      
    }else{
      #If the file is at least a week old and we have gridMET data for this year, update it
      if((file_age >= 7) && (Sys.Date() -2 >= as.Date(str_c(thisYear, "-01-01")))){
        #toastr_info("A degree day map is being updated.")
        weeks <- as.integer(file_age / 7)
        stop_update <- last_update + (7 * weeks)
        dir.create('./dat/tmp', showWarnings = FALSE)
        rasterOptions(tmpdir='./dat/tmp')
        hold_rast <- rasterTmpFile(prefix = make.names(name))
        writeRaster(toUpdate, filename = hold_rast)
        print(str_c("Updating ", name, ". This file was ", file_age ," days old."))
        accumulateDD(end_date = stop_update, 
                     species = name,
                     cum_DD = brick(hold_rast))
        #Just added to cleanup temp files
        #removeTmpFiles()
        unlink("./dat/tmp", recursive=TRUE)
        updates_remaining <- updates_remaining -1
        print(str_c(name, " was ", file_age, " days old. It is now 2 days old, due to gridMET restrictions."))
      } else print(str_c(name, " was modified less than a week ago (", file_age, " days). It will be updated in ", (7 - file_age), " days."))
    }
    i <- i + 1}}

updatePhenology()

