#SEASONALITY ANALYSIS

fdir= "C:\\Users\\Buckley\\Google Drive\\Seasonality\\"

#LOAD LIBRARIES
library(stringr)
library(dismo)
library(rnoaa) #http://recology.info/2015/07/weather-data-with-rnoaa/
library(zoo)
library(tidyr)
library(ggplot2)
library(tidyr)
library(maptools)
#for mapping
library(ggmap)
library(maps)
library(mapdata)
library(colorRamps)     # for matlab.like(...)
library(grid)
library(plyr); library(dplyr)
#devtools::install_github("hrbrmstr/nominatim")
library(nominatim) #for osm geocode
library(gridExtra)
library(akima) #for interpolations
library(nlme) #for lme
library(psych)  #for geometric mean

#--------------------------------------------------------------
#LOAD FUNCTIONS
#Make wrapper for Julian function
julian.wrap=function(date1){ 
  #date1= as.Date(date1, "%Y-%m-$d")
  origin1 = as.Date(paste("1-1-",as.numeric(format(date1, "%Y")), sep=""), format="%m-%d-%Y")
  julian(date1, origin=origin1)
}

#DEGREE DAYS CALCULATION
#Single sine wave approximation from Baskerville & Emin 1969
#(see http://www.ipm.ucdavis.edu/WEATHER/ddss_tbl.html)
#Input:
#Tdat: 2 column matrix with Tmin followed by Tmax
#LDT:lower developmental threshold
degree.days.mat=function(Tdat,LDT){
  Tmin=Tdat[1]
  Tmax=Tdat[2]
  
  # entirely above LDT
  if(Tmin>=LDT) {dd=(Tmax+Tmin)/2-LDT}
  
  # intercepted by LDT
  ## for single sine wave approximation
  if(Tmin<LDT && Tmax>LDT){
    alpha=(Tmax-Tmin)/2
    theta1=asin(((LDT-(Tmax+Tmin))/alpha)*pi/180)
    dd=1/pi*(((Tmax+Tmin)/2-LDT)*(pi/2-theta1)+alpha*cos(theta1))
    if(!is.na(dd))if(dd<0){dd=0}
  } #matches online calculation
  
  # entirely below LDT
  if(Tmax<=LDT){ dd=0}
  
  return(dd)
}

#COUNT
count= function(x) length(na.omit(x))

#CALCULATE SLOPES
calcm= function(x, ys=ys){
  yd= which( !is.na(x) )
  if(length(yd)>0) mod1= lm( as.numeric(x[yd])~ys[yd] )   
  return(tryCatch(coef(summary(mod1))[2, ], error=function(e) c(NA,NA,NA,NA)))
}

calcm.poly= function(x, ys=ys){
  yd= which( !is.na(x) )
  if(length(yd)>4) mod1= lm( as.numeric(x[yd])~ poly(ys[yd],2) )   
  return(tryCatch( c(coef(summary(mod1))[2:3, ]), error=function(e) c(NA,NA,NA,NA,NA,NA,NA,NA)))
}

#plot with shared legend
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

#--------------------------------
#LOAD DATA

setwd(paste(fdir,"out\\",sep="") )

##READ BACK IN
dddat= read.csv("AppendixS3_SeasonalityDatabase.csv")

##Restrict to dat with lat / lon
dddat= dddat[which(!is.na(dddat$lon) & !is.na(dddat$lat) ),]

#remove phys outliers
dddat$omit=NA
dddat[which(dddat$BDT.C< (-7)),"omit"]="y" #drops 3
dddat[which(dddat$EADDC>2000),"omit"]="y"  #drops 9

#group by index
dddat$index= as.factor(dddat$index)

dddat= dddat %>% group_by(index) %>% summarise(Species=head(Species)[1],Order=head(Order)[1],Family=head(Family)[1],Genus=head(Genus)[1],Species.1=head(Species.1)[1],BDT.C=mean(BDT.C), UDT.C=mean(UDT.C),EADDC=mean(EADDC),EEDDC=mean(EEDDC), pest=mean(pest), aquatic=mean(aquatic),pupal=mean(pupal),Location=head(Location)[1],lon=mean(lon),lat=mean(lat), quality=head(quality)[1], parasitoid=head(parasitoid)[1])

dddat= as.data.frame(dddat)

#--------------------------------
#FIND CLOSEST GHCN STATIONS

#Read GGCN database inventory http://www.ncdc.noaa.gov/oa/climate/ghcn-daily/
setwd(paste(fdir,"data\\",sep=""))
stations=read.table("ghcnd-inventory.txt")
names(stations)=c("Id","Lat","Lon","Var","FirstYear","LastYear")
stations.un= unique(stations$Id)
stats.elev= read.csv("ghcnd-stations.csv")

#Restrict stations to those with recent data and at least 20 years of data
stations= stations[which(stations$LastYear>2010 & (stations$LastYear-stations$FirstYear)>20 ),] 

#Check have TMIN and TMAX dat
stations= stations[which(stations$Var=="TMAX" | stations$Var=="TMIN"),]
stations= spread(stations,Var, Var)
stations= stations[which(stations$TMAX=="TMAX" & stations$TMIN=="TMIN"),]

stat.coords= cbind(stations$Lon, stations$Lat)

#--------------------------------
#SET UP DATA STORAGE
phen.dat= array(NA, dim=c(nrow(dddat), length(1970:2015), 20, 10), dimnames=list(NULL,as.character(1970:2015),as.character(1:20),c("phen","Tmean.e","Tsd.e","T10q.e","Tmean","Tsd","T10q","DaysGen", "Tmean.e.fixed", "Tmean.fixed")) )
ngens= array(NA, dim=c(nrow(dddat), length(1970:2015)), dimnames=list(NULL,as.character(1970:2015)))
phen.fixed= array(NA, dim=c(nrow(dddat), 20, 8), dimnames=list(NULL,as.character(1:20),c("phen","Tmean.e","Tsd.e","T10q.e","Tmean","Tsd","T10q","DaysGen")) )

#----------------------------------
#ANALYSIS

for(stat.k in 1:nrow(dddat) ){  
  if( all(!is.na(dddat[stat.k,c("lon","lat")] )) ){
    min.dist<- order(spDistsN1(stat.coords, as.numeric(dddat[stat.k,c("lon","lat")]), longlat = TRUE))[1:100]
    min.site= stations$Id[min.dist]
    
    ind=0
    years=NA
    
    while(length(years)<10 & ind<100){
      ind= ind + 1
      tmax=try( ghcnd_search(min.site[ind], var = "TMAX") )
      tmin=try( ghcnd_search(min.site[ind], var = "TMIN") )
      while( (is.null(nrow(tmax$tmax)) | is.null(nrow(tmin$tmin))) & ind<100){ ind=ind+1
      tmax=try( ghcnd_search(min.site[ind], var = "TMAX") )
      tmin=try( ghcnd_search(min.site[ind], var = "TMIN") )
      }
      
      if(!is.null(nrow(tmax$tmax)>0) ){ #CHECK DATA
        #combine tmin and tmax
        match1= match(tmax$tmax$date, tmin$tmin$date)
        is.matched= !is.na(match1)
        
        dat= tmax$tmax
        dat$tmin= NA
        dat$tmin[is.matched]= tmin$tmin$tmin[match1[is.matched]]
        
        #split date 
        date= as.Date(dat$date, "%Y-%m-$d")
        dat$year=as.numeric(format(date, "%Y"))
        dat$month=as.numeric(format(date, "%m"))
        dat$day=as.numeric(format(date, "%d"))
        dat$j= unlist(lapply(date, FUN="julian.wrap"))+1
        
        #Format data
        dat$tmax= as.numeric(dat$tmax)
        dat$tmin= as.numeric(dat$tmin)
        
        dat$tmax[which(dat$tmax==-9999)]= NA
        dat$tmax= dat$tmax/10 #correct for tenths of degrees or mm
        dat$tmin[which(dat$tmin==-9999)]= NA
        dat$tmin= dat$tmin/10 #correct for tenths of degrees or mm
        
        #Catch other NA values
        dat$tmax[which(dat$tmax>200)]= NA
        dat$tmin[which(dat$tmin>200)]= NA
        dat$tmax[which(dat$tmax< -200)]= NA
        dat$tmin[which(dat$tmin< -200)]= NA
        
        ## FIND YEARS WITH NEARLY COMPLETE DATA
        dat.agg= aggregate(dat, list(dat$year),FUN=count)  ### PROBLEM IF RASTER LOADED
        years= dat.agg$Group.1[which(dat.agg$tmax>300)]
        dat= dat[which(dat$year %in% years),]
        
      } #END CHECK NULL
    } #END WHILE YEARS
    
    #RECORD SITE DATA
    dddat$Id[stat.k]= as.character(min.site[ind])
    dddat$st.lat[stat.k]= stations$Lat[min.dist[ind]]
    dddat$st.lon[stat.k]= stations$Lon[min.dist[ind]]
    dddat$st.elev[stat.k]= stations$elev[min.dist[ind]]
    
    #--------------------------------------
    #INTERPOLATE MISSING DATA
    dat$tmin= na.approx(dat$tmin, maxgap=7, na.rm = FALSE)
    dat$tmax= na.approx(dat$tmax, maxgap=7, na.rm = FALSE)
    #Tmean
    dat$tmean= (dat$tmin+dat$tmax)/2
    
    #cut missing data
    dat= dat[which(!is.na(dat$tmin) & !is.na(dat$tmax)),]
    
    #CALCULATE DEGREE DAYS
    dat$dd= apply( dat[,c("tmin","tmax")], MARGIN=1, FUN=degree.days.mat, LDT=dddat$BDT.C[stat.k] )
    
    #normalize S hemisphere to N hemisphere 
    if(dddat[stat.k,"lat"]<0) 
      {inds.jd= which(dat$j>181)
        inds.jj= which(dat$j<182)
      
      dat[inds.jd, "j"] = dat$j[inds.jd] -181
      dat[inds.jj, "j"] = dat$j[inds.jj] +184
      } #end fix s hemi
      
    dat = dat %>% group_by(year) %>% arrange(j) %>% mutate(cs = cumsum(dd))
    
    #Estimate phenology across years based on DD accumulations
    #cumsum within groups
    #Egg to adult DD, First date beyond threshold
    for(genk in 1:20){
      
      phen= dat %>%  group_by(year) %>% slice(which.max(cs> (genk* dddat$EADDC[stat.k]) ))
      #replace eroneous values
      phen[which(phen$j==1),"j"]=NA #& phen$cs==0
      #drop years without generation
      phen= phen[!is.na(phen$j),]
      
      #use only years starting 1970
      years.ind=1970:2015
      year.loop= unique(phen$year)
      year.loop= year.loop[which(year.loop>1969 & year.loop<2016) ]
      
      for(yeark in year.loop){
        
        j= as.numeric(phen[phen$year==yeark,"j"])
        
        dat.yr= dat[dat$year==yeark ,]
        #TEMPS: 2 WEEKS AT EMERGENCE
        temps= as.numeric(unlist(dat.yr[dat.yr$j %in% (j-7):(j+7),"tmean"]))
        phen.dat[stat.k,which(years.ind==yeark),genk,"phen"]= j
        #MEAN
        phen.dat[stat.k,which(years.ind==yeark),genk,"Tmean.e"]= mean(temps)
        #STDEV
        phen.dat[stat.k,which(years.ind==yeark),genk,"Tsd.e"]= sd(temps)
        #10% QUANTILE
        phen.dat[stat.k,which(years.ind==yeark),genk,"T10q.e"]= quantile(temps, 0.1)
        
        #TEMPS: ACROSS GENERATIONS
        #FIND GEN TIME
        j.gs= as.numeric(ifelse(genk==1, dat.yr[which.min(dat.yr$dd>0),"j"], phen.dat[stat.k,which(years.ind==yeark),(genk-1),"phen"] ))
        
        temps= as.numeric(unlist(dat.yr[dat.yr$j %in% j.gs:j,"tmean"]))
        #MEAN
        phen.dat[stat.k,which(years.ind==yeark),genk,"Tmean"]= mean(temps)
        #STDEV
        phen.dat[stat.k,which(years.ind==yeark),genk,"Tsd"]= sd(temps)
        #10% QUANTILE
        phen.dat[stat.k,which(years.ind==yeark),genk,"T10q"]= quantile(temps, 0.1)
        
        #duration of generation
        phen.dat[stat.k,which(years.ind==yeark),genk,"DaysGen"]= j-j.gs
        
      } #end year loop
    } #end GEN LOOP
    
    #Number generations by year
    phen.dat2= as.data.frame(phen.dat[stat.k,,,"phen"])
    
    all.na= apply(phen.dat2, MARGIN=1, function(x)all(is.na(x)) )
    
    ngens[stat.k,]= apply(phen.dat2, MARGIN=1, FUN=function(x)length(which(x>1)) )
    #correct for years without data
    ngens[stat.k,which(all.na==TRUE) ]=NA
    
    #Generation j and temp average across years
    for(genk in 1:20){
      j= round( mean(phen.dat[stat.k,,genk,"phen"], na.rm=TRUE) )
      
      if(!is.nan(j)){
        phen.fixed[stat.k,genk,"phen"]= j
        dat.gen= dat[dat$year==yeark ,]
        
        temps= as.numeric(unlist(dat[dat$j %in% (j-7):(j+7),"tmean"]))
        
        #MEAN
        phen.fixed[stat.k,genk,"Tmean.e"]= mean(temps)
        #STDEV
        phen.fixed[stat.k,genk,"Tsd.e"]= sd(temps)
        #10% QUANTILE
        phen.fixed[stat.k,genk,"T10q.e"]= quantile(temps, 0.1)
        
        #---
        #Yearly temp at fixed date
        temps= dat[dat$j %in% (j-7):(j+7),]
        temps1= temps %>% group_by(year) %>% summarise(mean(tmean, na.rm=TRUE))
        temps1= temps1[which(temps1$year %in% 1970:2015),]
        
        match1= match(as.character(unlist(temps1[,1])), names(phen.dat[stat.k,,genk,"Tmean.e.fixed"]))
        phen.dat[stat.k,match1,genk,"Tmean.e.fixed"]= unlist(temps1[,2])
        
        #---
        if(genk>1){
          j_1= round( mean(phen.dat[stat.k,,genk-1,"phen"], na.rm=TRUE) )
          
          temps= as.numeric(unlist(dat[dat$j %in% j_1:j,"tmean"]))
          
          #MEAN
          phen.fixed[stat.k,genk-1,"Tmean"]= mean(temps)
          #STDEV
          phen.fixed[stat.k,genk-1,"Tsd"]= sd(temps)
          #10% QUANTILE
          phen.fixed[stat.k,genk-1,"T10q"]= quantile(temps, 0.1)
          
          #gen length
          phen.fixed[stat.k,genk-1,"DaysGen"]= j-j_1+1
          
          #---
          #Yearly temp at fixed date
          temps= dat[dat$j %in% j_1:j,]
          temps1= temps %>% group_by(year) %>% summarise(mean(tmean, na.rm=TRUE))
          temps1= temps1[which(temps1$year %in% 1970:2015),]
          
          match1= match(as.character(unlist(temps1[,1])), names(phen.dat[stat.k,,genk,"Tmean.fixed"]))
          phen.dat[stat.k,match1,genk-1,"Tmean.fixed"]= unlist(temps1[,2])
          
        } #end gen loop
        #---
        
      } #end check gen exists
    } #end GEN LOOP
    
    print(stat.k)
    
  } #check coordinates
} #end site (stat.k) loop

##SAVE OUTPUT
#setwd(paste(fdir,"out\\",sep="") )
#saveRDS(phen.dat, "phendat.rds")
#saveRDS(phen.fixed, "phenfix.rds")
#saveRDS(ngens, "ngens.rds")
#saveRDS(dddat, "dddat_media.rds")

##READ BACK IN
setwd(paste(fdir,"out\\",sep="") )
phen.dat= readRDS("phendat.rds")
phen.fixed= readRDS("phenfix.rds")
ngens= readRDS("ngens.rds")
dddat= readRDS("dddat_media.rds")

#drop omit data
dropi= which( dddat$omit=="y" )
phen.dat= phen.dat[-dropi,,,]
phen.fixed= phen.fixed[-dropi,,]
ngens= ngens[-dropi,]
dddat= dddat[-dropi,]

#============================================================
#STATISTICS 
#Duration of generations
#Developmental temperatures across generations
#Adult temperatures across generations

#Correct generation data for missing data
ngens[is.na(phen.dat[,,1,1])]=NA

#Account for negative generation durations
days.gen= phen.dat[,,,"DaysGen"]
days.gen2= days.gen
days.gen[days.gen<0] <- NA
phen.dat[,,,"DaysGen"]= days.gen

days.gen= phen.fixed[,,"DaysGen"]
days.gen3= days.gen
days.gen[days.gen<0] <- NA
phen.fixed[,,"DaysGen"]= days.gen

#======================================
#PLOTS

#Fig 2. MAPS SITES
#Plot locations
world <- map_data("world")

#change trait names
names(dddat)[which(names(dddat)=="BDT.C")]<-"T0"
names(dddat)[which(names(dddat)=="EADDC")]<-"G"

w1= ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group), fill=NA, color="black")+ 
  coord_fixed(1.3) +ylim(c(-55,80))+xlim(c(-170,195))+theme_bw()
w2= w1 +xlab("Longitude (°)")+ylab("Latitude (°)")
w3= w2+ geom_point(data = dddat, aes(x = lon, y = lat, color= T0, size=log(G) )) + scale_color_gradientn(colours=rev(matlab.like(10)))+ scale_size_continuous(range = c(1,4)) #+ theme(legend.position="bottom") +, alpha = 1/10

setwd(paste(fdir,"figures\\",sep="") )
pdf("DevMap.pdf", height = 6, width =10)
par(mfrow=c(1,1), cex=1.2, mar=c(3, 3, 0.5, 0.5), oma=c(0,0,0,0), lwd=1, bty="o", tck=0.02, mgp=c(1, 0, 0))

plot(w3)

dev.off()
#============================================
#Fig. 3 LATITUDINAL GRADIENTS IN DATA

#plots by order
dat=dddat

#restrict to orders with data
dat2=dat[dat$Order %in% c("Coleoptera","Diptera","Hemiptera","Homoptera","Hymenoptera","Lepidoptera") ,]
#Neuroptera   Thysanoptera
dat2$pupal= as.factor(dat2$pupal)

p<- ggplot(data=dat2, aes(x=T0, y = G, color=abs(lat) ))+facet_grid(.~Order) +theme_bw()+ scale_color_gradientn(colours=rev(matlab.like(20)))
p1= p + geom_point()  +geom_smooth(method=lm, se=FALSE, colour="grey")
#+shape=pupal #+scale_colour_discrete(guide = FALSE) + scale_shape_manual(values = c(1,19))

#by latitude
p<- ggplot(data=dat2, aes(x=abs(lat), y = T0, color=log(G) ))+facet_grid(.~Order) + scale_shape_manual(values = c(1,19)) +xlab("Absolute latitude (°)")+theme_bw() + scale_color_gradientn(colours=matlab.like(20)) #+scale_shape_discrete(guide = FALSE)
p2= p + geom_point() +geom_smooth(method=lm, se=FALSE, colour="grey")

p<- ggplot(data=dat2, aes(x=abs(lat), y = G, color=T0 ))+facet_grid(.~Order) + scale_shape_manual(values = c(1,19)) +xlab("Absolute latitude (°)") +theme_bw() + scale_color_gradientn(colours=matlab.like(20)) #+scale_shape_discrete(guide = FALSE)
p3= p + geom_point() +geom_smooth(method=lm, se=FALSE, colour="grey")

#--------------------
setwd(paste(fdir,"figures\\",sep="") )
pdf("T0_DDD.pdf", height = 8, width = 8)

grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size="last"))

dev.off()

#============================================
#Fig 4. PLOT ACCROSS GENERATIONS

#OMIT DUPLICATED VALUES #Now fixed
phen.fixed1= apply(phen.fixed, MARGIN=c(1,3),function(x){x[duplicated(x)]=NA; return(x)})

ylabs= c("Developmental temperature (°C)","Developmental T sd (°C)", "Adult T mean (°C)", "Adult T sd (°C)","Generation length (days)")

for(i in c(1,5) ){
  
  #Generation temps across years for ? generation
  if(i==1) phen.dat2= as.data.frame( cbind(1:nrow(dddat), dddat[,c("Species","Order","lon","lat")], t(phen.fixed1[,,"Tmean"]) ))
  
  #Generation length
  if(i==5) phen.dat2= as.data.frame( cbind(1:nrow(phen.dat), dddat[,c("Species","Order","lon","lat")], t(phen.fixed1[,,"DaysGen"]) ))
  
  colnames(phen.dat2)[1]="siteID"
  
  #latitude aggregate
  phen.dat2$abslat= abs(phen.dat2$lat)
  phen.dat2$lcut= cut(phen.dat2$abslat, breaks=c(0,10,20,30, 40, 50, 60,90) )
  
  vars1= c("phen","ngens","Tmean","Tsd","Tmean.e","Tsd.e")
  
  phen.dat4= aggregate(phen.dat2, list(phen.dat2$lcut), FUN=mean, na.rm=TRUE)
  names(phen.dat4)[1]= "latgroup"
  
  #convert to long format
  phen.dat4= gather(phen.dat4, "gen", "T",7:26)
  phen.dat4$gen= as.numeric(phen.dat4$gen)  
  
  plot.lat = ggplot(phen.dat4, aes(x=gen, y=T, group= latgroup, color=latgroup)) +geom_line() +theme_bw()+ylab(vars1[i])
  
  #----------------------------------
  
  # CALCULATE SLOPES
  ys= as.numeric( colnames(phen.dat2)[6:ncol(phen.dat2)])
  
  slopes= apply(phen.dat2[6:25], MARGIN=1, FUN=calcm.poly, ys=ys)
  phen.dat3= cbind(phen.dat2[,1:5],t(slopes))
  names(phen.dat3)[6:13]=c("Estimate1","Estimate2","Std.Error1","Std.Error2","t value1","t value2","P1","P2")
  
  #restrict to significant shifts
  phen.sig= phen.dat3[which(phen.dat3$P2<0.05),]
  if(i %in% c(2,4)) phen.sig= phen.sig[which(phen.sig$lat<70),]
  #curvature
  phen.sig= phen.dat3[which(phen.dat3$P1<0.05),]
  
  #------------------------------
  #Store plots over time
  #PLOT TRENDS OVER TIME
  
  #drop non-significant trends
  phen.dat2= phen.dat2[which(phen.dat3$"P1"<0.05),]
  
  #drop rows with all NAs
  drop.row=apply(phen.dat2, MARGIN=1, FUN=function(x)all(is.na(x[6:length(x)])) )
  if(!all(drop.row==FALSE)) phen.dat2= phen.dat2[-which(drop.row==TRUE),]
  
  phen.dat3= gather(phen.dat2, "gen", "phen",6:25)
  phen.dat3$gen= as.numeric(phen.dat3$gen)
  phen.dat4= phen.dat3[which(!is.na(phen.dat3$phen)) ,]
  phen.dat4= phen.dat3[which(phen.dat3$phen>1) ,] #get rid of erroneous phenology values of 1
  
  #remove intermediate latitudes
  phen.dat4= phen.dat4[which(phen.dat4$abslat<35 | phen.dat4$abslat>40) ,]
  
    if(i %in% c(1)) plot1 = ggplot(phen.dat4, aes(x=gen, y=phen, group=siteID, color=abs(lat))) +geom_line() + labs(y = ylabs[i])+ylim(0, 35)+theme_bw()+ scale_color_gradientn(colours=rev(matlab.like(20)),name="Absolute \nlatitude (°)")+ theme(legend.position="none")+xlab("Generation")
  
  if(i %in% c(5)) plot1 = ggplot(phen.dat4, aes(x=gen, y=phen, group=siteID, color=abs(lat))) +geom_line() + labs(y = ylabs[i])+theme_bw()+ scale_color_gradientn(colours=rev(matlab.like(20)),name="Absolute \nlatitude (°)")+xlab("Generation")+ylim(0, 60)
  
  if(i==1) p1= plot1 #for aggregate
  if(i==2) p2=plot1
  if(i==3) p3=plot1
  if(i==4) p4=plot1
  if(i==5) p5=plot1
  
} #end loop metrics

#-----------------------

setwd(paste(fdir,"figures\\",sep="") )
pdf("FixedPhen_genPlots.pdf", height = 7, width = 12)

par(mfrow=c(1,2), cex=1.2, mar=c(3, 3, 0.5, 0.5), oma=c(0,0,0,0), lwd=1, bty="o", tck=0.02, mgp=c(1, 0, 0))
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
vplayout<-function(x,y)
  viewport(layout.pos.row=x,layout.pos.col=y)

print(p1,vp=vplayout(1,1))
print(p5,vp=vplayout(1,2))

dev.off()

#---------------
#PLOT polynomial across latitude

plotp = ggplot(phen.sig, aes(x=abs(lat), y=Estimate2 )) +geom_point() +theme_bw()+xlab("Absolute latitude (°)")+ylab("quadratic coefficient")

setwd(paste(fdir,"figures\\",sep="") )
pdf("FixedPhen_genPlots_quadcoeff.pdf", height = 5, width = 5)

print(plotp)

dev.off()

#==============================================
#Fig 5. PATTERNS ACROSS TIME: adult phenology, number generations; dev temperature

ylabs= c("Mean of adult phenology (J)","Mean of number generations", "Mean of developmental temperature (°C)")
mylabs= c("Slope of adult phenology (J/yr)","Slope of number generations (1/yr)", "Slope of developmental temperature (°C/yr)")

#slore slope data
phen.slopes= dddat[,c("Species","Order","lon","lat", "T0","G")]

for(i in 1:3){
  
  #Calculate phenology shifts across years
  if(i==1) phen.dat2= as.data.frame( cbind(1:nrow(phen.dat), dddat[,c("Species","Order","lon","lat", "T0","G")],phen.dat[,,1,"phen"]))
  
  #Number generations
  if(i==2) phen.dat2= as.data.frame( cbind(1:nrow(phen.dat), dddat[,c("Species","Order","lon","lat", "T0","G")],ngens))
  
  #Generation temps across years for ? generation
  if(i==3) phen.dat2= as.data.frame( cbind(1:nrow(phen.dat), dddat[,c("Species","Order","lon","lat", "T0","G")],phen.dat[,,1,"Tmean"]))
  
  colnames(phen.dat2)[1]="siteID"

  #---------------------------------
  #!  ## CLUST BY LAT
  #elevation aggregate
  phen.dat2$abslat= abs(phen.dat2$lat)
  phen.dat2$lcut= cut(phen.dat2$abslat, breaks=c(0,25,35, 40,50,90) ) #c(0,15,30, 45, 60,90) #c(0,25,35, 40,50,90) #quantile(phen.dat2$abslat, probs = seq(0, 1, 0.2), na.rm = TRUE)
  #aggregate(phen.dat2, list(phen.dat2$lcut),FUN=count)
  
  vars1= c("phen","ngens","Tmean","Tsd","Tmean.e","Tsd.e")
  
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  
  phen.dat2[is.nan.data.frame(phen.dat2)] <- NA 
  
  phen.dat4= aggregate(phen.dat2, list(phen.dat2$lcut), FUN=mean, na.rm=TRUE)
  names(phen.dat4)[1]= "latgroup"
  
  #convert to long format
  phen.dat4= gather(phen.dat4, "year", "T", 9:54 )
  
  #phen.dat4 <- setNames(which(names(phen.dat4)=="T"),vars1[i])
  phen.dat4$year= as.numeric(phen.dat4$year)  
  
  #-------------------
  ylims=quantile(phen.dat4$T, probs= c(0.05,0.95), na.rm=TRUE )
  
  plot.lat = ggplot(phen.dat4, aes(x=year, y=T, group= latgroup, color=latgroup)) +geom_line()  +geom_smooth(method=lm, se=TRUE)+theme_bw(base_size = 18) + theme(legend.position="bottom") + scale_color_manual(labels = c("0-25", "25-35","35-40","40-50","50-61"), values = rainbow(5) ) +labs(color = "Absolute latitude (°)")+ylab(ylabs[i])+xlab("Years") #+ylim(ylims)
 
  phen.dat4[phen.dat4$latgroup=="(0,25]",]
  
  #----------------------------------
  # CALCULATE SLOPES
  
  ys= as.numeric( colnames(phen.dat2)[8:ncol(phen.dat2)])
  
  slopes= apply(phen.dat2[8:(ncol(phen.dat2)-2)], MARGIN=1, FUN=calcm, ys=ys)
  phen.dat3= cbind(phen.dat2[,1:7],t(slopes))
  names(phen.dat3)[8:11]=c("Estimate","Std.Error","t value","P")
  
  #store slopes
  phen.slopes= cbind(phen.slopes, phen.dat3$Estimate, phen.dat3$P)
  
  #code significant shifts
  phen.sig=phen.dat3
  phen.sig$sig=0
  phen.sig$sig[which(phen.dat3$P<0.05)]=1
  
  #remove temperature slope outliers >1
 ylims=quantile(phen.sig$Estimate, probs= c(0.05,0.95), na.rm=TRUE )
  
  plotm=  ggplot(phen.sig, aes(x=abs(lat), y=Estimate)) +geom_point(aes(colour = T0, shape=factor(sig)))  +geom_smooth(method=lm, se=TRUE, color="black")+theme_bw(base_size = 18) + ylab(mylabs[i]) +xlab("Absolute Latitude (°)") +ylim(ylims) +scale_shape_manual(values=c(1, 19)) + guides(shape=FALSE)+ scale_color_gradientn(colours=matlab.like(20))
  
  #---------------
  #Analyze sig nv ns
  aggregate(phen.sig$Estimate, by=list(phen.sig$sig), FUN=median, na.rm=TRUE) 
  
  #------------------------------
  #Store plots over time
  #PLOT TRENDS OVER TIME
  
  #drop non-significant trends
  phen.dat2= phen.dat2[which(phen.dat3$"P"<0.05),]
  
  #drop rows with all NAs
  drop.row=apply(phen.dat2, MARGIN=1, FUN=function(x)all(is.na(x[6:length(x)])) )
  if(!all(drop.row==FALSE)) phen.dat2= phen.dat2[-which(drop.row==TRUE),]
  
  phen.dat3= gather(phen.dat2, "year", "phen",6:(6+46) )
  phen.dat4= phen.dat3[which(!is.na(phen.dat3$phen)) ,]
  
  phen.dat4= subset(phen.dat4, phen.dat4$year>1970)
  phen.dat4$year= as.numeric(phen.dat4$year)
  
  ## smooth
  plot1 = ggplot(phen.dat4, aes(x=year, y=phen, group=siteID, color=abs(lat) )) +geom_smooth(method=lm, se=FALSE, size=0.8)+ labs(y = ylabs[i]) #+ylim(0, 5)
  ## line
  #plot1 = ggplot(phen.dat3, aes(x=year, y=phen, group=siteID, color=abs(lat) )) +geom_line()+ labs(y = ylabs[i]) 
  
  if(i==1) {p1=plot.lat; p1all=plot1; p1m= plotm}
  if(i==2) {p2=plot.lat; p2all=plot1; p2m= plotm}
  if(i==3) {p3=plot.lat; p3all=plot1; p3m= plotm}
  
} #end loop metrics

#-----------------------
#PLOTS ACROSS TIME

setwd(paste(fdir,"figures\\",sep="") )
pdf("TimePlots.pdf", height = 6, width = 12)

layout(matrix(c(1,1), 2, 1, byrow = TRUE))
fig5= grid_arrange_shared_legend(p1, p3, p2, ncol = 3, nrow = 1)
fig6= grid_arrange_shared_legend(p1m, p3m, p2m, ncol = 3, nrow = 1)

dev.off()

#====================================================
#FITNESS PLOTS

#FIG 7. PLOT RELATIONSHIP OF SLOPES

names(phen.slopes)[7:12 ]= c("phen.m","phen.p","ngen.m","ngen.p", "dtemp.m","dtemp.p")
phen.slopes= na.omit(phen.slopes)

#remove outliers
phen.slopes= phen.slopes[which(phen.slopes$phen.m>-1 & phen.slopes$phen.m<1 & phen.slopes$ngen.m>-1 & phen.slopes$dtemp.m>-1),]

#shifts in temp and phenology
fld <- with(phen.slopes, interp(x = phen.m*10, y = dtemp.m*10, z = ngen.m*10, duplicate=TRUE))

gdat <- interp2xyz(fld, data.frame=TRUE)

p3d= ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  coord_equal() +
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller(palette="Spectral", na.value="white", name="number generations\n (1/decade)") + 
  theme_bw(base_size = 18)+xlab("phenology (days/decade)")+ylab("temperature (°C/decade)")+ theme(legend.position="right")+ theme(legend.position="right")+ coord_fixed(ratio = 4)

#--------------
#PLOT

setwd(paste(fdir,"figures\\",sep="") )
pdf("FitnessShifts.pdf", height = 10, width = 10)
p3d
dev.off() 

#==================================================================

#FIG 6. FITNESS SURFACES
#number generations
ngens.ave= rowMeans(ngens, na.rm=TRUE)
#phen.dat2= as.data.frame( cbind(dddat[,c(1:9)],ngens.ave))
phen.dat2= as.data.frame( cbind(dddat[,c(1:6,7,9)],ngens.ave))
phen.dat2= phen.dat2[phen.dat2$Order %in% c("Coleoptera","Diptera","Hemiptera","Homoptera","Hymenoptera","Lepidoptera") ,]

#Plot fitness surface

ngens[is.nan(ngens)] = NA 

phen.dat2= as.data.frame( cbind(1:nrow(dddat), dddat[,c("Species","Order","lon","lat","T0","G")],ngens))
phen.dat3= as.data.frame( cbind(1:nrow(dddat), dddat[,c("Species","Order","lon","lat","T0","G")],rowMeans(ngens, na.rm=T) ))
colnames(phen.dat3)[8]= "Ngen"

#subset to Ngen data
phen.dat3= phen.dat3[which(!is.nan(phen.dat3$Ngen) ),]

#aggregate into latitudinal bins
phen.dat3$latbin= cut(phen.dat3$lat,breaks= c(-45,-20, 0,10, 20,30,40,50,61) ) #c(-45,0,20,40,61)

phen.w= phen.dat3 %>%
  group_by( latbin ) %>% 
  do(mod = lm(Ngen ~ T0,na.action=na.omit,  data = .)) %>%
  mutate(Slope = summary(mod)$coeff[2]) %>%
  select(-mod)

#==========================================================
#FITNESS SURFACE by order

ords= c("Coleoptera","Diptera","Hemiptera","Homoptera","Hymenoptera","Lepidoptera")

for(i in 1:6){
fld <- with(phen.dat3[which(phen.dat3$Order==ords[i]),], interp(x = abs(lat), y = T0, z = (Ngen+1), duplicate=TRUE))

gdat <- interp2xyz(fld, data.frame=TRUE)

p3d= ggplot(gdat) + 
  aes(x = x, y = y, z = z, fill = z) + 
  geom_tile() + 
  coord_equal() +
  geom_contour(color = "white", alpha = 0.5) + 
  scale_fill_distiller(palette="Spectral", na.value="white", name="Number\ngenerations",trans = "log", breaks=c(1,2, 5,10,20)) + 
  theme_bw(base_size=18)+xlab("latitude (°)")+ylab("T0 (°C)")+ggtitle(ords[i])+ theme(legend.position="bottom")+xlim(c(0,55))+ylim(c(-2,23))

if(i==1) f1= p3d
if(i==2) f2= p3d
if(i==3) f3= p3d
if(i==4) f4= p3d
if(i==5) f5= p3d
if(i==6) f6= p3d
} 

#plot
setwd(paste(fdir,"figures\\",sep="") )
pdf("FitnessSurface.pdf", height = 6, width = 10)

fig6= grid_arrange_shared_legend(f1,f2,f3,f4,f5,f6, ncol = 3, nrow = 2)

dev.off()

#============================================
#Fitness surface by latitude

lats= c("0-25°", "25-35°","35-40°","40-50°","50-61°")

phen.dat3$lcut= cut(abs(phen.dat3$lat), breaks=c(0,25,35, 40,50,90) )

lcuts= sort(unique(phen.dat3$lcut))

for(i in 1:5){
  fld <- with(phen.dat3[which(phen.dat3$lcut ==lcuts[i]),], interp(x = T0, y = G, z = (Ngen+1), duplicate=TRUE))
  
  gdat <- interp2xyz(fld, data.frame=TRUE)
  
  p3d= ggplot(gdat) + 
    aes(x = x, y = y, z = z, fill = z) + 
    geom_tile() + 
    coord_equal() +
    geom_contour(color = "white", alpha = 0.5) + 
    scale_fill_distiller(palette="Spectral", na.value="white", name="number\ngenerations",trans = "log", breaks=c(1,2, 5,10,20)) +
    theme_bw(base_size=18)+xlab("T0 (°C)")+ylab("G")+ggtitle(lats[i])+ theme(legend.position="right")+ coord_fixed(ratio = 0.01) +ylim(c(0,1500))+xlim(c(-2,23))
  
  if(i==1) f1= p3d
  if(i==2) f2= p3d
  if(i==3) f3= p3d
  if(i==4) f4= p3d
  if(i==5) f5= p3d
} 

#plot
setwd(paste(fdir,"figures\\",sep="") )
pdf("FitnessSurface_byLat.pdf", height = 6, width = 10)

fig6= grid_arrange_shared_legend(f1,f2,f3,f4,f5, ncol = 3, nrow = 2)

dev.off()

#============================================
#STATS FOR TEXT

phen.dat2= as.data.frame( cbind(1:nrow(phen.dat), dddat[,c("Species","Order","lon","lat", "T0","G")],ngens))

#ngens metrics
phen.dat2$arith.mean=  rowMeans(phen.dat2[,c(8:53)], na.rm = TRUE)
phen.dat2$geo.mean= geometric.mean( t(phen.dat2[,c(8:53)]) )
phen.dat2$geo.mean[is.nan(phen.dat2$geo.mean)]=NA
#restrict columns
phen.dat2=phen.dat2[,c("Species","Order","lon","lat", "T0","G","arith.mean","geo.mean")]

#restrict to orders with sufficient data
ords= c("Coleoptera","Diptera","Hemiptera","Homoptera","Hymenoptera","Lepidoptera", "Neuroptera", "Thysanoptera")
dat1= phen.dat2[phen.dat2$Order %in% ords,]

#------------------------------
#Check G vs T0
mod1<- lme(G~T0*Order, data =dat1, na.action = na.omit, random = ~1|Species)
anova(mod1)
summary(mod1) 

#wihout accounting for species
mod1<- lm(G~T0*Order, data=dat1)

#Table 1. Anova for T0 (=BDT.C). Mixed effects model (random = 1|species)
#NOTE: main effects of lat and Order (no sig. interaction); large random effect of species

mod3m <- lme(T0~abs(lat)*Order, data =dat1, na.action = na.omit, random = ~1|Species)
anova(mod3m)
summary(mod3m)

#check other fators
dat.n= dat2[dat2$Order %in% ords,]
mod3m <- lme(G~abs(lat)*Order+parasitoid+pest+aquatic+parasitoid, data =dat.n, na.action = na.omit, random = ~1|Species)

#Table 2. Anova for DDD (=EADDC). Mixed effects model (random = 1|species)
#NOTE: main effects of lat and Order, and sig. interaction large random effect of species

mod6m <- lme(G~abs(lat)*Order, data =dat1, na.action = na.omit, random = ~1|Species)
anova(mod6m)
summary(mod6m)

#Table 3. Anova for Mean # generations (log-transformed: log(ngen+1)) Mixed effects model (random = 1|species)NOTE: main effects of lat and Order, interaction;  first and 2nd-order effects of traits T0 and DDD, and their interactions.  Interaction between T0 and latitude (marginal)

modn1 <- lme(log(arith.mean+1)~abs(lat)+Order+T0+G+T0:G+I(T0*T0)+I(G*G)+I(T0*T0):I(G*G)+abs(lat):Order+abs(lat):T0+abs(lat):G, data = dat1, na.action = na.omit, random = ~1|Species)
anova(modn1)
summary(modn1)

library(lme4)
modn2 <- glmer(log(arith.mean+1)~abs(lat)+Order+T0+G+T0:G+I(T0*T0)+I(G*G)+I(T0*T0):I(G*G)+abs(lat):Order+abs(lat):T0+abs(lat):G+1|Species, data = dat1, na.action = na.omit, family=poisson)

#PHENOLOGY SHIFTS VS LAT
#phen.slopes1= phen.slopes[phen.slopes$Order %in% ords,]

mod1 <- lme(phen.m~abs(lat)*Order, data =phen.slopes1, na.action = na.omit, random = ~1|Species)
mod1 <- lme(dtemp.m~abs(lat)*Order, data =phen.slopes1, na.action = na.omit, random = ~1|Species)
mod1 <- lme(ngen.m~abs(lat)*Order, data =phen.slopes1, na.action = na.omit, random = ~1|Species)
anova(mod1)
summary(mod1)

phen.m
dtemp.m
ngen.m

#Fig. 7 stats
mod1 <- lme(ngen.m~phen.m+dtemp.m+Order, data =phen.slopes1, na.action = na.omit, random = ~1|Species)
anova(mod1)
summary(mod1)



