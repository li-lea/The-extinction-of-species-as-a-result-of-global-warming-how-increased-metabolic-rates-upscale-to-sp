### Loading libraries #######
#install.packages("biomod2")
#install.packages("rgdal")
#install.packages("raster")
#install.packages("rgeos")
#install.packages("fields")
#install.packages("rasterVis")
library(rgeos)
library(rgdal)
library(raster)
#library(biomod2) # species distribution models
library(rasterVis)
#library(fields)


### Reading in Landscapes:
readinToydata<-function(filename){
  filename<-paste(getwd(),"/",filename,".TXT",sep="")
  ToyLandscape<-read.table(file=filename,sep = "", header=T)
  
  ToyBackground = brick("predictors_lowBayern_Temperature.tif")
  
  Germany_Level1 <- readOGR("DEU_adm1.shp")
  Germany_Level1 <-  gBuffer(Germany_Level1,byid=TRUE, width=0) # used on laptop
  xmin=8.9; xmax=13.9; ymin=47.2; ymax=50.6
  extent <- c(xmin, xmax, ymin, ymax)
  BavariaLines <- crop(Germany_Level1,extent) #no need to aggegate, since parameters are the same
  
  LandscapeList<-list(ToyLandscape, ToyBackground, BavariaLines)
  
  return (LandscapeList)      
}


### Reading in SDMs:
readinSDM<-function(filename){
  SDMs<- read.csv(file = paste(getwd(),'/', filename, '.csv', sep=""), header=T)
  Xs<-sort(as.numeric(levels(as.factor(as.character(SDMs$lon)))))
  Ys<-sort(as.numeric(levels(as.factor(as.character(SDMs$lat)))))
  xmin=min(SDMs$lon)
  xmax=max(SDMs$lon)
  ymin=min(SDMs$lat)
  ymax=max(SDMs$lat) 
  extents <- c(xmin, xmax, ymin, ymax)
  SDMmatrix<-matrix(NA,ncol=length(Xs),nrow=length(Ys))
  Temperaturematrix<-matrix(NA,ncol=length(Xs),nrow=length(Ys))
  
  for(x in 1:length(Xs)){
    SDMs_2<-SDMs[which(SDMs$lon==Xs[x]),]
    if(nrow(SDMs_2)>0){
      for(y in 1:length(Ys)){
        SDMs_3<-SDMs_2[which(SDMs_2$lat==Ys[y]),]
        if(nrow(SDMs_3)>0){
          SDMmatrix[y,x]<- SDMs_3$curclim_GBM
          Temperaturematrix[y,x]<- SDMs_3$bio1_mean*0.1
        }
      }    
    }
  }
  
  dat1=list()
  dat1$x=seq(extents[1],by=0.5,len=(extents[2]-extents[1])/0.5+1)
  dat1$y=seq(extents[3],by=0.5,len=(extents[4]-extents[3])/0.5+1) #For lowestREs_SaoPaolo: dat1$y=seq(extent(Elevation)[3]+0.03,by=res(Elevation)[1],len=nrow(Elevation)-1)
  dat1$z=t(SDMmatrix)
  r=raster(dat1)
  SDMBrick<-brick(r)
  
  dat1=list()
  dat1$x=seq(extents[1],by=0.5,len=(extents[2]-extents[1])/0.5+1)
  dat1$y=seq(extents[3],by=0.5,len=(extents[4]-extents[3])/0.5+1) #For lowestREs_SaoPaolo: dat1$y=seq(extent(Elevation)[3]+0.03,by=res(Elevation)[1],len=nrow(Elevation)-1)
  dat1$z=t(Temperaturematrix)
  r=raster(dat1)
  
  ToyBackground<-brick(r)
  #plot(ToyBackground)  
  LandscapeList<-list(SDMBrick, ToyBackground)
  
  return (LandscapeList)      
}

BuildLandscape<-function(LandscapeName){
  if(!LandscapeName %in% c("Bavaria","Toydata","Theoretical","Alps")){print("invalid LandscapeName"); break}   
 if(LandscapeName=="Toydata"){
   olwd<-getwd()
   setwd("Toydata")#TODO set only in filename
   SDM=FALSE
   InputLanscapefilename= "Landscape_Variables_LowRes_Bayern"  # if "SDM" use "NCaryo_Plants_grid_df"
   if(!SDM) {
     #Reading-in a landscape file:
     LandscapeList<-readinToydata(InputLanscapefilename) #Reading in the landscape
     ToyLandscape<-LandscapeList[[1]] #TODO: maybe this can be create from the file of temperaure
     ToyBackground<- LandscapeList[[2]] 
     AdminLines<- LandscapeList[[3]]   
     #Setting dimensions from input file
     Xlength<-max(ToyLandscape$Y)  
     Ylength<-max(ToyLandscape$x) 
   }
   if(SDM) {
     #Reading-in a SDM file:
     LandscapeList<-readinSDM(InputLanscapefilename) #Reading in the SDM
     ToyLandscape<-LandscapeList[[1]] #TODO: maybe this can be create from the file of temperaure
     ToyBackground<- LandscapeList[[2]] 
     #Setting dimensions from input file
     Xlength<-nrow(ToyLandscape) 
     Ylength<-ncol(ToyLandscape)
     AdminLines<-NULL # only for the return
   } 
   setwd(olwd)
   return(Landscape<-list("ToyLandscape"=ToyLandscape,"ToyBackground"=ToyBackground,"AdminLines"=AdminLines,"Xlength"=Xlength,"Ylength"=Ylength,"SDM"=SDM))#,"InputLandscapeFile"=InputLandscapeFile))
 }
  
  
 if(LandscapeName=="Bavaria"){#TODO new data Berchtesgaden/alpenvorland, da rechenzeit zu lang
   olwd<-getwd()
   setwd("data/environment/")
   BavariaShape<-readRDS("bavaria_shape.rds")
   Grassv1<-raster("grassland_1km_v1.tif")
   Grassv2<-raster("grassland_1km_v2.tif")
   
   stacklist<-list.files("clc_landuse_tif",pattern = "landuse.*\\.tif$", all.files = T, full.names = T)
   Landuse<-stack(stacklist)
   #nature_types<-subset(landuse,clc==c("211","221","222","231","242","243","311","312","313","321","322","324","331","332","333","334","335","411","412","421","423"))
   
   mean_temp<-raster("bavaria_mean_temp.tif")
   mean_prec<-raster("bavaria_mean_prec.tif")
   mean_prec_warm<-raster("bavaria_mean_prec_warm.tif")
   mean_temp_warm<-raster("bavaria_mean_temp_warm.tif")
   season_prec<-raster("bavaria_season_prec.tif")
   season_temp<-raster("bavaria_season_temp.tif")
   TempData<-list("mean_temp"=mean_temp,"season_temp"=season_temp,"mean_temp_warm"=mean_temp_warm)
   PrecData<-list("mean_prec"=mean_prec,"season_prec"=season_prec,"mean_prec_warm"=mean_prec_warm)
   
   Xlength<-ncol(mean_temp) 
   Ylength<-nrow(mean_temp)
   
   Landscape<-list("Landuse"=Landuse, "Grassv1"=Grassv1, "Grassv2"=Grassv2, "TempData"=TempData, "PrecData"=PrecData, "BavariaShape"=BavariaShape,"Ylength"=Ylength,"Xlength"=Xlength)
   setwd(olwd)
   return (Landscape) 
 }
  
  if(LandscapeName=="Alps"){#TODO new data Berchtesgaden/alpenvorland, da rechenzeit zu lang
    olwd<-getwd()
    setwd("data/environment/")
    #BavariaShape<-readRDS("bavaria_shape.rds")
    Grass<-raster("alps_grass.tif")
   # Grassv2<-raster("grassland_1km_v2.tif")
    
    #stacklist<-list.files("clc_landuse_tif",pattern = "landuse.*\\.tif$", all.files = T, full.names = T)
    #Landuse<-stack(stacklist)
    #nature_types<-subset(landuse,clc==c("211","221","222","231","242","243","311","312","313","321","322","324","331","332","333","334","335","411","412","421","423"))
    
    mean_temp<-raster("alps_mean_temp.tif")
    mean_prec<-raster("alps_mean_prec.tif")
    mean_prec_warm<-raster("alps_mean_prec_warm.tif")
    mean_temp_warm<-raster("alps_mean_temp_warm.tif")
    season_prec<-raster("alps_season_prec.tif")
    season_temp<-raster("alps_season_temp.tif")
    TempData<-list("mean_temp"=mean_temp,"season_temp"=season_temp,"mean_temp_warm"=mean_temp_warm)
    PrecData<-list("mean_prec"=mean_prec,"season_prec"=season_prec,"mean_prec_warm"=mean_prec_warm)
    
    Xlength<-ncol(mean_temp) 
    Ylength<-nrow(mean_temp)
    
    Landscape<-list("Grass"=Grass, "TempData"=TempData, "PrecData"=PrecData, "Ylength"=Ylength,"Xlength"=Xlength)
    setwd(olwd)
    return (Landscape) 
  }
 

}

###calibrationfunction for shape values of the beta distribution for mortality Beverton
shapeValues <- function(m,alpha){
  beta <-  (alpha-(alpha*m))/m
  return(beta)
}

#######function for parameter constant calibration
ParamCalibration <-function(ObjectiveParamValue,Exponent,MassForCalibration,temperatureForCalibration, E,k){
  #calculating a parameter constant value that will generate via MTE 
  #the aimed parameter value under a reference body mass and temperature
  ParamConst<- ObjectiveParamValue/((MassForCalibration^(Exponent))*exp(-E/(k*temperatureForCalibration)))
  return(ParamConst)
}

####Initialization function#########
Initialization<-function(Ylength,
                         Xlength,
                         Timesteps,
                         LandscapeName,
                         HabitatType,
                         ProbHabitat,
                         DistMax,
                         MeanDispersalDistance,
                         InvasionSwitch,
                         Xstartmin,
                         Xstartmax,
                         Ystartmin,
                         Ystartmax,
                         ToyBackground,
                         optimumTemperature,
                         toleranceTemperature,
                         Landscape,
                         CutoffSuitability,
                         metabolic,
                         Mass,
                         MassForCalibration,
                         sdlog_M,
                         temperature,
                         temperatureForCalibration,
                         sd_T,
                         Rep_modelName,
                         R,
                         carryingcapacity,
                         m,
                         C,
                         sdlog_R,
                         sdlog_K,
                         alpha,
                         sd_C,
                         UseCataMortality,
                         cataprob,
                         sd_cata_mortality,
                         initialstoch_R,
                         initialstoch_K,
                         initialstoch_m,
                         initialstoch_C,
                         stoch_R,
                         stoch_K,
                         stoch_m,
                         stoch_C,
                         Exponent_R,
                         Exponent_K,
                         Exponent_m,
                         Exponent_C,
                         E,
                         k,
                         N=N,
                         H=H,
                         p=p,
                         temperature_scaling,
                         StartTempChange,
                         StopTempChange,
                         climate_change,
                         landscapeIndex){
                         #InputLandscapeFile
  
  CalibratedConst_R <- ParamCalibration(ObjectiveParamValue=R,Exponent=Exponent_R,MassForCalibration = MassForCalibration,temperatureForCalibration =temperatureForCalibration,E=E,k=k)
  CalibratedConst_K <- ParamCalibration(ObjectiveParamValue=carryingcapacity,Exponent=Exponent_K,MassForCalibration = MassForCalibration,temperatureForCalibration =temperatureForCalibration,E=E,k=k)
  CalibratedConst_m <- ParamCalibration(ObjectiveParamValue=m,Exponent=Exponent_m,MassForCalibration = MassForCalibration,temperatureForCalibration =temperatureForCalibration,E=E,k=k)
  CalibratedConst_C <- ParamCalibration(ObjectiveParamValue=C,Exponent=Exponent_C,MassForCalibration = MassForCalibration,temperatureForCalibration =temperatureForCalibration,E=E,k=k)

  if(LandscapeName %in% c("Toydata","Bavaria","Alps")){
    Ylength<-Landscape$Ylength
    Xlength<-Landscape$Xlength
  }
 # if(InputLandscapeFile=="Dataframe"|| "SDM"){
#    Ylength<-Landscape_data$Ylength
#    Xlength<-Landscape_data$Xlength
#    TemperatureBrick<-Landscape_data$TemperatureBrick
#    Landscape<-Landscape_data$Landscape
#  }
  if(LandscapeName == "Theoretical" && HabitatType == "MetabolicRange"){
    Ylength<-N
    Xlength<-N
  }
  if(LandscapeName=="Toydata"){
    ToyBackground<-Landscape$ToyBackground
    ToyLandscape=Landscape$ToyLandscape
  }
 
  
  #Habitat/Landscape:  
  #Temperature:
  Temp<-array(data=0,dim=c(Ylength,Xlength,Timesteps))
  
  if(HabitatType=="Random"){#TODO or type Theoretical?
    #Temp:
    Temp[,,1]<-array(rnorm(Ylength*Xlength,mean=temperature,sd = sd_T),dim = c(Ylength,Xlength))
    for (i in 2:Timesteps){
      for (x in 1:Xlength){ 
        for(y in 1:Ylength){
          #if(i==1) Temp[y,x,i]<-rnorm(1,temperature,sd_T) #write the value of the initial Temperature
         #  if(i>1) {#TODO: if call for normal distribution
            Temp[y,x,i]=Temp[y,x,i-1]+rnorm(1,0,sd_T) #write the value of the initial Temperature
        #  }
        }
      }
    }
    #Habitat
    Habitat<-array(data=rbinom(Ylength*Xlength, 1, ProbHabitat),dim=c(Ylength,Xlength,Timesteps))
  }
  #image.plot(Habitat[,,1])
  #image.plot(Temp[,,1]) 
  
  if(HabitatType=="MetabolicRange") {
    maxProb<-dnorm(x=optimumTemperature,mean=optimumTemperature,sd=toleranceTemperature) 
    Habitat<-array(0, dim=c(Ylength,Xlength,Timesteps))
    
    if(LandscapeName=="Toydata"){
      for(y in 1:Ylength){
        ToyLandscape2<-ToyLandscape[which(ToyLandscape$x==y),]
        for(x in 1:Xlength){  
          ToyLandscape3<-ToyLandscape2[which(ToyLandscape2$Y==x),]
          Temp[y,x,]=ToyLandscape3[1,6]+273.15
          Suitability_Temp<-dnorm(ToyLandscape3[1,6],optimumTemperature,toleranceTemperature)/maxProb # #=> to use normalization when adding other environmental axis
          Habitat[y,x,]<-Suitability_Temp
          if(Habitat[y,x,1]<0.1) Habitat[y,x,]<-0 #TODO: make the cutoff a parameter
        }
      }
    }
    if(LandscapeName=="Bavaria"|| LandscapeName=="Alps"){
      Temp<-array(data=0,dim=c(Ylength,Xlength,Timesteps))
      Temp[,,1]<-getValues(Landscape$TempData$mean_temp,format="matrix")/10#TODO variable
      Temp[,,1]<-Temp[,,1]+273.15
      Habitat<-array(dnorm(Temp,optimumTemperature,toleranceTemperature)/maxProb,dim=c(Ylength,Xlength,Timesteps)) # #=> to use normalization when adding other environmental axis
    }
    if(LandscapeName=="Theoretical"){
      #L=lscp(N=N,H=H,p=p)
      p = paste("/Landscape_", toString(landscapeIndex), ".csv", sep="")#########################################
      L=as.matrix(read.csv(file=p))
      Temp[,,1]=scale(L,center = TRUE, scale = FALSE)
      Temp[,,1]=Temp[,,1]*temperature_scaling+temperature
      #array(rnorm(Ylength*Xlength,mean=temperature,sd = sd_T),dim = c(Ylength,Xlength))
      Temp=array(rnorm(Ylength*Xlength*Timesteps,mean=Temp[,,1],sd=sd_T),dim = c(Ylength,Xlength,Timesteps))
      #Temp=array(rep(Temp,Timesteps),dim = c(N,N,Timesteps))
      if(climate_change>0){
        for (i in StartTempChange:StopTempChange) {
          Temp[,,i]<-Temp[,,i-1]+climate_change
        }
      }
      Habitat<-array(dnorm(Temp,optimumTemperature,toleranceTemperature)/maxProb,dim=c(N,N,Timesteps)) # #=> to use normalization when adding other environmental axis
      #TODO habitat unter schwellunwert null setzen # zeile 290
      }
  #  Temp[1,1,1]
  #  Temp[1,1,10]
  #   image(Temp[,,1])
 
   #  identical(Temp[,,1],Temp[,,10])
  }
  #image.plot(Habitat[,,1])
  #image.plot(Temp[,,1]) 
  
  #TODO: does not function anymore, deprecated
  if(HabitatType=="SDM"){ #TODO: does not function anymore, deprecated
    Habitat<-array(NA, dim=c(Xlength,Ylength,Timesteps))
    for(y in 1:Ylength){
      for(x in 1:Xlength){  
        if (!is.na(SDMBrick[(Xlength-x),y])) {
          Habitat[x,y,]<-SDMBrick[(Xlength-x),y]
          #if(Habitat[x,y,]<0.05) Habitat[y,x,]<-0
        }
      }
    }
    #image(SDMBrick)
    # image(Habitat[,,1])
    Habitat2<-array(NA, dim=c(Ylength,Xlength,Timesteps))
    for(time in 1:Timesteps){
      Habitat2[,,time]<-t(Habitat[,,time])
    }
    Habitat<-Habitat2
    # image(Habitat[,,1])
  }  
  
  #Abudances:
  Abundances<-array(data=0,dim=c(Ylength,Xlength,Timesteps))
  if(InvasionSwitch==T){
    for(y in Ystartmin:Ystartmax){
      for (x in Xstartmin:Xstartmax){
        if((!is.na(Habitat[y,x,1]))&&(Habitat[y,x,1]>CutoffSuitability)){
          Abundances[y,x,1]=round(runif(1,min=0,max=carryingcapacity))
        }
      }
    }
  }else{ 
    for (x in 1:Xlength){
      for(y in 1:Ylength){
        if((!is.na(Habitat[y,x,1]))&&(Habitat[y,x,1]>0)){
          Abundances[y,x,1]=round(runif(1,min=0,max=carryingcapacity))
        } 
      }
    }
    #image(Abundances[,,1]) 
  }
 # image.plot(Abundances[,,1]) 
  
  TotalAbundance<-vector(mode="numeric", length=Timesteps)
  
  #Biomass:
  Biomass<-array(data=rlnorm(n=Ylength*Xlength*Timesteps, meanlog = log(Mass), sdlog =sdlog_M),dim=c(Ylength,Xlength,Timesteps))

  #TODO: Carrying capacity array = input Carrying capacity (in biomass)/ local individual body mass #only for MTE=F 
  carryingcapacity_array<-array(data=0,dim=c(Ylength,Xlength,Timesteps))
  for (i in 1:Timesteps){
    for (x in 1:Xlength){
      for(y in 1:Ylength){
        carryingcapacity_array[y,x,i] <-Calculation_K(carryingcapacity=carryingcapacity,Biomass=Biomass[y,x,i])
      }
    }
  }  

  species_data<-list(
    RData=list(
      R_array                 =array(data=R,dim=c(Ylength,Xlength,Timesteps)),
      MTEconst                =CalibratedConst_R,
      ParaExponent            =Exponent_R),
    
    KData=list(
      K_array                 =array(data=carryingcapacity_array,dim=c(Ylength,Xlength,Timesteps)),
      MTEconst                =CalibratedConst_K,
      ParaExponent            =Exponent_K),
    
    MData=list(
      m_array                 =array(data=m,dim=c(Ylength,Xlength,Timesteps)),
      MTEconst                =CalibratedConst_m,
      ParaExponent            =Exponent_m),
    
    CData=list(
      C_array                 =array(data=C,dim=c(Ylength,Xlength,Timesteps)),
      MTEconst                =CalibratedConst_C,
      ParaExponent            =Exponent_C)
  )  
  
  #mortality Array for Beverton
  if(Rep_modelName=="Beverton"){
    m_arrayForBeta<-array(data=0,dim=c(Ylength,Xlength,Timesteps))
    for (i in 1:Timesteps){
      for (x in 1:Xlength){
        for(y in 1:Ylength){
          m_arrayForBeta[y,x,i] <-shapeValues(m=species_data$MData$m_array[y,x,i],alpha=alpha)
        }
      }
    }
  }
  
  
  
  #MTE 
  if(metabolic){
    for(x in 1:length(species_data)){
      species_data[[x]][[1]]<-metabolicRate(ParamConst =  species_data[[x]][[2]], Exponent =  species_data[[x]][[3]], Mass=Biomass, temperature = Temp, E=E, k=k)
    }
  }
    
  # initial stochasticity
  #if(initialstoch_R) {  #species_data$RData$R_array[,,1] <- mapply(rnorm,species_data$RData$R_array[,,1],n=1,sd=sd_r)
  #  species_data$RData$R_array[,,1]<-array(rlnorm(Ylength*Xlength,meanlog =log(species_data$RData$R_array[,,1]),sdlog =sdlog_R),dim = c(Ylength,Xlength))}
  #if(initialstoch_K) {  species_data$KData$K_array[,,1]<-array(rlnorm(Ylength*Xlength,meanlog=log(species_data$KData$K_array[,,1]),sdlog=sdlog_K),dim = c(Ylength,Xlength))} 
  #if(initialstoch_m) {  species_data$MData$m_array[,,1]<-array(rbeta(Ylength*Xlength,shape1=alpha, shape2=m_arrayForBeta),dim = c(Ylength,Xlength))}
  #if(initialstoch_C) {  species_data$CData$C_array[,,1]<-array(rnorm(Ylength*Xlength,mean=species_data$CData$C_array[,,1],sd = sd_C),dim = c(Ylength,Xlength))}
  
  #carryingcapacity_array<-carryingcapacity/((Biomass[y,x,i]^Exponent))*T) #^(Exponent_K)
  
  #species_data$RData$R_array[,,1] <- mapply(rnorm,species_data$RData$R_array[,,1],n=1,sd=sd_r)
  if(initialstoch_R) {  species_data$RData$R_array[,,]<-array(rep(rlnorm(Ylength*Xlength,meanlog=log(species_data$RData$R_array[,,1]),sdlog=sdlog_R),Timesteps),dim = c(Ylength,Xlength,Timesteps))}
  if(initialstoch_K) {  species_data$KData$K_array[,,]<-array(rep(rlnorm(Ylength*Xlength,meanlog=log(species_data$KData$K_array[,,1]),sdlog=sdlog_K),Timesteps),dim = c(Ylength,Xlength,Timesteps))} 
  if(initialstoch_m && Rep_modelName=="Beverton") {  species_data$MData$m_array[,,]<-array(rep(rbeta(Ylength*Xlength,shape1=alpha, shape2=m_arrayForBeta),Timesteps),dim = c(Ylength,Xlength,Timesteps))}
  if(initialstoch_C) {  species_data$CData$C_array[,,]<-array(rep(rnorm(Ylength*Xlength,mean=species_data$CData$C_array[,,1],sd=sd_C),Timesteps),dim = c(Ylength,Xlength,Timesteps))}
  
  # all stochasticity
  if(stoch_R) {  species_data$RData$R_array[,,]<-array(rlnorm(Ylength*Xlength*Timesteps,meanlog=log(species_data$RData$R_array[,,]),sdlog=sdlog_R),dim = c(Ylength,Xlength,Timesteps))}
  if(stoch_K) {  species_data$KData$K_array[,,]<-array(rlnorm(Ylength*Xlength*Timesteps,meanlog=log(species_data$KData$K_array[,,]),sdlog=sdlog_K),dim = c(Ylength,Xlength,Timesteps))} 
  if(stoch_m && Rep_modelName=="Beverton") {  species_data$MData$m_array[,,]<-array(rbeta(Ylength*Xlength*Timesteps,shape1=alpha, shape2=m_arrayForBeta),dim=c(Ylength,Xlength,Timesteps))}
  if(stoch_C) {  species_data$CData$C_array[,,]<-array(rlnorm(Ylength*Xlength*Timesteps,mean=species_data$CData$C_array[,,],sd=sd_C),dim = c(Ylength,Xlength,Timesteps))}
  
          
          
  #Catastrophic Mortality:
  CataMortalityArray<-array(data=0,dim=c(Ylength,Xlength,Timesteps))
  for (i in 1:Timesteps){
    for (x in 1:Xlength){
      for(y in 1:Ylength){
        #TODO: make the stochasticity here also a simulation config
        #TODO: check the best distribution
        CataMortalityArray[y,x,i]=abs(rnorm(1,cataprob,sd_cata_mortality)) #simulation parameter to check to do stochastic or not
    }
   }
  }
  
  #Dispersing juveniles:
  DispersingUnits<-array(data=0,dim=c(Ylength+2*DistMax,Xlength+2*DistMax,Timesteps))
  
  #Dispersal Kernel:
  DispersalKernel<-DispersalNegExpKernel(DistMax,MeanDispersalDistance)
  
  #Adding everything to a list, because the return functions returns only one object
  #sim_data<-list("Habitat"=Habitat,"Abundances"=Abundances,"Biomass"=Biomass,"Temp"=Temp,"R"=R,"carryingcapacity"=carryingcapacity,"m"=m,"C"=C,"MortalityArray"=MortalityArray,"DispersingUnits"=DispersingUnits,"DispersalKernel"=DispersalKernel)
  sim_data<-list("Temp"=Temp,"Habitat"=Habitat,"Abundances"=Abundances,"R_array"=species_data$RData$R_array,"K_array"=species_data$KData$K_array,"m_array"=species_data$MData$m_array,"C_array"=species_data$CDataC_array,"CataMortalityArray"=CataMortalityArray,"DispersingUnits"=DispersingUnits,"DispersalKernel"=DispersalKernel,"TotalAbundance"=TotalAbundance)
  
  return(sim_data)
}


###calibrationfunction for shape values of the beta distribution
#shapeValues <- function(m,alpha){
 # beta <-  alpha/(alpha*m)
#  return(beta)
#}

#Calculation Carryingcapacity as a function of biomass
Calculation_K<-function(carryingcapacity,Biomass){
  carryingcapacityIndiv <- carryingcapacity/Biomass #carryingcapacity in Indiv.
  return(carryingcapacityIndiv)
}

#######function of biomass and temperature
metabolicRate <- function(ParamConst,Exponent,Mass,temperature,E,k){ #TODO MTE for extinction rate with an activation energy of 0.47eV
  #bodymass dependent - demographic
  Mass1 <-Mass^(Exponent)
  #temperature dependent - environment 
  T1 <- exp(-E/(k*temperature))
  ModifiedParameter <- ParamConst*Mass1*T1
  #TODO: check whether the return does what it should (it was not there before)
  return(ModifiedParameter) 
}



###### functions for main loop ####

####### Reproduction functions
reproduction <- function(Rep_modelName, stoch_N, N, R, carryingcapacity, m, C){
  ###Beverton function
  #TODO: check the Beverton-Holt equation
  if (Rep_modelName == 'Beverton') {
    Bevertondeaths<-sum(rbinom(N,1,m)) #create a function for mortality in case Equation = 'Beverton'
    N2 <-N-Bevertondeaths+ R*N*1/(1*N+(R-1)/carryingcapacity) # excluding dying ind and adding new ind => have to simulate mortality as well in a separate function
    
    if (N2 <= 0) {
      N2 = 0
    }
    return (N2)
  }
  ###Ricker function
  if (Rep_modelName == 'Ricker') {
    N2 <- N*exp(R*(1-N/carryingcapacity))
    return (N2)
  }
  ## RickerAllee
  if (Rep_modelName == 'RickerAllee') {
    N2 <- N*exp(R*((4*(carryingcapacity-N)*(N-C))/((carryingcapacity-C)^2)))
    return (N2)
  }
}

####### Dispersal kernels  
DispersalNegExpKernel<-function(Dispersalbuffer,MeanDispersalDistance){
  x <- 2*Dispersalbuffer + 1
  y <- 2*Dispersalbuffer + 1
  sum<-0
  spDispKernel = matrix(0,nrow=x,ncol=y)
  
  for(i in 1:x) {
    for(j in 1:y) {
      r <- sqrt(abs(i-(Dispersalbuffer+ 1))*abs(i-(Dispersalbuffer+ 1)) + abs(j-(Dispersalbuffer+ 1))*abs(j-(Dispersalbuffer+ 1)))
      dispersal<-DispersalNegExpFunction(MeanDispersalDistance,r)
      sum <- sum + dispersal
      spDispKernel[i,j] <- dispersal
    }
  }
  spDispKernel
  #Normalizing
  for(i in 1:x) {
    for(j in 1:x) {
      spDispKernel[i,j] <- spDispKernel[i,j]/sum
    }
  }
  return (spDispKernel)  
}

DispersalNegExpFunction<-function(alpha, r) {
  N <- 2*pi*(alpha*alpha)
  p <- (1/N )* exp(- r/alpha)
  if(p<0) p <-0
  return (p)
}

### Dispersal:
Dispersal<-function(N,DistMax,ProbDisp,DispersingUnits, x,y,DispersalType,DispersalKernel){
  if(DispersalType=='Kernel') DispersingUnits<-KernelDispersal(N,DistMax,DispersingUnits,y,x,DispersalKernel)
  if(DispersalType=='Individual'){
    DispersingIndividuals<-round(N*ProbDisp) #calculating how many individuals will leave the cell
    DispersingUnits[y+DistMax,x+DistMax]<-DispersingUnits[y+DistMax,x+DistMax]+N-DispersingIndividuals
    #loop over dispersing individuals
    if(DispersingIndividuals>0) DispersingUnits<-IndividualDispersal(DispersingIndividuals,DistMax,DispersingUnits,y,x)
  }
  return(DispersingUnits)
}

### Dispersal types:
IndividualDispersal<-function(DispersingIndividuals,DistMax,DispersingUnits,y,x){
  for(ind in 1:DispersingIndividuals){
    DirX<-sample(-1:1,1,replace=T)
    DirY<-sample(-1:1,1,replace=T)
    RandomDist<-round(runif(1,min=1,max=DistMax))
    DispersingUnits[y+DistMax+DirY*RandomDist,x+DistMax+DirX*RandomDist]<-DispersingUnits[y+DistMax+DirY*RandomDist,x+DistMax+DirX*RandomDist]+1
  } 
  return(DispersingUnits)
}
KernelDispersal<-function(N,DistMax,DispersingUnits,y,x,DispersalKernel){#DispersingIndividuals,DistMax,DispersingUnits,y,x,DispersalKernel){
  DispersingUnits[y:(y+2*DistMax),x:(x+2*DistMax)]<-DispersingUnits[y:(y+2*DistMax),x:(x+2*DistMax)]+N*DispersalKernel# +DispersingIndividuals*DispersalKernel
  return(DispersingUnits)
}
###Recruitment:
DispersalSurvival<-function(Abundances, DispersingUnits, Habitat,Xlength,Ylength,DistMax,CutoffSuitability,stoch_N){
  for (x in 1:Xlength){
    for(y in 1:Ylength){
      if((!is.na(Habitat[y,x]))&&(Habitat[y,x]>CutoffSuitability)) {
        if(stoch_N) Abundances[y,x]= rpois(1,DispersingUnits[y+DistMax,x+DistMax]) 
        if(!stoch_N) Abundances[y,x]= round(DispersingUnits[y+DistMax,x+DistMax]) 
      }
      else Abundances[y,x]= 0
    }
  }
  return(Abundances)
}

###Catastrophic Mortality:
CatastrophicMortality<-function(Abundances,Xlength,Ylength,MortalityArray){
   for (x in 1:Xlength){
    for(y in 1:Ylength){
      Abundances[y,x]=Abundances[y,x]-sum(rbinom(Abundances[y,x],1,MortalityArray[y,x])) #TODO: convert rate into probability  p = 1-e^-rt
   }
  }
 return(Abundances)
}

#### Visualization function######
Visualization<-function(Abundances,TotalAbundance,carryingcapacity,i,LandscapeName,Habitats, ToyBackground, AdminLines, SDM, saveVisualization, StoragePath, RunName, width, height, unit){
 if(saveVisualization){
   png(filename = paste(StoragePath,"/",RunName,"/",RunName,"_",i,".png",sep = ""), width = width, height = height, units = as.character(unit))
 } 
   par(mfrow=c(1,3),mar=c(4,6,1.5,3),oma=c(1,1,1,1), las=1)
  #TODO function for bavaria
  if(LandscapeName=="Toydata"){
   if(!SDM){
     # Habitat suitability
    dat1=list()
    dat1$x=seq(extent(ToyBackground)[1],by=res(ToyBackground)[1],len=ncol(ToyBackground))
    dat1$y=seq(extent(ToyBackground)[3],by=res(ToyBackground)[1],len=nrow(ToyBackground)-1)
    dat1$z=Habitats[,,i]
    r=raster(dat1)
    plot(r)
    title(main = "Habitat suitability", font.main = 6)
    lines(AdminLines)
    #Abundance distributions:
    dat1=list()
    dat1$x=seq(extent(ToyBackground)[1],by=res(ToyBackground)[1],len=ncol(ToyBackground))
    dat1$y=seq(extent(ToyBackground)[3],by=res(ToyBackground)[1],len=nrow(ToyBackground)-1)
    dat1$z=Abundances[,,i]
    r=raster(dat1)
    plot(r)
    title(main = "Abundance", font.main = 6)
    lines(AdminLines)
   }
    if(SDM){
      # Habitat suitability
      dat1=list()
      dat1$x=seq(extent(ToyBackground)[1],by=res(ToyBackground)[1],len=ncol(ToyBackground))
      dat1$y=seq(extent(ToyBackground)[3],by=res(ToyBackground)[1],len=nrow(ToyBackground))
      dat1$z=Habitats[,,i]
      r=raster(dat1)
      plot(r)
      title(main = "Habitat suitability", font.main = 6)
      #Abundance distributions:
      dat1=list()
      dat1$x=seq(extent(ToyBackground)[1],by=res(ToyBackground)[1],len=ncol(ToyBackground))
      dat1$y=seq(extent(ToyBackground)[3],by=res(ToyBackground)[1],len=nrow(ToyBackground))
      dat1$z=Abundances[,,i]
      r=raster(dat1)
      plot(r)
      title(main = "Abundance", font.main = 6)
    }
  }
  
  if(LandscapeName=="Theoretical") {
    image(Habitats[,,i])
    title(main = "Habitat suitability", font.main = 6)
    image(Abundances[,,i])
    title(main = "Abundance", font.main = 6)
  }
  plot(TotalAbundance[1:i], type="b", col="blue", main="Total Abundance",xlab="Time step", ylab="",las=1, bty="l")
  if(saveVisualization) dev.off()
   #Sys.sleep(0.2) 
}


  