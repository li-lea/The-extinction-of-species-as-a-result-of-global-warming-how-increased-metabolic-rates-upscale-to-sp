#####  Simulation function ######

EcoSimulation<-function(
    InputParameter,
    StoragePath,
    Experiment = F,
    Rep_modelName_Experiment,
    Mass_Experiment,
    sdlog_M_Experiment,
    optimumTemperature_Experiment,
    toleranceTemperature_Experiment,
    MeanDispersalDistance_Experiment,
    ProbDisp_Experiment,
    climate_change_Experiment,
    landscapeIndex_Experiment,
    SimulationTimesteps_Experiment
  ){
  setwd("/")
  RunName<- gsub(".*/|\\..*","",InputParameter) #define name for plot output
  InputParameter<-read.csv(InputParameter)
  with(InputParameter,{# load Input Parameter in environment
   #überschreiben mit Parametern aus climate change experiment 
    #Mass=Mass_exp
    #sdlog_M=sdlog_M_exp
    #temperature=temperature_experiment
    #...
  if (Experiment) {
    Rep_modelName=Rep_modelName_Experiment 
    Mass=Mass_Experiment
    sdlog_M=sdlog_M_Experiment
    optimumTemperature=optimumTemperature_Experiment
    toleranceTemperature=toleranceTemperature_Experiment
    MeanDispersalDistance=MeanDispersalDistance_Experiment
    ProbDisp=ProbDisp_Experiment
    climate_change=climate_change_Experiment
    landsacpeIndex=landscapeIndex_Experiment
    SimulationTimesteps=SimulationTimesteps_Experiment
  }
   
  # defining the progress bar & the random seed for reproducibility
  Timesteps=SimulationTimesteps+1
  pb <- txtProgressBar(min = 1, max = SimulationTimesteps, style = 3)
  start_time <- Sys.time()
  randomseed<- as.numeric(Sys.time())
  set.seed(randomseed)
  
  #create wd to save results to if it does not already exist
  if(saveVisualization || saveOutput){
    if(!dir.exists(paste(StoragePath,RunName,sep = "/"))){dir.create(paste(StoragePath,RunName,sep = "/"))}
  }
######## Initialization ######
  Landscape<-BuildLandscape(LandscapeName=LandscapeName)


  sim_data<-Initialization(            Ylength=Ylength,
                                       Xlength=Xlength,
                                       Timesteps=Timesteps,
                                       HabitatType=HabitatType,
                                       ProbHabitat=ProbHabitat,
                                       DistMax=DistMax,
                                       MeanDispersalDistance=MeanDispersalDistance,
                                       InvasionSwitch=InvasionSwitch,
                                       Xstartmin=Xstartmin,
                                       Xstartmax=Xstartmax,
                                       Ystartmin=Ystartmin,
                                       Ystartmax=Ystartmax,
                                       Landscape = Landscape,
                                       LandscapeName = LandscapeName,
                                       optimumTemperature=optimumTemperature,
                                       toleranceTemperature=toleranceTemperature,
                                       CutoffSuitability=CutoffSuitability,
                                       metabolic=metabolic,
                                       Mass=Mass,
                                       MassForCalibration=MassForCalibration,
                                       sdlog_M=sdlog_M,
                                       temperature=temperature,
                                       temperatureForCalibration = temperatureForCalibration,
                                       sd_T=sd_T,
                                       Rep_modelName=Rep_modelName,
                                       R=R,
                                       carryingcapacity=carryingcapacity,
                                       m=m,
                                       C=C,
                                       sdlog_R=sdlog_R,
                                       sdlog_K=sdlog_K,
                                       alpha=alpha,
                                       sd_C=sd_C,
                                       UseCataMortality=UseCataMortality,
                                       cataprob=cataprob,
                                       sd_cata_mortality=sd_cata_mortality,
                                       initialstoch_R=initialstoch_R,
                                       initialstoch_K=initialstoch_K,
                                       initialstoch_m=initialstoch_m,
                                       initialstoch_C=initialstoch_C,
                                       stoch_R=stoch_R,
                                       stoch_K=stoch_K,
                                       stoch_m=stoch_m,
                                       stoch_C=stoch_C,
                                       Exponent_R=Exponent_R,
                                       Exponent_K=Exponent_K,
                                       Exponent_m=Exponent_m,
                                       Exponent_C=Exponent_C,
                                       E=E,
                                       k=k,
                                       N=N,
                                       H=H,
                                       p=p,
                                       landscapeIndex=landscapeIndex,
                                       temperature_scaling=temperature_scaling,
                                       StartTempChange=StartTempChange,
                                       StopTempChange=StopTempChange,
                                       climate_change=climate_change)#TODO: bundle species traits in a vector

  if(LandscapeName %in% c("Toydata","Bavaria","Alps")){
    Ylength<-Landscape$Ylength
    Xlength<-Landscape$Xlength
  }
  if(LandscapeName == "Theoretical" && HabitatType == "MetabolicRange"){
    Ylength<-N
    Xlength<-N
  }
  if(LandscapeName=="Toydata"){
    ToyBackground<-Landscape$ToyBackground
    ToyLandscape=Landscape$ToyLandscape
  }
print ("Initialized") 

###### Ecological loop ######
  for(i in 1:(Timesteps-1)){  #temporal loop
    for (x in 1:Xlength){   #spatial loop x
      for(y in 1:Ylength){   #spatial loop y
        #Population dynamics ######
        if((!is.na(sim_data$Habitat[y,x,i]))&&(sim_data$Habitat[y,x,i]>CutoffSuitability)){
          #TODO: make the role of parameter-modifyer of habitat suitability flexible as simulation configuration
          sim_data$Abundances[y,x,i+1]<- reproduction(Rep_modelName, stoch_N, N=sim_data$Abundances[y,x,i], R= sim_data$R_array[y,x,i], carryingcapacity=sim_data$K_array[y,x,i]*sim_data$Habitat[y,x,i], m=sim_data$m_array[y,x,i], C=sim_data$C_array[y,x,i])   
          #print(c("Time=",i,"|x=",x,"|y=",y,"reproduce"))
        }

        #Dispersal #######
      #  if(DispersalSimulation){
          sim_data$DispersingUnits[,,i]<-Dispersal(N=sim_data$Abundances[y,x,i+1],DistMax=DistMax,ProbDisp=ProbDisp,DispersingUnits=sim_data$DispersingUnits[,,i],x=x,y=y,DispersalType=DispersalType,DispersalKernel=sim_data$DispersalKernel)
        #  print(c("Time=",i,"|x=",x,"|y=",y,"disperse"))
      #    }
          
      }
    }
    ##Dispersal Survival #######
  #  if(DispersalSimulation){
      sim_data$Abundances[,,i+1]<-DispersalSurvival(Abundances=sim_data$Abundances[,,i+1], DispersingUnits=sim_data$DispersingUnits[,,i], Habitat=sim_data$Habitat[,,i],Xlength=Xlength,Ylength=Ylength,DistMax=DistMax, CutoffSuitability=CutoffSuitability,stoch_N=stoch_N)
  #  } 
    
    #Catastrophic Mortality ####
    #(non-metabolic and disturbance-based):
    if(UseCataMortality) sim_data$Abundances[,,i+1]<-CatastrophicMortality(Abundances=sim_data$Abundances[,,i+1],Xlength=Xlength,Ylength=Ylength, MortalityArray=sim_data$CataMortalityArray[,,i+1])
    
    #For  Output:
    sim_data$TotalAbundance[i]<- sum(sim_data$Abundances[,,i])
    print(sim_data$TotalAbundance)
    
    #Vizualization ##### 

    if(Visualize){
      Visualization(Abundances=sim_data$Abundances,TotalAbundance=sim_data$TotalAbundance,carryingcapacity=sim_data$carryingcapacity,i=i,LandscapeName=LandscapeName,Habitats=sim_data$Habitat, ToyBackground=Landscape$ToyBackground, AdminLines= Landscape$AdminLines, SDM=Landscape$SDM, StoragePath=StoragePath, saveVisualization=saveVisualization, RunName=RunName, width=width, height=height, unit=unit)
    }
 
    setTxtProgressBar(pb,i)
  }
  end_time <- Sys.time()
  print(end_time - start_time) 
  
  ## Output ####
  Output<-mget(ls())
  if(saveOutput){
    RunParameter<-as.data.frame(Output[! names(Output) %in% c("Landscape","pb", "sim_data")])
    write.csv(RunParameter,file = paste(StoragePath,"/",RunName,"/",RunName,"_","Parameter",".csv",sep = ""),row.names = F)
    Results<-Output$sim_data
    saveRDS(Results, file = paste(StoragePath,"/",RunName,"/",RunName,"_","Simulation_Data",".rds",sep = ""))
  }
  return(Output)
 })
}


