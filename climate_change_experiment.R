rm(list=ls())
setwd("/")#######################################################################################################
source("/Functions for toy range dynamic model.R")
source("/Toy range dynamic model.R")
Path=("") # folder wher results canbe stored

speciesPool <- read.csv(file="/speciesPool.csv")###################################################################

modelVector <- c('Beverton')
TempCangeYear<-c(0.00, 0.02, 0.04, 0.06)
scenarioVectorNames<-c('Scenario1: 0,00', 'Scenario2: 0,02', 'Scenario3: 0,04', 'Scenario4: 0,06')
landscapeVector <- c(1:20)
timestepsVector <- c(1:120)

#matrix experiments
ClimateChangeExperiment <-  as.data.frame(matrix(0, ncol=12 , nrow=length(modelVector)*length(TempCangeYear)*nrow(speciesPool)*length(landscapeVector)*length(timestepsVector)))
colnames(ClimateChangeExperiment) <- c("Model","Species ID","Mass","sdlog Mass","Temperature","sd Temp","MeanDispersal","ProbDisp","Climate Change Scenario","Landscape","Timesteps","Abundance")
#matrix extinction
ClimateChangeExtinction <-  as.data.frame(matrix(0, ncol=11 ,  nrow=length(modelVector)*length(TempCangeYear)*nrow(speciesPool)))
colnames(ClimateChangeExtinction) <- c("Model","Species ID","Mass","sdlog Mass","Temperature","sd Temp","MeanDispersal","ProbDisp","Climate Change Scenario","Extinctionrisk", "Mean TimestepsTillExtinction")

for (modelIndex in 1:length(modelVector)) {
  for (speciesPoolIndex in 1:nrow(speciesPool)) {
    for (scenarioIndex in 1:length(TempCangeYear)){
      
      row_extinction <- nrow(speciesPool)*length(TempCangeYear)*(modelIndex-1) +
        length(TempCangeYear)*(speciesPoolIndex-1)+
          scenarioIndex
      
      sum_counter <- 0
      meanTimeTillExtinction <- 0
      
      for(landscapeIndex in 1:length(landscapeVector)){
        
        print('EcoSimulation running')
        result <- EcoSimulation(InputParameter = "/Test_1.csv",#######################################################
                                StoragePath = Path,
                                Experiment = T,
                                Rep_modelName_Experiment = modelVector[modelIndex],
                                Mass_Experiment = speciesPool[speciesPoolIndex, 2], 
                                sdlog_M_Experiment = speciesPool[speciesPoolIndex, 3],
                                optimumTemperature_Experiment = speciesPool[speciesPoolIndex, 4], 
                                toleranceTemperature_Experiment = speciesPool[speciesPoolIndex, 5],
                                MeanDispersalDistance_Experiment = speciesPool[speciesPoolIndex, 6],
                                ProbDisp_Experiment = speciesPool[speciesPoolIndex, 7],
                                climate_change_Experiment = TempCangeYear[scenarioIndex],
                                landscapeIndex_Experiment = landscapeIndex,
                                SimulationTimesteps_Experiment = length(timestepsVector)
        )
        print('EcoSimulation done')
        
        timesTillExtinction <- 0
        
        for(time in 1:length(timestepsVector)) {
          row_experiment <- nrow(speciesPool)*length(TempCangeYear)*length(landscapeVector)*length(timestepsVector)*(modelIndex-1) +
            length(TempCangeYear)*length(landscapeVector)*length(timestepsVector)*(speciesPoolIndex-1) +
            length(landscapeVector)*length(timestepsVector)*(scenarioIndex-1) +
            length(timestepsVector)*(landscapeIndex-1) +
            time
          
          ClimateChangeExperiment[row_experiment,1] <-modelVector[modelIndex]
          ClimateChangeExperiment[row_experiment,2] <-speciesPool[speciesPoolIndex, 1]
          ClimateChangeExperiment[row_experiment,3] <-speciesPool[speciesPoolIndex, 2]
          ClimateChangeExperiment[row_experiment,4] <-speciesPool[speciesPoolIndex, 3]
          ClimateChangeExperiment[row_experiment,5] <-speciesPool[speciesPoolIndex, 4]
          ClimateChangeExperiment[row_experiment,6] <-speciesPool[speciesPoolIndex, 5]
          ClimateChangeExperiment[row_experiment,7] <-speciesPool[speciesPoolIndex, 6]
          ClimateChangeExperiment[row_experiment,8] <-speciesPool[speciesPoolIndex, 7]
          ClimateChangeExperiment[row_experiment,9] <-scenarioVectorNames[scenarioIndex]
          ClimateChangeExperiment[row_experiment,10]<-landscapeIndex
          ClimateChangeExperiment[row_experiment,11]<-time
          ClimateChangeExperiment[row_experiment,12]<-result$sim_data$TotalAbundance[time]
          
          if (result$sim_data$TotalAbundance[time] == 0 && timesTillExtinction == 0){
            timesTillExtinction <- time
          }
          
        }
        
        meanTimeTillExtinction <- meanTimeTillExtinction + timesTillExtinction
        
        print(ClimateChangeExperiment[row_experiment,])
        
        if(ClimateChangeExperiment[row_experiment,12] == 0){
          sum_counter <- sum_counter + 1
        }
      }
      
      ClimateChangeExtinction[row_extinction,1] <- modelVector[modelIndex]
      ClimateChangeExtinction[row_extinction,2] <- speciesPool[speciesPoolIndex, 1]
      ClimateChangeExtinction[row_extinction,3] <- speciesPool[speciesPoolIndex, 2]
      ClimateChangeExtinction[row_extinction,4] <- speciesPool[speciesPoolIndex, 3]
      ClimateChangeExtinction[row_extinction,5] <- speciesPool[speciesPoolIndex, 4]
      ClimateChangeExtinction[row_extinction,6] <- speciesPool[speciesPoolIndex, 5]
      ClimateChangeExtinction[row_extinction,7] <- speciesPool[speciesPoolIndex, 6]
      ClimateChangeExtinction[row_extinction,8] <- speciesPool[speciesPoolIndex, 7]
      ClimateChangeExtinction[row_extinction,9] <- scenarioVectorNames[scenarioIndex]
      ClimateChangeExtinction[row_extinction,10] <- sum_counter / length(landscapeVector)
      ClimateChangeExtinction[row_extinction,11] <- meanTimeTillExtinction / length(landscapeVector)
      
      print(ClimateChangeExtinction[row_extinction,])
    }
  }
}


#save
write.csv(ClimateChangeExperiment,"/ClimateChangeExperiment.csv")#############################################################
write.csv(ClimateChangeExtinction,"/ClimateChangeExtinction.csv")

