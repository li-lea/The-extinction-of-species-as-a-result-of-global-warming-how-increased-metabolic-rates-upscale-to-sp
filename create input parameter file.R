####Test1
#setwd("C:/Users/tu_install/Documents/Master/mecha.range/data/Input")
setwd("/Users/robert.urban/Desktop/two")############################################################################################################

  InputParameter<-data.frame(  
    stringsAsFactors = F,
    HabitatType="MetabolicRange", #"Random" or "MetabolicRange" 
    LandscapeName="Theoretical",# Toydata or "Bavaria" or "Alps" or "Theoretical"    SDM can be enabled in functions
    #if Theoretical, what should be the extent?
    Ylength=20,
    Xlength=20,
    # how many years should be simulated?
    SimulationTimesteps=100,
    # only important for HabitatType="Random"
    ProbHabitat=0.25,
    # Only important for HabitatType="MetabolicRange"
    optimumTemperature= 298.15,  #optimum temperature is 2°C for higher temperatures the population is dying..
    toleranceTemperature=20,  
    CutoffSuitability=0.7, # minimal Habitat Suitability for inital Survival
    DispersalSimulation=TRUE,
    DistMax=50,
    DispersalType="Kernel", # "Individual" or "Kernel"
    MeanDispersalDistance=0.5, #for kernel dispersal (in grid cell)
    ProbDisp=0.01, # for individual dispersal
    InvasionSwitch=FALSE,
    UseCataMortality=FALSE,
    cataprob=0.9,#Rate for catastrophic mortality when UseCataMortality=T
    sd_cata_mortality=0.01,
    Xstartmin=1,
    Xstartmax=20,#Xlength
    Ystartmin=1,
    Ystartmax=1,
    metabolic= T, #including MTE or without MTE
    Mass=0.082,#Bodymass of bees
    MassForCalibration=0.05,
    E = 0.65, #activation energy
    k = 8.6*10^-5, #boltzmann constant
    temperature=288.15, #temperature
    temperatureForCalibration=288.15,
    Rep_modelName="Beverton", #'Ricker' or 'RickerAllee' or 'Beverton'
    R=1.1, #growth rate
    carryingcapacity=100, #carryingcapacity
    m=0.01, #Mortality rate
    alpha=20,#shape1 for betadeviation in the mortality of the Beverton equation 
    C=-100, #Allee threshold/-carryingcapacity
    #minMass= 0.072, #minimum mass of species for parameter calculation
    #maxMass=0.092,#maximum mass of species for parameter calculation
    #mintemperature=273.15,#minimum temperature for parameter calculation
    #maxtemperature=303.15,#maximum temperature for parameter calculation
    Exponent_R=-1/4, #Exponent_R MTE
    Exponent_m=-1/4, #Exponent_m MTE
    Exponent_K=-3/4, #Exponent_K MTE
    Exponent_C=-3/4, #Exponent_C MTE
    initialstoch_R=T,#initial growth rate stochastic or not
    initialstoch_K=T,#initial carryingcapacity stochastic or not
    initialstoch_m=T,#initial mortality rate stochastic or not
    initialstoch_C=F,#initial Allee threshold stochastic or not
    stoch_N=T, #Number of Individuals stochastic or not
    stoch_R=T,#growth rate stochastic or not
    stoch_K=T,#carryingcapacity stochastic or not
    stoch_m=T,#mortality rate stochastic or not
    stoch_C=F,#Allee threshold stochastic or not
    sdlog_R=0.01, #standard deviation growth rate
    sdlog_K=0.01, #standard deviation carryingcapacity
    sd_C=0.1, #standard deviation Allee threshold
    sdlog_M=0.001,#standard deviation bodymass
    sd_T=1.5, ##standard deviation temperature
    Visualize=TRUE, ##plot results
    saveVisualization=F, #plots will not be shown then
    width = 1200,
    height = 400,
    unit= "px",
    saveOutput=T,
    N=20,
    H=0.7,
    p=0.2,
    landscapeIndex=1,
    temperature_scaling=10,
    StartTempChange=20,
    StopTempChange=120,
    climate_change=0.1)

write.csv(InputParameter,file = "Test_1.csv",row.names = F)
#InputParameter<-read.csv("InputParameter.csv")
#Test1<-read.csv("/Users/robert.urban/Desktop/two/InputParameter.csv")

###create species pool
minMass= 0.01 #minimum mass of species for parameter calculation
maxMass=0.1#maximum mass of species for parameter calculation
sdlog_Mmin=0
sdlog_Mmax=0.002
mintemperature=273.15#minimum temperature for parameter calculation
maxtemperature=303.15#maximum temperature for parameter calculation
mintoleranceTemp=1
maxtoleranceTemp=20
MinDispersalDistance=0.01 #for kernel dispersal (in grid cell)
MaxDispersalDistance=0.5 #for kernel dispersal (in grid cell)
sdDispersalDistanceMin=0.01
sdDispersalDistanceMax=0.01
speciesPool <- data.frame("Mass"=c(runif(20,min=minMass,max=maxMass)),
                          "sdlog_M"=c(runif(20,min=sdlog_Mmin,max=sdlog_Mmax)),
                          "Temp"=c(runif(20,min=mintemperature,max=maxtemperature)),
                          "Temptolerance"=c(runif(20,min=mintoleranceTemp,max=maxtoleranceTemp)),
                          "MeanDispersalDIstance"=c(runif(20,min=MinDispersalDistance,max=MaxDispersalDistance)),
                          "sdDispersalDistance"=c(runif(20,min=sdDispersalDistanceMin,max=sdDispersalDistanceMax)))
speciesPool
write.csv(speciesPool,file = "/Users/robert.urban/Desktop/two/speciesPool.csv",row.names = T)###################################################################


#new landscapes in file landscape
#create new species pool
#read input parameter 

