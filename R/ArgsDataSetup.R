## Setting arguments for DataSetup function ##

#initialize workspace
rm(list=ls())

PriorRun <- function(directory = getwd(), functions.dir, data, 
    poly.size = 1, poly.percent = 0.5,
    strip.projects = TRUE, select.projects = TRUE, project.list = c(14083,14003,13152,13074,2050,2032,2024), projects = 20,
    select.taxa = FALSE, taxa = "Vascular Plant",
    set.benefit = FALSE, new.benefit = 100, set.success = FALSE, new.success = 100, 
    overlap.indicator = TRUE, 
    output.monitoring = TRUE, 
    run.prioritisation = TRUE, 
    set_budget = 1919263, 
    multiplier = 1,
    weighting = 1,
    discount.rate = 0.01,
    T = 50,
    stop.iteration = Inf, 
    summary = TRUE, 
    prefix_text = as.character(Sys.Date())
    ){
        
        # Check the RODBC package is installed
        if("RODBC" %in% rownames(installed.packages()) == FALSE) {
            install.packages("RODBC", dependencies = TRUE)
            }
        require(RODBC)
    }

# directories
directory<-getwd() # directory for all input and output files
output.dir <- "D:/PRIORITYDATABASE/PrioritisationII_RUNS/OutputData"
functions.dir <- path.expand("~/PPP/R") # directory all functions are stored in
data<-"TESTMASTER_SPECIES_PRESCRIPTIONS_30May_2012.mdb" # name of the database

# polygon thresholds
#poly.size <- 1 # threshold for polgon slither deletion (hectares)
#poly.percent <- 0.5 # threshold for polygon slither deletion (proportion of original size)

# strip projects settings
#strip.projects <- TRUE # set to TRUE if you wish to limit the number of projects prioritised by either number and/or taxa type
#select.projects <- TRUE # set to true to limit projects to only those in project.list
#project.list<-as.integer(unlist(read.csv("ID.csv",header=FALSE))) # list of species IDs to select
#project.list <- c(14083,14003,13152,13074,2050,2032,2024)
#projects <- 20 # the number of projects prioritised if strip.projects=TRUE (from the order projects are listed in R_benefits)
select.taxa <- FALSE # set to TRUE to enable selection of one taxa type only for prioritisation
taxa <- "Vascular Plant" # if select.taxa is TRUE, then set to taxa text
set.benefit <- FALSE # set to TRUE to set benefit 
new.benefit <- 100 # if set.benefit is TRUE then set to 0-100 to change benefit values
set.success <- FALSE # set to TRUE to set benefit
new.success <- 100 # if set.success is TRUE then set to 0-100 to change success values

# overlaps and output monitoring
overlap.indicator <- TRUE # set to TRUE if you want overlaps to be calculated
output.monitoring <- TRUE # set to TRUE to include output monitoring data

# prioritisation settings
run.prioritisation <- TRUE # TRUE - prioritisation will run immediately, FALSE - csv files will be written to directory
set_budget <- 1919263 # budget limit for prioritisation
multiplier <- 1
weighting <- 1
discount.rate <- 0.01
T <- 50
stop.iteration <- Inf
input.w.data <- FALSE

# summary info
summary <- TRUE # when set to TRUE the function summary.PPP will be run after Priortisation
prefix_text <- as.character(Sys.Date()) # if set to Sys.Date() the prefix to each summary file will be today's date

# sourcing setup functions
source(file.path(functions.dir,'DataSetup.R'))
source(file.path(functions.dir,"ConnectToGISdata.R"))
source(file.path(functions.dir,'ConnectToActionBenefitWeights.R'))
source(file.path(functions.dir,'SetupWeights.R'))
source(file.path(functions.dir,'DataChecksIII.R'))
source(file.path(functions.dir,'NewSetupCosts.R'))
source(file.path(functions.dir,'SetupOverlaps.R'))
source(file.path(functions.dir,'SetupOutputMon.R'))

# sourcing functions from PPP_v0.5
source(file.path(functions.dir,'NewCalcCosts.R'))
source(file.path(functions.dir,'CalculateWeightsII.R'))
source(file.path(functions.dir,'MakeOverlaps.R'))
source(file.path(functions.dir,'SetupWeights.R'))
source(file.path(functions.dir,'UpdateWeightsII.R'))
source(file.path(functions.dir,'plot.PPP.R'))
source(file.path(functions.dir,'ppp-internal.R'))
source(file.path(functions.dir,'print.PPP.R'))
source(file.path(functions.dir,'print.summary.PPP.R'))
source(file.path(functions.dir,'summary.PPP.R'))

# sourcing modified Prioritisation function (based on PPP_v0.5)
source(file.path(functions.dir,'PrioritisationII.R'))

# calling DataSetup function with arguments set as above
DataSetup(directory, data, 
          poly.size, poly.percent, 
          strip.projects, projects, 
          select.taxa, taxa, 
          set.benefit, new.benefit, 
          set.success, new.success, 
          overlap.indicator, output.monitoring, 
          run.prioritisation, set_budget, 
          summary, output.dir,
          prefix_text, 
          multiplier, weighting, 
          discount.rate, T, 
          stop.iteration, select.projects, 
          project.list
          input.w.data)



