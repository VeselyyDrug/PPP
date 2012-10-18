DataChecksIII <- function(a.data, b.data, w.data, directory, for.overlaps, poly.data, ...){
  
  # This function performs a variety of data checks. The code will stop
  # if any of these tests fail. When some tests fail an output is written
  # to directory to help debugging
  
  #Perform checks for NAs in data
  checks<- as.integer(rep(0,5))
  checks[1]<- any(is.na(a.data[,c(1:11,18:20)]))
  checks[2]<-any(is.na(b.data[,c(1:3,5:8)]))
  checks[3]<-any(is.na(w.data[,-4])) #check all columns except ID as non-threatend or non-included spp don't get IDs in w.data
  checks[4]<-any(is.na(for.overlaps))
  checks[5]<-any(is.na(poly.data[,c(1,2,3,5)]))
  
  if(sum(checks)>0){
    if(checks[1]==1)
      cat("Action data contains NA values")
    if(checks[2]==1)
      cat("Benefit data contains NA values")
    if(checks[3]==1)
      cat("Weight data contains NA values")
    if(checks[4]==1)
      cat("ForOverlaps data contains NA values")
    if(checks[5]==1)
      cat("Poly.data contains NA values")
    return(stop("Code stopped due to NA values"))
  }
  
  # Perform  checks on the data (check all are zero/FALSE)
  checks <- rep(0, 7)
  
  # The number of threatened species in genera should be less than
  # or equal to the number of species in the genera
  checks[1] <- with(w.data, sum(Bs < As))
  if(checks[1]==1){
    cat('There are fewer species in genera than threatened species in genera', '\n')
    return(stop("Code stopped due to error in R_weights input data"))
  }
  # Ditto above but with genera in family
  checks[2] <- with(w.data, sum(Bg < Ag))
  if(checks[2]==1){
    cat('There are fewer genera in family than threatened genera in family', '\n')
    return(stop("Code stopped due to error in R_weights input data"))
  }
  
  # Ditto above but with families in order
  checks[3] <- with(w.data, sum(Bf < Af))
  if(checks[3]==1){
    cat('There are fewer family in order than threatened family in order', '\n')
    return(stop("Code stopped due to error in R_weights input data"))
  }
  
  # There should not be duplicated action IDs in the actions dataset
  checks[4] <- with(a.data, sum(duplicated(action_id)))
  if(checks[4]==1){
    cat('There are duplicated action IDs in R_actions', '\n')
    return(stop("Code stopped due to error in R_actions input data"))
  }
  
  # test for same number of management action ids in for.overlaps and a.data
  checks[5] <- length(for.overlaps$ManagementActionID)!=length(a.data$action_id)
  if(checks[5]==1){
    cat('Action IDs in R_DataForOverlaps and R_actions do not match', '\n')
    return(stop("Code stopped due to error in input data"))
  }
  
  # are all species IDs in b.data found in w.data?
  thr.spp.in.b.data <- with(b.data, unique(species_id))
  thr.spp.in.w.data <- subset(w.data, subset = Ts.old == 1, select = species_id)
  checks[6] <- !(nrow(thr.spp.in.w.data) >= length(thr.spp.in.b.data))
  # write any species IDs missing from w.data to csv
  if(checks[6]==1){
    filetowrite<-setdiff(thr.spp.in.w.data, thr.spp.in.b.data)
    write.csv(filetowrite,file="SppIDsMissingInBenefitsData.csv,row.names=FALSE")
    cat('Some SpeciesIDs in R_benefits are missing from R_weights', '\n')
    cat('##Missing IDs have been written to', directory,'##\n')
    return(stop("Code stopped due to error in input data"))
  }
  
  # Check the number of thr spp in b.data with a.data
  thr.spp.in.a.data <- unique(with(a.data, species_id))
  checks[7] <- !(length(thr.spp.in.b.data) == length(thr.spp.in.a.data))
  if(checks[7]==1){
    filetowrite1<-setdiff(thr.spp.in.b.data, thr.spp.in.a.data)
    write.csv(filetowrite1,file="SppIDsMissingFromActionData.csv", row.names=FALSE)
    filetowrite2<-setdiff(thr.spp.in.a.data, thr.spp.in.b.data)
    write.csv(filetowrite2,file="SppIDsMissingFromBenefitData.csv", row.names=FALSE)
    cat('Species IDs DO NOT match between action dataset and benefits dataset - ', '\n')
    cat('##Missing IDs have been written to', directory,'##\n')
    return(stop("Code stopped due to error in input data"))
    }
  
  # Further checks.
  if(sum(w.data[,c("Bs", "Bg", "Bf", "As", "Ag", "Af")]<0, na.rm=T)){
    cat('Error >>>>>>>> Some taxonomic counts (Bs, Bg, etc.) are less than zero.')
    return(stop("Code stopped due to errors in weights data"))
  }
  cat('Data checks completed','\n')
  return(invisible())
}