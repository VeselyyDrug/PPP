ConnectToGISdata<-function(poly.size, poly.percent, data,...){
  
  # This function reads in the 2 outputs from the SuperRegionPoly analysis
  # and undertakes data cleaning to reduce data volume for calculating
  # overlaps. It deletes records for any slithers below size resolution set in the 
  # GIS, deletes records for non-overlapping polygons, and deletes polygons less
  # than size thresholds set by poly.size and poly.percent. Finally it joins the
  # cleaned GIS outputs to data on management actions from the database and produces
  # dataframes that are used in SetupOverlaps to determine which management actions
  # spatially overlap.
  
#   new.hectares<-read.dbf(paste(directory,"/",data[1], sep=""))
#   colnames(new.hectares)[1]<-"SpatialID"
#   super.poly<- read.csv(paste(directory,"/",data[2], sep=""))
  
  #Read in data from GIS outputs
  cat("Connecting to database",'\n')
  dbconn<-odbcConnectAccess(data)
  cat("Reading GIS data",'\n')
  # read in ForOverlaps qry
  for.overlaps<-sqlFetch(dbconn,"R_DataForOverlaps")
  # set ID columns to integers
  tointeger <- c("ActivityID", "MethodID", "ThreatID", "ManagementActionID", "ManagementSiteID", "SpeciesID")
  for(n in tointeger){
    for.overlaps[,n]<-as.integer(for.overlaps[,n])
  }
  # read in SRP file
  new.hectares<-sqlFetch(dbconn, "R_cutpolygonha")
  colnames(new.hectares)[1]<-"SpatialID"
  super.poly<-sqlFetch(dbconn,"R_superregionpolyoutput")
  #Close database connection
  close(dbconn)
  
  cat("Setting up GIS data",'\n')
  
  # are the number of MgmtSiteIDs from R_superregionpolyoutput the same as number of
  # MgmtSiteIDs in R_DataForOverlaps
  set.flag=FALSE
  if(length(unique(for.overlaps$ManagementSiteID))!=length(unique(super.poly$MANAGEMENT))){
    set.flag=TRUE
  }
  
  #Delete any NAs in MgmtSiteIDs in for.overlaps as these don't yet have polygons drawn
  for.overlaps<-for.overlaps[with(for.overlaps, !is.na(ManagementSiteID)),]
  
  #Join and create polygon data for use in overlap calculations
  #poly.data<-merge(new.hectares,super.poly,by.x = "SpatialID",by.y="SPATIALID")
  poly.data<-merge(new.hectares,super.poly,by = "SpatialID")
  colnames(poly.data)[2]<-"NewHA"
  colnames(poly.data)[8]<-"OriginalHA"
  
  #Delete any NA hectares as these are smaller than the precision setting in SRP parameters
  poly.data<-poly.data[with(poly.data,!is.na(OriginalHA)),]
  
  #Delete polys that didn't get cut by superregionpoly as they don't overlap with anything.
  #poly.data<-poly.data[with(poly.data, !OriginalHA==NewHA),]
  
  #Remove any polys that are less than poly.size AND less than poly.percent of the original size
  remove.index<-with(poly.data, NewHA<poly.size&((NewHA/OriginalHA<poly.percent)))
  poly.data<-poly.data[!remove.index,c(1,5,6,7,8)]
  colnames(poly.data)[2]<-"ManagementSiteID"
  
  #Join together spatial data outputs (poly.data) and info from database to enable determination of which actions overlap
  GIS.data<-merge(for.overlaps,poly.data,by="ManagementSiteID")
  GIS.data<-as.data.frame(GIS.data)
  
  GIS<-list()
  GIS$data<-GIS.data
  GIS$overlaps<-for.overlaps
  GIS$poly<-poly.data
  GIS$flag<-set.flag
  return(GIS)
}
  