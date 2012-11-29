SetupOverlaps<-function(GIS.data, a.data, ...){
  
  # This function determines which management actions should share costs.
  # Management actions that should share are given the same ID. This function
  # returns o.data which Prioritisation uses.
  
  cat("Calculating overlaps...",'\n')
  uniqueID=1 #number to start assigning ids which identify overlapping mgmt actions
  
  # Create unique activity code by combining OpActivityID,OpMethodID,OutputTargetID,THreatID and add to overlap data array
  # Only managment action with the same activity code can share costs.
  activity.code<- as.integer(paste(GIS.data[,2], GIS.data[,3], GIS.data[,4], GIS.data[,5], sep=""))
  GIS.data<-cbind(GIS.data,activity.code)
  colnames(GIS.data)[13]<- "ActivityCode"
  
  #create array of non-dulpicated ActivityCode, MgmtActionID, and overlapID
  overlap.codes<- unique(GIS.data[,c(6,9,13)])
  AC<-unique(overlap.codes[,3]) #vector of unique list of ACs
  
  #Clean workspace
  rm(GIS.data)
  
  o.data<-as.data.frame(NULL)
  for(n in AC){
    #AC.data is a subset of just one activity code
    AC.data<-subset(overlap.codes,subset=ActivityCode==n)
    #cat("     ....for activity code",n,'\n')
    
    # 1.counts of overlapID and MgmtActionID
    count.overlap<-as.data.frame(with(AC.data, table(SpatialID)))
    colnames(count.overlap)[2]<-"CountOverlap"
    count.mgmt<-as.data.frame(with(AC.data, table(ManagementActionID)))
    colnames(count.mgmt)[2]<-"CountMgmt"
    AC.counts<-merge(AC.data,count.overlap,by="SpatialID")
    AC.counts<-merge(AC.counts,count.mgmt,by="ManagementActionID")
    colnames(AC.counts)[1]="action_id"
    rm(AC.data, count.overlap,count.mgmt)
    track.id<-c(1:nrow(AC.counts)) # id to enable tracking of which rows have had an unique id assigned
    AC.counts<-cbind(AC.counts,track.id)
    
    # 2a.where both counts=1, assign new uniqueID. These are isolated polygons not overlapping with any others.
    # the majority of these should be removed anyway in SetupGIS where original=new polygons are removed.
    index.2a<-with(AC.counts, which(CountOverlap==1 & CountMgmt==1))
    if(length(index.2a>0)){
      AC.counts.2a<-AC.counts[index.2a,]
      vct.ID<-as.integer(c(uniqueID:(uniqueID+length(index.2a)-1)))
      AC.counts.2a[,7]<-vct.ID
      colnames(AC.counts.2a)[7]<-"overlap_id"
      uniqueID=uniqueID+length(index.2a)  
      output.2a<-unique(AC.counts.2a[,-2:-6],MARGIN=1)
      Remaining.AC<-AC.counts[with(AC.counts, !(CountOverlap==1 & CountMgmt==1)),] #those that don't get uniqueID assigned here
    }else {output.2a<-data.frame(NULL)
           AC.counts.2a<-data.frame(NULL)
           Remaining.AC<-AC.counts
    }
    
    # 2b.where countOverlap=1 and countMgmtAction>1, assign uniqueIDs to all those mgmt sites. This is where one action cuts another into two or more polygons.
    index.2b<-with(AC.counts, which(CountOverlap==1 & CountMgmt>1))
    if(length(index.2b>0)){
      AC.counts.2b<-AC.counts[index.2b,]
      UniqueMgmt<-unique(AC.counts.2b$action_id)
      overlap_id<-c(uniqueID:(uniqueID+length(UniqueMgmt)-1))
      UniqueMgmt<-as.data.frame(cbind(UniqueMgmt,overlap_id))
      AC.counts.2b<-merge(AC.counts.2b,UniqueMgmt,by.x="action_id",by.y="UniqueMgmt")
      #AC.counts.2b
      uniqueID=uniqueID+length(overlap_id)  
      output.2b<-unique(AC.counts.2b[,-2:-6],MARGIN=1)
      Remaining.AC<-Remaining.AC[with(Remaining.AC, !(CountOverlap==1 & CountMgmt>1)),]# those that don't get uniqueID assigned here
    }else{ output.2b<-data.frame(NULL)
           AC.counts.2b<-data.frame(NULL)
    }
    
    # 2c.where countOVerlap>1 and CountMgmtSite=1 assign uniqueID to all those overlaps. These are actions completely within another action.
    # ID gets applied to all occurrences of Overlap, regardless of any counts

    index.2c<-with(AC.counts, which(CountOverlap>1 & CountMgmt==1))
    if(length(index.2c>0)){
      AC.counts.2c<-AC.counts[index.2c,]
      UniqueOverlap<-unique(AC.counts.2c$SpatialID)
      overlap_id<-as.integer(c(uniqueID:(uniqueID+length(UniqueOverlap)-1)))
      UniqueOverlap<-as.data.frame(cbind(UniqueOverlap,overlap_id))
      # merge needs to be with Remaining.AC as id is applied to spatial overlapID regardless of other counts.
      AC.counts.2c<-merge(Remaining.AC,UniqueOverlap,by.x="SpatialID",by.y="UniqueOverlap")
      uniqueID=uniqueID+length(overlap_id)  
      output.2c<-unique(AC.counts.2c[,c(2,7)],MARGIN=1)
      colnames(output.2c)[2]<-"overlap_id"
    }else{ output.2c<-data.frame(NULL)
           AC.counts.2c<-data.frame(NULL)
    }
    
    # Determine AC.counts.3 from only those remaining in AC.counts that haven't already
    # been allocated an ID in 2a,2b or 2c.
    Done.AC<-rbind(AC.counts.2a,AC.counts.2b,AC.counts.2c) #Rows from AC.counts which have OverlapIDs assigned so far
    if(nrow(Done.AC)!=nrow(AC.counts)){
      id<-setdiff(track.id,Done.AC$track.id) # track ids from AC.counts still requiring OverlapIDs
      id<-as.data.frame(id)
      Remaining.AC<-merge(AC.counts,id,by.x="track.id",by.y="id")
    }else{Remaining.AC<-data.frame(NULL)
    }
   
    # 3.for remaining data list all mgmt sites which occur for the overlap code
    if(nrow(Remaining.AC)>0){
      #create list of the overlaps
      AC.counts.3<-Remaining.AC[,-1] # remove track.id from array
      o.find<-AC.counts.3[,2]
      m.find<-AC.counts.3[,1]
      #create array for storing combocells, i.e. all MgmtActionIDs for each polygon piece
      counter=1
      ComboCells<-matrix(NA,nrow=length(o.find),ncol=30)
      # for each overlapID find all mgmt sites and create array which also contains overlap ID and mgmtActionID
      # Combocells has col1=mgmtactionID, col2=SpatialID, cols3:30= mgmtactionIDs for the overlapID.
      for(ii in o.find){
        mgmt<-c(m.find[counter],ii, AC.counts.3[which(AC.counts.3$SpatialID==ii),1])
        ComboCells[counter,1:length(mgmt)]<-mgmt
        counter=counter+1
      }
      
      #Find ComboCell rows which are unique and apply same uniqueID to all rows which are the same.i.e. each unique row gets a new ID.
      if(nrow(ComboCells)==1){#if ComboCells has only 1 unique row then need to assign UniqueID in different way.
        cc.unique<-t(ComboCells[1,3:30])
        id.vector<-uniqueID
        cc.unique<-cbind(cc.unique,id.vector)
      }else{cc.unique<-unique(ComboCells[,3:30],MARGIN=1)
            id.vector<-as.integer(c(uniqueID:(uniqueID+nrow(cc.unique)-1)))
            cc.unique<-cbind(cc.unique,id.vector)
      }
      #cc.unique has 29 cols. cols1:28 are mgmtactionIDs, and col 29 is id.vector
      #ComboCells2 has 31 cols. cols 1:28 as combocells, 29 is mgmtActionID, 30 is overlapID and 31 is ID.
      ComboCells2<-merge(ComboCells,cc.unique,by.x=c(3:30),by.y=c(1:28))
      uniqueID=uniqueID+length(id.vector)
      output.3<-data.frame(action_id=ComboCells2[,29], overlap_id=ComboCells2[,31])
      output.3<-unique(output.3,MARGIN=1)
    }else{ output.3<-data.frame(NULL)
    }
    #combine outputs 2a,2b,2c and 3
    final.output<-rbind(output.2a,output.2b,output.2c,output.3)
    rm(output.2a,output.2b,output.2c,output.3)
    o.data.new<-unique(final.output,MARGIN=1) 
    o.data<-rbind(o.data,o.data.new)
  }
  
  #remove any rows with an overlapID that occurs only once, as these don't overlap
  counts<-as.matrix(table(o.data$overlap_id))
  to.keep<-as.data.frame(which(counts>1))
  if(dim(to.keep)[1]>0){ #if there are overlaps
    colnames(to.keep)<-"overlap_id"
    o.data<-merge(to.keep,o.data)
    o.data<-data.frame(action_id=o.data[,2],overlap_id=o.data[,1])
  }
  
  ########################
  #Datachecks on o.data
  ########################
  # Test if actionIDs in overlaps data are a subset (or equal to) of those in a.data
  checks <- 0
  logical <- with(o.data, action_id) %in% with(a.data, action_id)
  checks[1] <- !all(logical)
  # if action IDs are found in o.data that don't exists in a.data, write to CSV
  if(checks[1]==1){
    missingIDs.in.adata <- setdiff(with(a.data, action_id), with(o.data, action_id))
    write.csv(missingIDs.in.adata, file="MissingActionIDinActionData.csv", row.names=FALSE)
    cat('Action IDs are found in overlap data that do not exist in action data - ', '\n')
    cat('## Missing IDs have been written to', directory,'##\n')
    return(stop('Code stopped due to errors between action data and overlap data'))
  }
  cat("Finished overlap calculations",'\n')
  return(o.data)
}