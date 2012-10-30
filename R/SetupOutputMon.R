SetupOutputMon<-function(data, directory, a.data, o.data, cost.period, all.costs, 
                         spp.keep, overlap.indicator, strip.projects, ...){
  
  # This function reads in output monitoring data, creates IDs to match those in
  # o.data to allow cost sharing between output monitoring actions but not between
  # output monitoring and management actions. Cost period matricies and action data
  # are amended to include output monitoring data.
  
  cat('Connecting to database and reading...','\n')
  dbconn<-odbcConnectAccess(data)
  
  # output monitoring data
  cat('...output monitoring data','\n')
  om.data<-sqlFetch(dbconn,"R_OM_actions")
  for(n in c(1,2,4,6,18:20)){
    om.data[,n]<-as.integer(om.data[,n])
  }
  for(n in c(3,5,7:11,13,15,17)){
    om.data[,n]<-as.character(om.data[,n])
  }
  for(n in c(14,16)){
    om.data[,n]<-as.numeric(om.data[,n])
  }
  om.names <-  c("action_id", "species_id",
                 "taxa_code_text", "taxa_code", "spp_text", "spp_code",
                 "method", "method1_text", "method2_text", "method3_text",
                 "method4_text", "period1_cost", "period1_sequence",
                 "period2_cost", "period2_sequence", 
                 "period3_cost", "period3_sequence",  "pr_t_data",
                 "pr_o_data", "pr_m_data")
  names(om.data)<-om.names
  close(dbconn)
  
  # test for any blank values as per action data check in DataChecksIII
  checks<-0
  checks<- any(is.na(om.data[,c(1:11,18:20)]))
  
  if(checks==1){
    cat("Output monitoing data contains NA values")
    return(stop("Code stopped due to NA values"))
  }
  
  cat('Setting up output monitoring data','\n')
  
  NA.index<-with(om.data,is.na(period1_cost))
  om.data$period1_cost[NA.index]<-0
  NA.index<-with(om.data,is.na(period2_cost))
  om.data$period2_cost[NA.index]<-0
  NA.index<-with(om.data,is.na(period3_cost))
  om.data$period3_cost[NA.index]<-0
  
  #strip projects to match species in spp.keep
  if(strip.projects==TRUE){
    om.index<-unlist(lapply(spp.keep, function(x) with(om.data, which(species_id==x))))
    om.data<-om.data[om.index,]
  }
  
  # spread cost years out across 50 columns for each of the cost periods
  counter<-0
  for(i in c(13,15,17)){
    spread<-strsplit(om.data[,i],"\\,")
    sizes<-sapply(spread,length)
    mat<-matrix(rep(0,50*length(spread)),nrow=length(spread),ncol=50)
    for(ii in 1:length(spread)){ # for each object in spread1 list
      if(sizes[ii]==0){
        mat[ii,1:50]<-rep(0,50)
      }
      else{mat[ii,1:sizes[ii]]<-spread[[ii]]
      }
    }
    mode(mat)<-"integer"
    # check for invalid year values
    if(any(mat>50, na.rm=TRUE)){
      return(stop("Code stopped due to years >50 in R_OM_actions",'\n'))
    }
    
    # create logical matrix and populate with TRUE for years that have costs. This eliminates the need for CleanCosts function in Prioritisation.
    logical<-matrix(rep(FALSE,50*nrow(om.data)),nrow=nrow(om.data),ncol=50)
    for(i in 1:nrow(om.data)){ # for each row in mat
      
      years<-mat[i,mat[i,]>0]# if the value in mat is greater than 0, then there is a cost for this year, so put a 
      
      logical[i,years]<-TRUE
    }
    counter<-counter+1
    if(counter==1){
      om.cost.period1<-logical
      #colnames(om.cost.period1)<-period1_text
    }
    if(counter==2){
      om.cost.period2<-logical
      #colnames(om.cost.period2)<-period2_text
    }
    if(counter==3){
      om.cost.period3<-logical
      #colnames(om.cost.period3)<-period3_text
    }
  }
  om.cost.period<-om.cost.period1==TRUE|om.cost.period2==TRUE|om.cost.period3==TRUE
  period_text <- paste("Year", 1:50, sep = "")
  
  om.cost.1<-om.data$period1_cost*om.cost.period
  om.cost.2<-om.data$period2_cost*om.cost.period
  om.cost.3<-om.data$period3_cost*om.cost.period
  om.all.costs<-om.cost.1+om.cost.2+om.cost.3
  
  ####### this section has been implemented as Richard Maloney wishes, however a more elegant/suitable solution would be to
  ####### give OM actions their own IDs in the database, and create a table relationship betweem them and actionIDs in the database
  
  
  all.costs<-rbind(all.costs,om.all.costs)
  
  # copy o.data and add 5000000 to IDs and overlap codes to create new IDs
  o.data.new<-o.data+5000000
  o.data<-rbind(o.data, o.data.new)
  
  # add 5000000 to om.data action IDs and add to a.data
  action_id<-om.data$action_id+5000000
  om.data.new<-cbind(action_id,om.data[,2:20])
  a.data<-rbind(a.data,om.data.new)
  
  # copy om cost periods underneath existing cost period matricies
  cost.period<-rbind(cost.period,om.cost.period)
  
  OutputMon<-list()
  OutputMon$a<-a.data
  OutputMon$o<-o.data
  OutputMon$p<-cost.period
  OutputMon$c<-all.costs
  
  return(OutputMon)
  
}