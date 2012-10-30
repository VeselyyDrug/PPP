NewSetupCosts<-function(a.data,...){
  
  ## Generate logical cost.period matricies for use in Prioritisation()##
  ## CleanCosts function in Prioritisation is no longer needed.
  
  cat('Generating cost period matricies','\n')
  
  #set up names for cost period matricies
  period1_text <- paste("p1_year", 1:50, sep = "")
  period2_text <- paste("p2_year", 1:50, sep = "")
  period3_text <- paste("p3_year", 1:50, sep = "")
  
  # spread cost years out across 50 columns for each of the cost periods
  counter<-0
  for(i in c(13,15,17)){
    spread<-strsplit(a.data[,i],"\\,")
    sizes<-sapply(spread,length)
    mat<-matrix(rep(0,50*length(spread)),nrow=length(spread),ncol=50)
    for(i in 1:length(spread)){ # for each object in spread1 list
      if(sizes[i]==0){
        mat[i,1:50]<-rep(0,50)
      }
      else{mat[i,1:sizes[i]]<-spread[[i]]
      }
    }
    mode(mat)<-"integer"
    if(max(mat>50, na.rm=TRUE)){
      return(stop("Code stopped due to years >50 in R_actions",'\n'))
    }
    # create logical matrix and populate with TRUE for years that have costs. This eliminates the need for CleanCosts function in Prioritisation.
    logical<-matrix(rep(FALSE,50*nrow(a.data)),nrow=nrow(a.data),ncol=50)
    for(i in 1:nrow(a.data)){ # for each row in mat
      years<-mat[i,mat[i,]>0]
      logical[i,years]<-TRUE
    }
    counter<-counter+1
    if(counter==1){
      cost.period1<-logical
      colnames(cost.period1)<-period1_text
    }
    if(counter==2){
      cost.period2<-logical
      colnames(cost.period2)<-period2_text
    }
    if(counter==3){
      cost.period3<-logical
      colnames(cost.period3)<-period3_text
    }
  }
  
  cost.period<-cost.period1==TRUE|cost.period2==TRUE|cost.period3==TRUE
  period_text <- paste("Year", 1:50, sep = "")
  colnames(cost.period)<-period_text
  
  if(sum(cost.period>50,na.rm=T)){
    cat('Error >>>>>>>> Some period sequence values are greater than 50.')
    return(invisible)
  }
     if(sum(cost.period1<0,na.rm=T)){
       cat('Error >>>>>>>> Some period sequence values are less than 0.')
       return(invisible)
     }
        
        # spread costs across the 50 years
        cost.1<-a.data$period1_cost*cost.period
        cost.2<-a.data$period2_cost*cost.period
        cost.3<-a.data$period3_cost*cost.period
        costs.allperiods<-cost.1+cost.2+cost.3 
        
        CostList<-list()
        CostList$period<-cost.period
        CostList$text<-period_text
        CostList$allcosts<-costs.allperiods
        return(CostList)
}
