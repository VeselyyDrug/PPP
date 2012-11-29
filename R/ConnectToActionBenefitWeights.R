ConnectToActionBenefitWeights<-function(data,...){
  
 # This function connects to the database specified in "data" and loads
 # action, benefit and weights data. It sets any
 # NA or blank values for costs to 0. It also calls the SetupWeights function
 # which performs dataset up and initial weighting calculations on the weights
 # dataset  

 
  # Connect to Access database and read qry tables
  cat('Connecting to database and reading...','\n')
  dbconn<-odbcConnectAccess(data)
  
  # action data
  cat('...action data','\n')
  a.data<-sqlFetch(dbconn,"R_actions")
  for(n in c(1,2,4,6,18:20)){
    a.data[,n]<-as.integer(a.data[,n])
  }
  for(n in c(3,5,7:11,13,15,17)){
    a.data[,n]<-as.character(a.data[,n])
  }
  for(n in c(12,14,16)){
    a.data[,n]<-as.numeric(a.data[,n]) #cost columns
  }
  a.names <-  c("action_id", "species_id",
                "taxa_code_text", "taxa_code", "spp_text", "spp_code",
                "method", "method1_text", "method2_text", "method3_text",
                "method4_text", "period1_cost", "period1_sequence",
                "period2_cost", "period2_sequence",
                "period3_cost", "period3_sequence", "pr_t_data",
                "pr_o_data", "pr_m_data")
  names(a.data)<-a.names
  
  # benefit data
  cat('...benefit data','\n')
  b.data<-sqlFetch(dbconn,"R_benefits")
  for(n in c(1,3,6:8)){
    b.data[,n]<-as.integer(b.data[,n])
  }
  for(n in c(2,4:5)){
    b.data[,n]<-as.character(b.data[,n])
  }
  
  b.names <- c("species_id", "taxa_text", "taxa_code", "spp_text",
               "sciname", "bene_with_action", "bene_no_action",
               "spp_code")
  names(b.data)<-b.names    
  
  # weights data
  cat('...weights data','\n')
  if(input.w.data)
    w.data<-sqlFetch(dbconn,"R_weights")
    for(n in c(1,5,6,8,9,11:15)){
        w.data[,n]<-as.integer(w.data[,n])
    }
    for(n in c(2:4,7,10)){
        w.data[,n]<-as.character(w.data[,n])
    }
    w.names <- c("species_id", "species_type", "sciname", "genus", "Bs",
                "Ts", "family", "Bg", "Tg", "order", "Bf",
                "Tf", "Endem.spp", "Endem.gen", "Endem.fam")
                names(w.data)<-w.names
    
                if(any(is.na(w.data[,2:15]))){
                    return(stop("Code stopped due to NA values in w.data columns 2:15"))
                }
  
                #Close database connection
                close(dbconn)
  }else{
      w.data <- data.frame(
      species_id = with(b.data, species_id), 
      taxa_text = with(b.data, taxa_text), 
      sciname = with(b.data, sciname), 
      genus = "aa", spp_in_gen = 1, thr_spp_in_gen = 1, 
      family = "aa", gen_in_fam = 1, thr_gen_in_fam = 1, 
      order = "aa", fam_in_ord = 1, thr_fam_in_ord = 1,
      Endem_spp = 0, Endem_gen = 0, Endem_fam = 0)
  }
  
  cat('Setting up weights data','\n')
  w.data <- SetupWeights(w.data)
  
  # for any NA values in period_cost set to 0
  NA.index<-with(a.data,is.na(period1_cost))
  a.data$period1_cost[NA.index]<-0
  NA.index<-with(a.data,is.na(period2_cost))
  a.data$period2_cost[NA.index]<-0
  NA.index<-with(a.data,is.na(period3_cost))
  a.data$period3_cost[NA.index]<-0
  
  
  
 
  
  DataList<-list()
  DataList$actions<-a.data
  DataList$benefits<-b.data
  DataList$weights<-w.data
  return(DataList)
}
  
  
  
  
 