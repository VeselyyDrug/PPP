NewCalcCosts <- function(act_id, action.vector, raw.overlaps, costs, overlaps, ...){
  
  if(!is.logical(action.vector))
    stop("Action vector is not logical", call. = FALSE)
  dim(action.vector) <- c(length(action.vector),1)
  
  if(all(action.vector==FALSE)){ # return costs all 0
    ans <- action.vector*1*costs
    colnames(ans) <- "cost.shared"
    return(ans)
  }
  
  if(overlaps==FALSE){ # overlap indicator is false so return costs that occur in that year but without sharing
    ans <- action.vector*1*costs
    colnames(ans) <- "cost.shared"
    return(ans)
  }
  
  if(sum(raw.overlaps)==0){ #no costs overlap so return costs
    ans <- action.vector*1*costs
    colnames(ans) <- "cost.shared"
    return(ans)
  }
  
  raw.overlaps<-as.matrix(raw.overlaps)
  overlap.flag<-rowSums(raw.overlaps)>0
  real.overlap<-data.frame(act_id,overlap.flag,action.vector,raw.overlaps)
  
  # create final.costs vector to hold costs to return from function
  # where action.vector is F, final.costs is 0
  # where action.vector is T and overlap is F, final.costs get cost value
  # where action.vector and overlap are both T, then final.costs also get
  # cost value as true overlaps will get shared cost and replaced in final.costs
  # at the end of this function, and non-true costs (those with a unique overlapID
  # which gets removed in cost sharing process)
  # will remain as an unshared cost in final.costs.
  
  final.costs<-cbind(act_id, costs)
   
  #### Non-active costs ####
  # Do nothing as the cost for these is already 0 in final.costs
  
  #### Non-overlapping, active costs ####
  # Do nothing as the cost for these is already in final.costs
  
  #### Overlapping costs ####
  # find active overlaps 
  active.row<-which(overlap.flag==TRUE & action.vector==TRUE)
  active.overlaps<-as.matrix(raw.overlaps[active.row,])
  
  if(length(active.row)>0){ # if there are rows with costs to share
    
    # replace any overlapIDs that occur only once with 0.
    activedim<-dim(active.overlaps)
    activevect<-as.vector(active.overlaps)
    single.values<-table(activevect)
    single.ind<-which(single.values==1)
    if(length(single.ind)>0){ # if there are single occurences of an overlapID then
                              # replace with a zero and remove
      singlevect<-as.integer(names(single.values)[single.ind])
      for(element in singlevect){
        index<-which(activevect==element,arr.ind=T)
        activevect[index]<-0
      }
      # put active.overlaps back into orginal dim
      active.overlaps<-matrix(activevect,nrow=activedim[1],ncol=activedim[2])
      zero.row<-rowSums(active.overlaps)>0
      active.overlaps<-matrix(active.overlaps[zero.row,])
      active.row<-active.row[zero.row]
    }
    
    if(length(active.row)>0){ # if there are still rows with costs to share
      # 'Normalise' action costs by the number of codes per action 
      overlap.count<-rowSums(active.overlaps>0)
      codes.ind<-which(active.overlaps > 0, arr.ind = TRUE)
      
      # normalise costs by the number of codes per action
      cost.norm <- costs[active.row]/overlap.count # divides cost by the number of times it is shared
      
      # combine index, costs and overlap codes
      all.codes<-cbind(codes.ind, cost = cost.norm[codes.ind[,1]],
                       code = active.overlaps[codes.ind])
      
      maximums <- aggregate(cost ~ code, data = all.codes, max)
      maxsums <- cbind(aggregate(cost ~ code, data = all.codes, sum), 
                       "maxC" = maximums[,2])
      
      # Merge sums and maximums
      total <- merge(all.codes, maxsums, 
                     by.x = "code", 
                     by.y = "code")
      
      # Set 'sum' column to 1 if 'sum' is zero (so that we are not dividing by zeros)
      if( sum(check <- with(total, cost.y == 0)))
        total[check, "cost.y"] <- 1
      
      # Calculate proportional costs across codes
      result <- cbind(total, "cost.shared" = with(total, cost.x*maxC/cost.y))
      
      # Sum the costs across codes, giving a cost for each action/row
      result.per.row <- aggregate(cost.shared ~ row, data = result, sum)
      
      #populate final.cost with only the active, cost-shared costs contained in result
      final.costs[active.row,2]<-result.per.row[,2]
    }
  }
  
  colnames(final.costs)[2] <- "cost.shared"
  colnames(final.costs)[1] <- "action_id"
  
  # return costs only to Prioritisation function
  ans<-action.vector*0

  ans[,1]<-final.costs[,2]
  
  colnames(ans) <- "cost.shared"
  return(ans)
}




