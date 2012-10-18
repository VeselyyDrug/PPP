MakeOverlaps <-
function(act_id, o.data, overlap.indicator){

  # Use match()?
  if(overlap.indicator==TRUE){
    result <- lapply(act_id, function(y) c(y, which(o.data$action_id==y))) 
    overlap_data <- lapply(result, function(x) c(x[1], o.data$overlap_id[x[-1]])) 
    
    # Find the number of elements of each list item
    sizes <- sapply(overlap_data, length)
    
    mat <- matrix(rep(0, max(sizes)*length(overlap_data)), nrow = length(overlap_data), ncol = max(sizes))
    for(i in 1:length(sizes)){
      
      mat[i,1:sizes[i]] <- overlap_data[[i]]
    }
    return(mat[,-1])
    
    #if overlap.indicator=FALSE then return the single column of unique overlapIDs
  }else{o.data[,-1]}
 
}
