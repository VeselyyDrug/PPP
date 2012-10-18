PrioritisationII<-function(directory = getwd(), weighting, 
                           multiplier, set_budget, 
                           overlap.indicator, discount.rate, 
                           T, stop.iteration, 
                           a.data, b.data, o.data, w.data, 
                           cost.period, all.costs, ...){

  ppp.call <- match.call()
  
  # Start the timer for the whole prioritisation.  
  beginner <- proc.time()
  cat(paste('Weighting used with power', weighting, ' and multiplier', multiplier, '\n'))

  ######################################################
  ######################################################
  ######################################################
  
  # Calculate weights for all species in the actions dataset.  
  cat('Calculating weights for all species in the actions dataset \n')  
  w.data <- CalculateWeightsII(weights.data = w.data, 
                       affected = unique(with(a.data, species_id)), 
                       weighting = weighting, 
                       multiplier = multiplier)
  
  ######################################################
  ####################  PARAMETERS  ####################
  ######################################################
  
  # [Meaningful] Parameter values
  generated_firstyear_budget  <- Inf  # Generated budget
  loop_iteration              <- 0    # Counter of no. spp removed
  firstyear_budgets           <- c()  # Vect of gen first year budgets
  full_budgets                <- c()  # Vect of gen budget over all 50 years
  
  n_projects <- NROW(a.data)
  n_species <- length(with(a.data, unique(species_id)))
  
  ########################################################
  ### Create a matrix of overlap codes (row per act) #####
  ########################################################
  overlap_data <- MakeOverlaps(with(a.data, action_id), o.data, overlap.indicator)
  
 
  
  ###############################################################
  ## Create a matrix of years actions are costed (row per act) ##
  ###############################################################
  
  # not needed as DataSetup function creates cost.period dataframes    
  # Find column numbers of costs data
  #     period3_sequence <- which(names(a.data) == "p3_year1"):which(names(a.data) == "p3_year50")
  #     period2_sequence <- which(names(a.data) == "p2_year1"):which(names(a.data) == "p2_year50")
  #     period1_sequence <- which(names(a.data) == "p1_year1"):which(names(a.data) == "p1_year50")
  #     
  #     cost.period1 <- CleanCosts(as.matrix(a.data[, period1_sequence]), 50)
  #     cost.period2 <- CleanCosts(as.matrix(a.data[, period2_sequence]), 50)
  #     cost.period3 <- CleanCosts(as.matrix(a.data[, period3_sequence]), 50)
  
  cat('Preliminary data cleaning complete.\n')
  cat('##################################\n')
  cat('Starting loop for... \n')
  
  ###############################
  ### Print initial summaries ###
  ###############################
  
  # Print the number of actions, unique species, unique genera, unique families, unique orders 
  cat('\t', n_projects, '\tunique actions,\n') 
  cat('\t', n_species, '\tunique species,\n') 
  cat('\t', length(with(w.data, table(genus))), '\tunique genera present,\n') 
  cat('\t', length(with(w.data, table(family))), '\tunique families present, \n') 
  cat('\t', length(with(w.data, table(order))), '\tunique orders present.\n') 
  
  #######################################
  ######## CLEAN THE WORKSPACE ##########
  #######################################
  
  #     rm(a.names, b.names, w.names,
  #        a.classes, b.classes, w.classes,
  #        period1_sequence, period2_sequence, period3_sequence, 
  #        period1_text, period2_text, period3_text)
  
  ################################################################
  ###########################  LOOP ##############################
  ################################################################
  
  # Initialise the data.frames of remaining and removed species
  removed <- c()
  
  # Create the 'remaining' list of species
  remaining.spp.df <- data.frame('species_id' = with(a.data, unique(species_id)), 
                                 'CE' = 1, 'B' = 0, 'W' = 0, 'Co' = 1, 'S' = 0)
  
  w_match <- match(with(remaining.spp.df, species_id), with(w.data, species_id))
  remaining.spp.df <- 
    cbind(remaining.spp.df, 
          "init.W" = w.data[w_match, "weight_value"])
  remaining.spp.df$W <- w.data[w_match, "weight_value"]
  
  # Add some extra information for each species, ie sciname, genus, family, order, etc
  extra <- match(with(a.data, unique(species_id)), with(w.data, species_id))
  
  
  remaining.spp.df <- cbind(remaining.spp.df, w.data[extra,c("sciname", "genus", 
                                                             "family", "order", "Bs", "Bg", "Bf", "Endem.spp", "Endem.gen", "Endem.fam")])
  
  # Create the 'removed' list of species (currently empty)
  removed.spp.df <- data.frame(remaining.spp.df[rep(FALSE, NROW(remaining.spp.df)),], 
                               iteration = numeric(0))
  
  ############################################################################################
  ############################################################################################
  
  while(generated_firstyear_budget > set_budget){ # & stop.iteration != loop_iteration ){
    
    #********************* DELETE SPECIES IF CE=0 *****************
    
    # Delete species_id which have a CE of zero, inf or NaN
    
    removers <- with(remaining.spp.df, is.nan(CE)|is.infinite(CE)|is.na(CE)|CE==0)
    removers <- with(remaining.spp.df, species_id[removers])
    
    # If we're past the first iteration of the prioritisation then 
    # take off the species with the smallest CE value
    
    if(loop_iteration > 1){
      # Take the last species out
      removed <- with(remaining.spp.df, species_id[n.remaining])
      cat(paste('Removed species: ', removed, ',', with(remaining.spp.df, sciname[ n.remaining ]), '\n'))
      
      removers <- union(removers, removed)
    }
    
    if(sum(removers)>0){ # if there are any species due to strange CE, then print a summary
      if(loop_iteration == 1){
        #out <- with(remaining.spp.df, species_id[removers])
        cat('\t|------------------------------------------\n')
        cat('\t| No. spp removed (CE = 0) : \t', with(remaining.spp.df, sum(CE == 0)), '\n')
        cat('\t| No. spp removed (B = 0) : \t', with(remaining.spp.df, sum(B==0)), '\n')
        cat('\t| No. spp removed (S = 0): \t', with(remaining.spp.df, sum(S==0)), '\n')
        cat('\t| No. spp removed (B = S = 0): \t', with(remaining.spp.df, sum(S==0 & B==0)), '\n')
        cat('\t| No. spp removed (W = 0):\t', with(remaining.spp.df, sum(W==0)), '\n')
        cat('\t|------------------------------------------\n')
      }
    
      # Update the list of removed species and the ordered list
      to.remove <- cbind(subset(remaining.spp.df, subset = species_id %in% removers), 
                         "iteration" = loop_iteration)
      removed.spp.df <- rbind(removed.spp.df, to.remove)
      remaining.spp.df <- subset(remaining.spp.df, subset = !(species_id %in% removers))
      
      no_actions_removed <- sum(with(a.data, species_id==removed))
      cat('\t| actions removed (from last it.) : ', no_actions_removed, '\n')
      cat('\t| species removed so far : \t\t', NROW(removed.spp.df), '\n')
      
      # Remove actions and benefits associated with the removed species
      out_actions <- with(a.data, species_id %in% removers)
      a.data <- subset(a.data, subset = !out_actions)
      overlap_data <- subset(overlap_data, subset = !out_actions)
      cost.period <- subset(cost.period, subset = !out_actions)
      costs_matrix <- subset(costs_matrix, subset = !out_actions)
      all.costs<-subset(all.costs, subset = !out_actions)
    }
    
    # should be the same as length(with(a.data, unique(species_id)))
    n.remaining <- NROW(remaining.spp.df) 
    cat('\t| Species left:\t\t\t', n.remaining, '\n')
    cat('\t|-----------------------------------\n\n')
    # Check if there are any species left, if not then exit the prioritisation
    if(n.remaining < 1){
      generated_firstyear_budget <- 0
      generated_full_budget <- 0
      full_budgets <- c(full_budgets, generated_full_budget)
      firstyear_budgets <- c(firstyear_budgets, generated_firstyear_budget)
      cat(paste('All species removed under budget $', prettyNum(set_budget, big.mark=","), '\n', sep = ""))
      #break
    
      now <- gsub(":", "-", date())
      ans <- list(duration = proc.time() - beginner)
      ans$date <- now # used for naming files and directories
      ans$func_call <- ppp.call
      
      # Data sets
      ans$initially_removed <- subset(removed.spp.df, iteration == 1)
      
      # round outputs for csv files
      removed.spp.df<-merge(removed.spp.df,b.data[,c(1:3)],by="species_id",sort=FALSE)
      removed.spp.df[,c(3:4,7)]<-round(removed.spp.df[,c(3:4,7)],digits = 2)
      removed.spp.df[,5]<-round(removed.spp.df[,5], digits = 0)                 
      ans$removed.spp.df<-removed.spp.df
      
      #no species are remaining so nothing to write here...
      ans$ranked_list <-data.frame(NULL)
      ans$ordered_annual_costs <-data.frame(NULL)
      
      # Budgets
      ans$full.budgets <- full_budgets
      ans$firstyear.budgets <- firstyear_budgets
      
      # Extra costs data
      crows <- data.frame(a.data$spp_text, a.data$action_id, a.data$species_id)
      ans$costs_matrix <- cbind(crows, costs_matrix)
      #ans$period1_costs <- cbind(crows, period1_costmatrix)
      #ans$period2_costs <- cbind(crows, period2_costmatrix)
      #ans$period3_costs <- cbind(crows, period3_costmatrix)
      
      ans$dir <- directory
      
      # Save parameters of interest
      ans$datasets <- data
      
      ans$input.parameters <- 
        cbind(set_budget, 
              n_projects, 
              n_species, 
              weighting, 
              multiplier, 
              discount.rate)
      
      colnames(ans$input.parameters) <- 
        list("Set_budget", 
             "N_projects", 
             "N_species", 
             "W_weight", 
             "W_multiplier", 
             "Discount")
      
      ans$output.parameters <- 
        cbind(generated_full_budget, 
              generated_firstyear_budget, 
              loop_iteration, 
              NROW(removed.spp.df), 
              NROW(remaining.spp.df),
              NROW(subset(removed.spp.df, iteration == 1)))
      
      colnames(ans$output.parameters) <- 
        list("Gen_full_budget", 
             "Gen_ann_budget", 
             "N_loops", 
             "N_removed", 
             "N_ranked", 
             "N_initially_rem")
      
      # Save host computer specific variables
      ans$host.platform <- .Platform
      ans$host.r.version <- getRversion()
      
      
      cat('################################################\n')
      cat('Prioritisation took', prettyNum(ans$duration, big.mark=',')[3], 'seconds\n')
      cat('################################################\n')
      
      class(ans) <- "PPP"
      return(ans)
    }
    
    loop_iteration <- loop_iteration + 1
    cat('##################################\n')
    cat(paste('Loop iteration:\t', loop_iteration, '\n'))
  
#     if(loop_iteration==10){
#       browser()
#     }
    
    ############################################################################################
    ############################################################################################
    
    # All lists will be ordered in this order, until they are ordered wrt CE.  
    unique_spp_no <- with(a.data, sort(unique(species_id)))
    # DOES THIS CHANGE IF WE USE unique_spp_no <- with(remaining.spp.df, species_id) # it shouldn't...
    
    # Find the rows of each dataset associated with each species
    actions_spp_index <- lapply(unique_spp_no, function(x) with(a.data, which(species_id == x)))
    benefits_spp_index <- match(unique_spp_no, with(b.data, species_id))
    weights_spp_index <- match(unique_spp_no, with(w.data, species_id))
    
    ##########################################################
    ####### CALC. SPECIES WEIGHTS ############################
    ##########################################################
    
    cat('\tUpdating new weights \n')
    
    output <- 
      UpdateWeightsII(
        weights.data = w.data, 
        just.removed = union(removed, removers),
        remaining = with(remaining.spp.df, species_id))
    
    w.data <- output$w
    affected.list <- output$aff
    
    cat('\tCalculating new weights \n')
    w.data <- 
      CalculateWeightsII(
        weights.data = w.data, 
        affected = affected.list, 
        weighting = weighting, 
        multiplier = multiplier)
    
    cat('\t|-----------------------------------\n\n')
    
    # -------------------- IDENTIFYING SPECIES' WEIGHTS ------------
    
    WEIGHT <- with(w.data, weight_value[weights_spp_index])
    
    ########################################################################
    ### STEP 3 ADDING UP COSTS, CALC. BENE., WEIGHTS, S, and COSTS #########
    ########################################################################
    
    cat('\tCombining costs, benefits, weights and success probs\n')
    
    # Print the number of species we're sorting
    cat('\tSorting costs of the ', length(unique_spp_no), ' species \n')
    
    # --------------------- CALCULATE COSTS --------------------
    
    cat('\n\tCalculating shared costs... \n')
    
    costs_matrix <- sapply(1:50, function(x) NewCalcCosts(
       act_id = a.data$action_id,
      action.vector = subset(cost.period, select = x), 
      raw.overlaps = overlap_data, 
      costs = subset(all.costs, select = x),
       overlaps = overlap.indicator))
    
#     cat('\t...period 2 costs...\n')
#     period2_costmatrix <- sapply(1:50, function(x) NewCalcCosts(
#        act_id = a.data$action_id,
#       action.vector = subset(cost.period2, select = x), 
#       raw.overlaps = overlap_data, 
#       costs = a.data$period2_cost, overlaps = overlap.indicator))
#     
# #   if(loop_iteration==481){
# #     browser()
# #   }
#     cat('\t...period 3 costs...\n')
#     period3_costmatrix <- sapply(1:50, function(x) NewCalcCosts(
#        act_id = a.data$action_id,
#       action.vector = subset(cost.period3, select = x), 
#       raw.overlaps = overlap_data, 
#       costs = a.data$period3_cost, overlaps = overlap.indicator))
    
    cat('\t|-----------------------------------\n\n')
    
   
    # Calculate cost per prescription (all actions for a species)
    # Sum costs of all actions relating to each species in each year
    costs_m <- lapply(actions_spp_index, function(x) costs_matrix[x,])
    
    # Cost per year (for each year calculate the sum over the actions)
    cost_prescr_vect <- lapply(costs_m, colSums)
    
    # Calculate cost of prescription in today's dollar terms for each species
    # by applying discounting to each years cost
    disc <- (1 + discount.rate)^(0:49)
    new_costs <- lapply(cost_prescr_vect, function(x) (x/disc))
    
    # Find the order of new_costs
    orders <- lapply(new_costs, function(x) c(which(x != 0), which(x == 0)))
    annual_costs <- t(sapply(1:length(new_costs), function(x) new_costs[[x]][orders[[x]]]))
    
    # Sum all costs for each species over the 50 years
    COST <- sapply(new_costs, sum)
    
    #********************* STEP 5 GENERATE BUDGETS *****************
    
    # Budget for generated actions
    generated_full_budget <- sum(COST)
    full_budgets <- c(full_budgets, generated_full_budget)
    cat(paste('Generated full budget: $', prettyNum(generated_full_budget, big.mark=","), '\n', sep=""))
    
    # Annual (year 1) budget for generated actions
    generated_firstyear_budget <- colSums(annual_costs)[1]
    firstyear_budgets <- c(firstyear_budgets, generated_firstyear_budget)
    cat(paste('Generated annual budget: $', 
              prettyNum(generated_firstyear_budget, big.mark=","),
              '\t(set: $', prettyNum(set_budget, big.mark=","), ')\n', sep=""))
    
    # --------------------- CALCULATE BENEFITS --------------------
    
    # for each species what are the benefits WITH actions and benefits WITHOUT actions
    BENEFIT <- with(b.data, (bene_with_action[benefits_spp_index] - bene_no_action[benefits_spp_index])/100)
    
    # ---------------- CALCULATE PR( SUCCESS ) -------------
    
    # Calculate Pr(INPUT SUCCESS), Pr(OUTPUT SUCCESS), Pr(OUTCOME SUCCESS), per species
    pr_inp <- sapply(actions_spp_index, function(x) prod(a.data$pr_m_data[x]/100))
    pr_output <- sapply(actions_spp_index, function(x) prod(a.data$pr_o_data[x]/100))
    pr_outcome <- sapply(actions_spp_index, function(x) prod(a.data$pr_t_data[x]/100))
    
    # Calculate Pr(total success) per species
    SUCCESS <- pr_inp * pr_output * pr_outcome
    
    #---------------------IDENTIFYING SPECIES' CODE AND NAME-----------------
    
    ## Grab species names (prescription ID) and scientific names
    #spp_names <- sapply(actions_spp_index, function(x) with(a.data, species_id[x[1]]))
    #spp_sciname <- with(b.data, sciname[benefits_spp_index])
    
    #spp_genus <- with(w.data, genus[weights_spp_index])
    #spp_family <- with(w.data, family[weights_spp_index])
    #spp_order <- with(w.data, order[weights_spp_index])
    
    cat('\t|-----------------------------------\n\n')
    
    #********************* STEP 4 GENERATE RANK ORDERED LIST *************
    #-------------------- CALCULATE COST EFFICIENCY VALUE ---------------
    
    # Cost efficiency of each species 
    # (ordered in the same order as unique_spp_no)
    COST.EFFICIENCY <- (BENEFIT*WEIGHT*SUCCESS)/COST
    
    # Find rank order of CE
    spp_rank <- order(COST.EFFICIENCY, decreasing = TRUE)
    
    # Arrange the species IDs in this order
    species_id.ranked <- unique_spp_no[spp_rank]
    
    # Match the ranked species IDs with the remaining.spp.df and reorder remaining.spp.df
    remaining.spp.ranking <- match(species_id.ranked, with(remaining.spp.df, species_id))
    remaining.spp.df <- remaining.spp.df[remaining.spp.ranking,]
    
    # Update the B, W, S, Co, and CE of the remaining.spp.df
    remaining.spp.df <- 
      within(remaining.spp.df, {
        S <- SUCCESS[spp_rank]
        W <- WEIGHT[spp_rank]
        B <- BENEFIT[spp_rank]
        Co <- COST[spp_rank]
        CE <- COST.EFFICIENCY[spp_rank]
      })
    
    ordered_annual_costs <- annual_costs[spp_rank,] # Check
    
    #****************** CLEAN THE WORKSPACE ***************
    rm(actions_spp_index, annual_costs, benefits_spp_index, 
       cost_prescr_vect, costs_m, new_costs, orders, pr_inp, 
       pr_outcome, pr_output, removers, #spp_family, spp_genus, 
       #spp_names, spp_order, spp_rank, spp_sciname, 
       unique_spp_no, weights_spp_index) #weight_name
    #********************   ---  *****************
    
  } # end WHILE
  
  # Write the current time to a variable for filenames
  # (change colons in the date string to hyphens)  
  now <- gsub(":", "-", date())
  
  ######################################
  ##   Save removed.spp.df to csv file ##
  ######################################
  
#   write.csv(removed.spp.df, 
#             file = paste(directory, '/Removed_list.csv', sep = ""), 
#             row.names = FALSE)
  
  #########################
  ##   Save PPP object      ##
  #########################
  
  # Save the duration of the prioritisation (seconds)
  ans <- list(duration = proc.time() - beginner)
  
  ans$date <- now # used for naming files and directories
  ans$func_call <- ppp.call
  
  # Data sets
  ans$initially_removed <- subset(removed.spp.df, iteration == 1)
  #ans$removed.spp.df <- data.frame(subset(removed.spp.df, iteration > 2))
  
  # round outputs for csv files
  removed.spp.df<-merge(removed.spp.df,b.data[,c(1:3)],by="species_id",sort=FALSE)
  removed.spp.df[,c(3:4,7)]<-round(removed.spp.df[,c(3:4,7)],digits = 2)
  removed.spp.df[,5]<-round(removed.spp.df[,5], digits = 0)                 
  ans$removed.spp.df<-removed.spp.df
  
  remaining.spp.df<-merge(remaining.spp.df,b.data[,c(1:3)],by="species_id",sort=FALSE)
  remaining.spp.df[,c(3:4,7)]<-round(remaining.spp.df[,c(3:4,7)],digits = 2)
  remaining.spp.df[,5]<-round(remaining.spp.df[,5],digits = 0)
  ans$ranked_list <- remaining.spp.df
  
  ans$ordered_annual_costs <- round(ordered_annual_costs)
  
  # Budgets
  ans$full.budgets <- full_budgets
  ans$firstyear.budgets <- firstyear_budgets
  
  # Extra costs data
  crows <- data.frame(a.data$spp_text, a.data$action_id, a.data$species_id)
  ans$costs_matrix <- cbind(crows, costs_matrix)
#   ans$period1_costs <- cbind(crows, period1_costmatrix)
#   ans$period2_costs <- cbind(crows, period2_costmatrix)
#   ans$period3_costs <- cbind(crows, period3_costmatrix)
  
  ans$dir <- directory
  
  # Save parameters of interest
  ans$datasets <- data
  
  ans$input.parameters <- 
    cbind(set_budget, 
          n_projects, 
          n_species, 
          weighting, 
          multiplier, 
          discount.rate)
  
  colnames(ans$input.parameters) <- 
    list("Set_budget", 
         "N_projects", 
         "N_species", 
         "W_weight", 
         "W_multiplier", 
         "Discount")
  
  ans$output.parameters <- 
    cbind(generated_full_budget, 
          generated_firstyear_budget, 
          loop_iteration, 
          NROW(removed.spp.df), 
          NROW(remaining.spp.df),
          NROW(subset(removed.spp.df, iteration == 1)))
  
  colnames(ans$output.parameters) <- 
    list("Gen_full_budget", 
         "Gen_ann_budget", 
         "N_loops", 
         "N_removed", 
         "N_ranked", 
         "N_initially_rem")
  
  # Save host computer specific variables
  ans$host.platform <- .Platform
  ans$host.r.version <- getRversion()
  
  
  cat('################################################\n')
  cat('Prioritisation took', prettyNum(ans$duration, big.mark=',')[3], 'seconds\n')
  cat('################################################\n')
  
  class(ans) <- "PPP"
  return(ans)
  }
  
  
