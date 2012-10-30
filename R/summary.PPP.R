summary.PPP <-
function(object, verbose_cost_info = TRUE, save.to.file = TRUE, directory=getwd(), prefix, ...){
    pppobject <- object
    
    if(save.to.file){
        # Create a new directory to store all output?
        # Save the ranked list
        write.csv(pppobject$ranked_list, 
            file = paste(directory, '/', prefix,'_Remaining_list.csv', sep = ""), 
            row.names = FALSE)
        # Save the removed list.  
        write.csv(pppobject$removed.spp.df, 
            file = paste(directory, '/', prefix,'_Removed_list.csv', sep = ""), 
            row.names = FALSE)
        # Save the input parameters
        write.csv(pppobject$input.parameters, 
            file = paste(directory, '/', prefix,'_input.parameters.csv', sep = ""), 
            row.names = FALSE)
        # Save the output parameters
        write.csv(pppobject$output.parameters, 
            file = paste(directory, '/', prefix,'_output.parameters.csv', sep = ""), 
            row.names = FALSE)
        
        # Save the ordered costed species list
        if(verbose_cost_info){
            
            extra_ordered_list <- data.frame(pppobject$ranked_list, 
                                            pppobject$ordered_annual_costs)
            write.csv(extra_ordered_list, 
                file = paste(directory, '/', prefix,'_Ranked_list.csv', sep = ""), 
                row.names = FALSE)

            # Write the costs_matrix
            write.csv(pppobject$costs_matrix, 
                file = paste(directory, '/', prefix, '_TotalCosts.csv', sep = ""), 
                row.names = FALSE)

            # Write the matrices from the 3 period cost calculations
#             write.csv(pppobject$period1_costs, 
#                 file = paste(directory, '/', prefix, '_SimpleCostsPeriod1.csv', sep = ""), 
#                 row.names = FALSE)
#                 
#             write.csv(pppobject$period2_costs, 
#                 file = paste(directory, '/', prefix, '_SimpleCostsPeriod2.csv', sep = ""), 
#                 row.names = FALSE)
#                 
#             write.csv(pppobject$period3_costs, 
#                 file = paste(directory, '/', prefix, '_SimpleCostsPeriod3.csv', sep = ""), 
#                 row.names = FALSE)
            }
            cat('\nResults saved in', directory, '\n')
    } # if SAVE end
    
    ans <- pppobject[c("func_call", "dir", "duration", 
                "input.parameters", "output.parameters", 
                "full.budgets", "firstyear.budgets", "date")]
    
    ans$save.dir <- directory
    
    # If loop_iterations is greater than 2
    if(pppobject$output.parameters[3] > 2){
        ans$full.budget.diff <- abs(diff(pppobject$full.budgets[-1]))
        ans$ann.budget.diff <- abs(diff(pppobject$firstyear.budgets))
        
        diff.order <- order(-ans$ann.budget.diff)
        ordered.removed <- pppobject$removed.spp.df[diff.order,]
        
        ans$smallest.name <- as.character(ordered.removed[NROW(ordered.removed),c("sciname")])
        ans$smallest.diff <- with(ans, min(ann.budget.diff))
        
        ans$mean.diff <- with(ans, mean(ann.budget.diff))
        ans$median.diff <- with(ans, median(ann.budget.diff))
        
        ans$largest.name <- as.character(ordered.removed[1, c("sciname")])
        ans$largest.diff <- ans$ann.budget.diff[1]
    }
    class(ans) <- "summary.PPP"
    return(ans)
}
