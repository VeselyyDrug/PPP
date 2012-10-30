plot.PPP <-
function(x, ...){
    # Plot budget, species removed and iteration number.  
    
    # If the number of iterations is 1, then do not plot anything.  
    if(x$output.parameters[3] < 2){
        cat('Nothing to plot, no species were removed.\n')
        return(invisible())
    }
    
    # Change axes text to plot in millions/billions/etc
    suffix.words <- c("", "thousands", "millions", "billions", "trillions", "quadrillions")
    full.suff <- log10(median(x$full.budgets)) %/% 3
    first.suff <- log10(median(x$firstyear.budgets)) %/% 3
    budsuffix <- paste("Full budget (", suffix.words[full.suff+1], ")", sep = "")
    fbudsuffix <- paste("First year annual budget (", suffix.words[first.suff+1], ")", sep = "")
    
    # Increase margins to fit RHS axis label
    par(mar=c(5,4,4,5)+.1)

#    plot(c(2, x$output.parameters[3]), yrange, col = 'white', 
    # Plot full budgets
    plot(2:x$output.parameters[3], x$full.budgets/10^(3*full.suff), 
    col = "blue", 
    type = 'l',
    xlab = "Iteration",
    ylab = budsuffix,
    main = paste("Generated budgets versus removed species\n", x$date))
    
    par(new = TRUE)
    
    # Plot first year budgets
    plot(2:x$output.parameters[3], x$firstyear.budgets/10^(3*first.suff), 
        type = 'l', 
        xlab = "",
        ylab = "",
        xaxt = "n", 
        yaxt = "n",
        col = "red")
    
    # Add RHS axis and legend
    axis(4)
    mtext(fbudsuffix, side = 4,line = 3)
    legend("bottomleft", col = c("blue","red"), lty = 1, legend = c("Full budget","First year budget"))
}
