print.summary.PPP <-
function(x, ...){
    
    # Function call and timing
    hours <- x$duration[3]%/%3600
    minutes <- x$duration[3]%/%60 - hours*60
    seconds <- x$duration[3]%%60
    cat("\nCall: \n", paste(deparse(x$func_call), sep = "\n", collapse = "\n"), sep = "")
    cat('"\n\nTiming:\nPrioritisation complete on ', x$date,' and took ',
    hours,'h',minutes,'m',round(seconds),'s\n\n',sep="")
    
    # Budget summaries
    cat('Budgets:', 
    '\n\nFull budgets:\n', x$full.budgets, 
    '\n\nFirst year budgets:\n', x$firstyear.budgets, 
    '\n\n')
    
    # Parameters/arguments    
    cat('Input parameters:', 
    '\nSet ann budget:\t\t', prettyNum(x$input.parameters[1], big.mark = ","),
    '\nN projects:\t\t', x$input.parameters[2],
    '\nN species :\t\t', x$input.parameters[3],
    '\nWeighting power:\t', x$input.parameters[4], 
    '\nWeighting multiplier:\t', x$input.parameters[5], 
    '\nDiscount rate:\t\t', x$input.parameters[6],
    '\n\n')
    
    cat('Output parameters:', 
    '\nGen full budget:\t', prettyNum(x$output.parameters[1], big.mark = ","),
    '\nGen first year budget:\t', prettyNum(x$output.parameters[2], big.mark = ","),
    '\nIterations:\t\t', x$output.parameters[3],
    '\nN removed:\t\t', x$output.parameters[4],
    '\nN ranked:\t\t', x$output.parameters[5],
        '\nSpp removed in 1st it:\t', x$output.parameters[6])
    
    # Print smallest/largest budget drop IF any species were removed
    n.initially.removed <- x$output.parameters[4] - x$output.parameters[6]
    if( n.initially.removed > 0){
    cat('\nSmallest drop (ann):\t', prettyNum(round(x$smallest.diff, digit = 2), big.mark = ","), 
    '(', as.character(x$smallest.name), ')',
    '\nMean drop (ann):\t', prettyNum(round(x$mean.diff, digits = 2), big.mark = ","),
    '\nMedian drop (ann):\t', prettyNum(round(x$median.diff, digits = 2), big.mark = ","),
    '\nLargest drop (ann):\t', prettyNum(round(x$largest.diff, digit = 2), big.mark = ","), 
    '(', as.character(x$largest.name), ')\n')
    }
}