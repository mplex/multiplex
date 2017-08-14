as.strings <-
function (x, lbs = NULL) 
{
    if (is.array(x) == FALSE) 
        stop("Data must be an array")
    ifelse(isTRUE(is.na(dim(x)[3])) == TRUE, ord <- 1, ord <- dim(x)[3])
    ifelse(is.null(lbs) == FALSE, lst <- list(wt = x, ord = ord, 
        st = lbs), lst <- list(wt = x, ord = ord))
    class(lst) <- "Strings"
    return(lst)
}
