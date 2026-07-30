## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
as.strings <-
function (x, lbs) 
{
    if (is.array(x) == FALSE) 
        stop("Data must be an array.")
    ifelse(isTRUE(is.na(dim(x)[3])) == TRUE, ord <- 1, ord <- dim(x)[3])
    ifelse(missing(lbs) == FALSE && is.vector(lbs) == TRUE, lst <- list(wt = x, 
        ord = ord, st = lbs), lst <- list(wt = x, ord = ord))
    class(lst) <- "Strings"
    return(lst)
}

