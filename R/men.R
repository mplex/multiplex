men <-
function (y, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    mn <- vector()
    if (isTRUE(length(y) > 0) == TRUE) 
        for (i in 1:length(y)) if (isTRUE(strsplit(y[i], sep)[[1]][1] < 
            strsplit(y[i], sep)[[1]][2]) == TRUE) 
            mn <- append(mn, y[i])
    rm(i)
    return(mn)
}
