swp <-
function (y, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    sy <- vector()
    length(sy) <- length(y)
    for (i in 1:length(y)) sy[i] <- paste(strsplit(y[i], sep)[[1]][2], 
        strsplit(y[i], sep)[[1]][1], sep = sep)
    rm(i)
    return(sy)
}
