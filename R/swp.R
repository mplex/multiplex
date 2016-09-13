swp <-
function (y, prsep) 
{
    ifelse(missing(prsep) == TRUE, prsep <- ", ", NA)
    sy <- vector()
    length(sy) <- length(y)
    for (i in 1:length(y)) sy[i] <- paste(strsplit(y[i], prsep)[[1]][2], 
        strsplit(y[i], prsep)[[1]][1], sep = prsep)
    rm(i)
    return(sy)
}
