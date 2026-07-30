## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
swp <-
function (y, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    sy <- vector(length = length(y))
    for (i in seq_len(length(y))) {
        sy[i] <- paste(strsplit(y[i], sep)[[1]][2], strsplit(y[i], 
            sep)[[1]][1], sep = sep)
    }
    rm(i)
    return(sy)
}

