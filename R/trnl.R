## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
trnl <-
function (x, lbs = NULL, lb2lb = FALSE, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (isTRUE(is.matrix(x) == TRUE) == FALSE) 
        x <- as.matrix(x)
    if (is.null(lbs) == FALSE | isTRUE(lb2lb == TRUE) == FALSE) {
        lbsr <- lbsc <- lbs
    }
    else {
        lbsr <- dimnames(x)[[1]]
        lbsc <- dimnames(x)[[2]]
    }
    rws <- vector()
    cls <- vector()
    inc <- list()
    for (l in seq_len(max(x))) {
        xd <- dichot(x, c = l)
        for (i in seq_len(length(which((xd) == 1L)))) {
            cls[i] <- (ceiling(which((xd) == 1L)/dim(x)[1]))[i]
            ifelse((which((xd) == 1L)%%dim(x)[1])[i] == 0L, rws[i] <- (which((xd) == 
                1L)%%dim(x)[1])[i] + dim(x)[1], rws[i] <- (which((xd) == 
                1L)%%dim(x)[1])[i])
            ifelse(isTRUE(lb2lb == TRUE) == TRUE, inc[[length(inc) + 
                1L]] <- paste(lbsr[rws[i]], lbsc[cls[i]], sep = sep), 
                inc[[length(inc) + 1L]] <- paste(rws[i], cls[i], 
                  sep = sep))
        }
        rm(i)
    }
    rm(l)
    return(sort(unlist(inc)))
}

