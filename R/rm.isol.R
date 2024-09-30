rm.isol <-
function (x, diag, diag.incl) 
{
    if (isTRUE(sum(x) == 0L) == TRUE) 
        return(NULL)
    ifelse(missing(diag) == FALSE && isTRUE(diag == FALSE) == 
        TRUE, diag <- FALSE, diag <- TRUE)
    ifelse(is.na(dim(x)[3]) == TRUE | isTRUE(dim(x)[3] == 1) == 
        TRUE, px <- x, px <- mnplx(x))
    ifelse(isTRUE(diag == TRUE) == TRUE, NA, diag(px) <- 0L)
    out <- vector()
    for (i in seq_len(nrow(x))) {
        ifelse(isTRUE(sum(px[i, ] + px[, i]) == 0L) == TRUE, 
            out[length(out) + 1L] <- i, NA)
    }
    rm(i)
    inn <- which(!(seq_len(nrow(x)) %in% out))
    ifelse(missing(diag.incl) == FALSE && isTRUE(diag.incl == 
        FALSE) == TRUE, diag(x) <- 0, NA)
    if (is.na(dim(x)[3]) == TRUE) {
        x[inn, inn]
    }
    else {
        x[inn, inn, ]
    }
}
