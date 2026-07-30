## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
mnplx <-
function (net, directed = TRUE, valued, diag, clu) 
{
    if (missing(valued) == FALSE && isTRUE(valued == FALSE) == 
        TRUE) {
        flgval <- FALSE
        net <- dichot(net, c = 1L)
    }
    else {
        flgval <- TRUE
    }
    ifelse(missing(diag) == FALSE && isTRUE(diag == FALSE) == 
        TRUE, flgdg <- FALSE, flgdg <- TRUE)
    ifelse(missing(clu) == FALSE && is.vector(clu) == TRUE, net <- perm(net, 
        clu = clu), NA)
    if ((is.na(dim(net)[3]) == TRUE | isTRUE(dim(net)[3] == 1L) == 
        TRUE)) {
        ifelse(isTRUE(dim(net)[3] == 1) == TRUE, net <- net[, 
            , 1], NA)
        ifelse(isTRUE(directed == FALSE) == TRUE, net <- (net + 
            t(net)), NA)
        ifelse(isTRUE(flgdg == FALSE) == TRUE, diag(net) <- 0L, 
            NA)
        ifelse(isTRUE(flgval == FALSE) == TRUE, return(dichot(net, 
            c = 1L)), return(net))
    }
    else {
        mat <- matrix(0L, nrow = dim(net)[1], ncol = dim(net)[2])
        for (k in seq_len(dim(net)[3])) mat <- mat + net[, , 
            k]
        ifelse(isTRUE(directed == TRUE) == TRUE, NA, mat <- mat + 
            t(mat))
        ifelse(isTRUE(flgval == FALSE) == TRUE, mat <- dichot(mat, 
            c = 1L), NA)
        ifelse(isTRUE(flgdg == FALSE) == TRUE, diag(mat) <- 0L, 
            NA)
        mat
    }
}

