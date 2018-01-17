mnplx <-
function (net, directed = TRUE, dichot, diag, clu) 
{
    if (missing(dichot) == FALSE && isTRUE(dichot == FALSE) == 
        TRUE) {
        dichot <- FALSE
    }
    else {
        dichot <- TRUE
        net <- dichot(net, c = 1L)
    }
    ifelse(missing(diag) == FALSE && isTRUE(diag == FALSE) == 
        TRUE, diag <- FALSE, diag <- TRUE)
    ifelse(missing(clu) == FALSE && is.vector(clu) == TRUE, net <- perm(net, 
        clu = clu), NA)
    if ((is.na(dim(net)[3]) == TRUE | isTRUE(dim(net)[3] == 1L) == 
        TRUE)) {
        ifelse(isTRUE(dim(net)[3] == 1) == TRUE, net <- net[, 
            , 1], NA)
        ifelse(isTRUE(directed == FALSE) == TRUE, net <- (net + 
            t(net)), NA)
        ifelse(isTRUE(diag == FALSE) == TRUE, diag(net) <- 0L, 
            NA)
        ifelse(isTRUE(dichot == TRUE) == TRUE, return(dichot(net, 
            c = 1L)), return(net))
    }
    else {
        mat <- matrix(0L, nrow = dim(net)[1], ncol = dim(net)[2])
        for (k in seq_len(dim(net)[3])) mat <- mat + net[, , 
            k]
        ifelse(isTRUE(directed == TRUE) == TRUE, NA, mat <- mat + 
            t(mat))
        ifelse(isTRUE(dichot == TRUE) == TRUE, mat <- dichot(mat, 
            c = 1L), NA)
        ifelse(isTRUE(diag == FALSE) == TRUE, diag(mat) <- 0L, 
            NA)
        mat
    }
}
