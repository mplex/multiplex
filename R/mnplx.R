mnplx <-
function (net, directed = TRUE, dichot = TRUE, diag.incl = FALSE) 
{
    if ((is.na(dim(net)[3]) == TRUE | isTRUE(dim(net)[3] == 1L) == 
        TRUE)) {
        ifelse(isTRUE(dim(net)[3] == 1) == TRUE, net <- net[, 
            , 1], NA)
        ifelse(isTRUE(diag.incl == TRUE) == TRUE, NA, diag(net) <- 0L)
        ifelse(isTRUE(dichot == TRUE) == TRUE, net <- dichot(net, 
            c = 1L), NA)
        ifelse(isTRUE(directed == FALSE) == TRUE, return(net + 
            t(net)), return(net))
    }
    else {
        mat <- matrix(0L, nrow = dim(net)[1], ncol = dim(net)[2])
        for (k in 1:dim(net)[3]) mat <- mat + net[, , k]
        if (isTRUE(directed == TRUE) == TRUE) {
            NA
        }
        else {
            mat <- mat + t(mat)
        }
        if (dichot) {
            mat <- dichot(mat, c = 1L)
        }
        else {
            NA
        }
        if (isTRUE(diag.incl == FALSE) == TRUE) {
            diag(mat) <- 0L
        }
        mat
    }
}
