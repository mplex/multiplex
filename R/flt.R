## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
flt <-
function (e, PO, rclos = TRUE) 
{
    if (isTRUE(e > nrow(PO)) == TRUE) 
        stop(paste(deparse(substitute(e)), " is greater than size of partial order in ", 
            deparse(substitute(PO)), ".", sep = ""))
    pfl <- vector()
    for (i in seq_len(nrow(PO))) {
        if (isTRUE(PO[e, i] == 1) == TRUE && isTRUE(PO[i, e] == 
            0) == TRUE) {
            pfl <- append(pfl, i)
        }
        else {
            NA
        }
    }
    rm(i)
    if (rclos) {
        pfl <- append(pfl, e)
    }
    return(pfl)
}

