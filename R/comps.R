## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
comps <-
function (x, bonds = c("entire", "strong", "weak"), sort) 
{
    ifelse(isTRUE(is.null(dimnames(x)[1]) == TRUE | is.null(dimnames(x)[1][[1]]) == 
        TRUE) == TRUE, lbs <- seq_len(nrow(x)), lbs <- dimnames(x)[[1]])
    if (isTRUE(sum(mnplx(x, diag = FALSE)) > 0) == TRUE) {
        bd <- bundles(x, lb2lb = FALSE, collapse = TRUE)
        switch(match.arg(bonds), entire = lbd <- bd, strong = lbd <- list(bd$recp, 
            bd$txch, bd$mixd, bd$full), weak = lbd <- list(bd$asym, 
            bd$tent))
        if (isTRUE(length(unlist(lbd)) == 0) == TRUE) {
            com <- dimnames(rm.isol(x))[[1]]
        }
        else {
            tx <- transl(unlist(lbd))
            while (length(tx) > length(transl(tx))) {
                tx <- transl(tx)
            }
            if (missing(sort) == FALSE && isTRUE(sort == TRUE) == 
                TRUE) {
                tx <- tx[order(unlist(lapply(as.list(tx), function(z) {
                  length(dhc(z))
                })))]
            }
            else {
                NA
            }
            com <- vector(mode = "list", length = length(tx))
            for (i in seq_len(length(tx))) {
                com[[i]] <- lbs[as.numeric(dhc(tx[i]))]
            }
            rm(i)
        }
        lst <- list(com = com, isol = lbs[which(!(lbs %in% unlist(com)))])
    }
    else {
        lst <- list(com = character(0), isol = lbs)
    }
    class(lst) <- c(class(lst), match.arg(bonds))
    return(lst)
}

