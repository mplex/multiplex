comps <-
function (x, bonds = c("entire", "strong", "weak")) 
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
            com <- list()
            for (i in 1:length(tx)) {
                com[[i]] <- lbs[as.numeric(dhc(tx[i]))]
            }
            rm(i)
        }
        return(list(com = com, isol = lbs[which(!(lbs %in% unlist(com)))]))
    }
    else {
        return(list(com = NULL, isol = lbs))
    }
}
