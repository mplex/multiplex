pi.rels <-
function (x, po, po.incl = FALSE) 
{
    if (isTRUE(attr(x, "class")[1] == "Pacnet") == FALSE) 
        stop("\"x\" should be an object of a \"Pacnet\" class.")
    if (isTRUE(attr(x, "class")[1] == "Pacnet") == TRUE) {
        ifelse(isTRUE(attr(x, "class")[length(attr(x, "class"))] == 
            "transp") == TRUE, Po <- (po), Po <- t(po))
    }
    pii <- array(dim = c(nrow(po), ncol(po), length(x$ii)))
    for (i in seq_len(length(x$ii))) {
        pii[, , i] <- transf(x$ii[[i]], type = "toarray", ord = nrow(po), 
            lbs = seq_len(nrow(po))) + Po
    }
    rm(i)
    pat <- array(dim = c(nrow(po), ncol(po), length(x$at)))
    for (i in seq_len(length(x$at))) {
        pat[, , i] <- transf(x$at[[i]], type = "toarray", ord = nrow(po), 
            lbs = seq_len(nrow(po))) + Po
    }
    rm(i)
    pmc <- array(dim = c(nrow(po), ncol(po), dim(x$mc)[3]))
    for (i in seq_len(dim(x$mc)[3])) {
        pmc[, , i] <- x$mc[, , i]
    }
    rm(i)
    piis <- zbnd(pii, zbnd(pat, pmc))
    tmp <- data.frame(matrix(ncol = (nrow(po) * ncol(po)), nrow = 0))
    for (i in seq_len(dim(piis)[3])) {
        ifelse(isTRUE(dim(pii)[3] > 1) == TRUE, tmp[i, ] <- as.vector(piis[, 
            , i]), tmp <- as.vector(piis))
    }
    rm(i)
    tmpu <- unique(tmp)
    pisu <- array(1L, dim = c(dim(piis)[1], dim(piis)[2], nrow(tmpu)))
    if (isTRUE(nrow(tmpu) > 0L) == TRUE) {
        for (i in seq_len(nrow(tmpu))) {
            pisu[, , i][seq_len((dim(piis)[1] * dim(piis)[2]))] <- as.numeric(tmpu[i, 
                ])
        }
        rm(i)
    }
    dimnames(pisu)[[1]] <- dimnames(pisu)[[2]] <- as.list(seq_len(dim(po)[1]))
    ifelse(isTRUE(po.incl == FALSE) == TRUE, lst <- list(pi = pisu, 
        mc = x$mc), lst <- list(pi = pisu, mc = x$mc, po = po))
    class(lst) <- "Pi.rels"
    lst
}
