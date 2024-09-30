pi.rels <-
function (x, po.incl, vc, po) 
{
    if (isTRUE(attr(x, "class") == "Ind.incl") == FALSE && isTRUE(attr(x, 
        "class")[1] == "Pacnet") == FALSE) 
        stop("\"x\" should be an \"Ind.incl\" or \"Pacnet\" object class.")
    po <- x$po
    Po <- po[seq_len(nrow(po)), seq_len(ncol(po))]
    if (isTRUE(attr(x, "class") == "Ind.incl") == TRUE) {
        if (missing(vc) == FALSE) {
            pii <- array(dim = c(nrow(po), ncol(po), length(vc)))
            k <- 1L
            for (i in vc) {
                pii[, , k] <- transf(x$iin[[i]], type = "toarray", 
                  ord = nrow(po), lbs = seq_len(nrow(po))) + 
                  Po
                k <- k + 1L
            }
            rm(i)
            dimnames(pii)[[3]] <- attr(x$iin, "names")[vc]
        }
        else {
            pii <- array(dim = c(nrow(po), ncol(po), length(x$iin)))
            for (i in seq_along(x$iin)) {
                pii[, , i] <- transf(x$iin[[i]], type = "toarray", 
                  ord = nrow(po), lbs = seq_len(nrow(po))) + 
                  Po
            }
            rm(i)
            dimnames(pii)[[3]] <- as.list(attr(x$iin, "names"))
        }
        pat <- array(dim = c(nrow(po), ncol(po), length(x$atm)))
        for (i in seq_along(x$atm)) {
            pat[, , i] <- transf(x$atm[[i]], type = "toarray", 
                ord = nrow(po), lbs = seq_len(nrow(po))) + Po
        }
        rm(i)
        dimnames(pat)[[1]] <- dimnames(pat)[[2]] <- as.list(seq_len(dim(po)[1]))
        if (isTRUE(length(unlist(x$mca)) != 0) == TRUE) {
            pmc <- array(dim = c(nrow(po), ncol(po), length(x$mca)))
            for (i in seq_len(length(x$mca))) {
                pmc[, , i] <- x$mca[[i]]
            }
            rm(i)
            piis <- zbnd(dichot(pii), pmc)
            xmc <- x$mca
        }
        else {
            piis <- dichot(pii)
            warning("There is no meet-complement of the atom in \"x\", then use atom itself.")
            xmc <- pat
        }
    }
    if (isTRUE(attr(x, "class")[1] == "Pacnet") == TRUE) {
        if (missing(po) == TRUE) 
            stop("The partial order in \"po\" is required for the Pacnet option.")
        ifelse(isTRUE(attr(x, "class")[length(attr(x, "class"))] == 
            "transp") == TRUE, Po <- (po), Po <- t(po))
        pii <- array(dim = c(nrow(po), ncol(po), length(x$ii)))
        for (i in seq_along(x$ii)) {
            pii[, , i] <- transf(x$ii[[i]], type = "toarray", 
                ord = nrow(po), lbs = seq_len(nrow(po))) + Po
        }
        rm(i)
        pat <- array(dim = c(nrow(po), ncol(po), length(x$atm)))
        for (i in seq_along(x$atm)) {
            pat[, , i] <- transf(x$atm[[i]], type = "toarray", 
                ord = nrow(po), lbs = seq_len(nrow(po))) + Po
        }
        rm(i)
        dimnames(pat)[[1]] <- dimnames(pat)[[2]] <- as.list(seq_len(dim(po)[1]))
        pmc <- array(dim = c(nrow(po), ncol(po), dim(x$mca)[3]))
        for (i in seq_len(dim(x$mca)[3])) {
            pmc[, , i] <- x$mca[, , i]
        }
        rm(i)
        piis <- zbnd(pii, zbnd(pat, pmc))
    }
    tmp <- data.frame(matrix(ncol = (nrow(po) * ncol(po)), nrow = 0))
    for (i in seq_len(dim(piis)[3])) {
        ifelse(isTRUE(dim(pii)[3] > 1) == TRUE, tmp[i, ] <- as.vector(piis[, 
            , i]), tmp <- as.vector(piis))
    }
    rm(i)
    if (missing(vc) == FALSE || is.vector(tmp) == TRUE) {
        tmpu <- tmp
    }
    else {
        tmpu <- unique(tmp)
    }
    if (isTRUE(nrow(tmpu) > 0L) == TRUE) {
        pisu <- array(1L, dim = c(dim(piis)[1], dim(piis)[2], 
            nrow(tmpu)))
        for (i in seq_len(nrow(tmpu))) {
            pisu[, , i][seq_len((dim(piis)[1] * dim(piis)[2]))] <- as.numeric(tmpu[i, 
                ])
        }
        rm(i)
        dimnames(pisu)[[3]] <- c(dimnames(pii)[[3]], seq(from = dim(pii)[3] + 
            1, to = dim(pisu)[3]))[which(duplicated(tmp) == FALSE)]
    }
    else if (isTRUE(nrow(tmpu) > 0) == FALSE) {
        pisu <- piis
    }
    dimnames(pisu)[[1]] <- dimnames(pisu)[[2]] <- as.list(seq_len(dim(po)[1]))
    if (missing(po.incl) == FALSE && isTRUE(po.incl == TRUE) == 
        TRUE) {
        lst <- list(pi = pisu, at = pat, mca = xmc, po = Po)
    }
    else {
        lst <- list(pi = pisu, at = pat, mca = xmc)
    }
    class(lst) <- "Pi.rels"
    lst
}
