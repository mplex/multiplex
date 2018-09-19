decomp <-
function (S, pr, type = c("mc", "pi", "at", "cc"), reduc, fac) 
{
    if (isTRUE(attr(pr, "class") == "Pi.rels" || attr(pr, "class")[1] == 
        "Congruence") == FALSE) 
        stop("\"pr\" should be an object either of a \"Pi.rels\" or a \"Congruence\" class.")
    if (missing(fac) == FALSE && isTRUE(attr(pr, "class") == 
        "Pi.rels") == FALSE) 
        warning("'fac' is ignored since it requires a \"Pi.rels\" class object as inpu.")
    if (isTRUE(attr(pr, "class") == "Pi.rels") == TRUE) {
        if (missing(fac) == FALSE && isTRUE("Decomp" %in% attr(S, 
            "class")) == TRUE) {
            if (is.numeric(fac) == FALSE || isTRUE(fac > length(S$IM)) == 
                TRUE) {
                warning("'fac' must be an integer no larger than the number of factors in the input. First factor is taken.")
                S <- S$IM[[1]]
            }
            else {
                S <- S$IM[[fac]]
            }
        }
        else {
            NA
        }
        switch(match.arg(type), mc = poi <- pr$mc, pi = poi <- pr$pi, 
            at = poi <- pr$at, cc = stop("Type options for a \"Pi.rels\" class object should be either \"pi\", \"at\" or \"mc\""))
    }
    else if (isTRUE(attr(pr, "class")[1] == "Congruence") == 
        TRUE) {
        if (match.arg(type) != "cc") {
            warning("Other type options than \"cc\" are not suitable for the \"Congruence\" class object")
        }
        ifelse(isTRUE(attr(pr, "class")[2] == "PO.Semigroup") == 
            TRUE, poi <- pr$PO, NA)
    }
    if (isTRUE("symbolic" %in% attr(S, "class")) == TRUE || isTRUE("Semigroup" %in% 
        attr(S, "class")) == FALSE) {
        s <- as.semigroup(S, numerical = TRUE)
    }
    else {
        s <- S
    }
    if (isTRUE(attr(pr, "class") == "Pi.rels") == TRUE) {
        if (is.list(poi) == TRUE) {
            tmp <- poi
            poi <- array(NA, dim = c(dim(tmp[[1]]), length(tmp)), 
                dimnames = dimnames(tmp[[1]]))
            for (k in seq_along(tmp)) {
                poi[, , k] <- tmp[[k]]
            }
            rm(k)
            rm(tmp)
        }
        else {
            NA
        }
        if (is.na(dim(poi)[3]) == FALSE) {
            clu <- list()
            lb <- list()
            length(lb) <- length(clu) <- dim(poi)[3]
            for (i in seq_len(dim(poi)[3])) {
                ifelse(isTRUE(all(as.vector(poi[, , i]) == 1L) == 
                  TRUE) == TRUE, clu[[i]] <- rep(1, s$ord), clu[[i]] <- stats::cutree(stats::hclust(stats::dist(poi[, 
                  , i])), k = length(cut(stats::as.dendrogram(stats::hclust(stats::dist(poi[, 
                  , i]))), h = 0L)$lower)))
                ifelse(isTRUE("symbolic" %in% attr(S, "class")) == 
                  TRUE, attr(clu[[i]], "names") <- S$st, attr(clu[[i]], 
                  "names") <- s$st)
                lb[[i]] <- list()
                for (j in seq_along(tabulate(clu[[i]]))) {
                  lb[[i]][[j]] <- noquote(attr(which(clu[[i]] == 
                    j), "names"))
                }
                rm(j)
            }
            rm(i)
        }
        else {
            clu <- seq_len(S$ord)
            lb <- S$st
        }
    }
    else {
        clu <- pr$clu
        if (is.list(clu) == FALSE && isTRUE(nlevels(factor(clu)) == 
            1) == TRUE) {
            lb <- S$st
        }
        else {
            lb <- list()
            length(lb) <- length(clu)
            for (i in seq_along(clu)) {
                ifelse(isTRUE("symbolic" %in% attr(S, "class")) == 
                  TRUE, attr(clu[[i]], "names") <- S$st, attr(clu[[i]], 
                  "names") <- rownames(S))
                lb[[i]] <- list()
                for (j in seq_along(tabulate(clu[[i]]))) {
                  lb[[i]][[j]] <- noquote(attr(which(clu[[i]] == 
                    j), "names"))
                }
                rm(j)
            }
            rm(i)
        }
    }
    if (missing(reduc) == FALSE && isTRUE(reduc == TRUE) == TRUE) {
        if (isTRUE(nlevels(factor(as.matrix(s$S))) == 1) == TRUE) {
            im <- S$S
            ord <- S$ord
        }
        else {
            im <- list()
            po <- list()
            length(po) <- length(im) <- length(clu)
            ord <- vector()
            for (i in seq_along(clu)) {
                if (isTRUE("symbolic" %in% attr(S, "class")) == 
                  TRUE) {
                  im[[i]] <- reducs(S, cl = as.vector(clu[[i]]))
                }
                else {
                  im[[i]] <- reducs(as.semigroup(S), cl = as.vector(clu[[i]]))
                }
                if (isTRUE(attr(pr, "class") == "Pi.rels") == 
                  TRUE) {
                  po[[i]] <- reduc(poi[, , i], clu = as.vector(clu[[i]]), 
                    lbs = dimnames(im[[i]])[[1]])
                }
                else if (isTRUE(attr(pr, "class")[2] == "PO.Semigroup") == 
                  TRUE) {
                  po[[i]] <- reduc(poi, clu = as.vector(clu[[i]]), 
                    lbs = dimnames(im[[i]])[[1]])
                }
                else {
                  NA
                }
                ord <- append(ord, dim(im[[i]])[1])
            }
            rm(i)
        }
        ifelse(isTRUE(attr(pr, "class") == "Pi.rels" || attr(pr, 
            "class")[2] == "PO.Semigroup") == TRUE, lst <- list(clu = clu, 
            eq = lb, IM = im, PO = po, ord = ord), lst <- list(clu = clu, 
            eq = lb, IM = im, ord = ord))
    }
    else {
        lst <- list(clu = clu, eq = lb)
    }
    ifelse(isTRUE(attr(pr, "class")[1] == "Congruence") == TRUE, 
        class(lst) <- c("Decomp", attr(pr, "class")[1], "cc"), 
        class(lst) <- c("Decomp", attr(pr, "class")[1], match.arg(type)))
    return(lst)
}
