decomp <-
function (S, pr, type = c("mca", "pi", "at", "cc"), reduc, fac, 
    force) 
{
    if (isTRUE("Pi.rels" %in% attr(pr, "class") || "Congruence" %in% 
        attr(pr, "class")) == FALSE) 
        stop("\"pr\" must be either a \"Pi.rels\" or a \"Congruence\" class object.")
    if (missing(fac) == FALSE && isTRUE("Pi.rels" %in% attr(pr, 
        "class")) == FALSE) 
        warning("\"fac\" is ignored since it requires a \"Pi.rels\" class object as input.")
    if (isTRUE("Pi.rels" %in% attr(pr, "class")) == TRUE) {
        if (missing(fac) == FALSE && isTRUE("Decomp" %in% attr(S, 
            "class")) == TRUE) {
            if (is.numeric(fac) == FALSE || isTRUE(fac > length(S$IM)) == 
                TRUE) {
                warning("First factor is taken since \"fac\" must be an integer no larger than the number of factors in the input.")
                S <- S$IM[[1]]
            }
            else {
                S <- S$IM[[fac]]
            }
        }
        else {
            NA
        }
        switch(match.arg(type), mca = poi <- pr$mca, pi = poi <- pr$pi, 
            at = poi <- pr$at, cc = stop("Type options for a \"Pi.rels\" class object must be either \"pi\", \"at\" or \"mc\"."))
    }
    else if (isTRUE(attr(pr, "class")[1] == "Congruence") == 
        TRUE) {
        if (match.arg(type) != "cc") {
            warning("Only option \"cc\" is suitable for a \"Congruence\" class object.")
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
    if (isTRUE("Pi.rels" %in% attr(pr, "class")) == TRUE) {
        if (isTRUE(s$ord == dim(pr$pi)[1]) == FALSE) 
            stop("Semigroup order and partial order dimension differ.")
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
                attr(clu[[i]], "names") <- S$st
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
        ifelse(missing(force) == FALSE && isTRUE(force == TRUE) == 
            TRUE, force <- TRUE, force <- FALSE)
        if (isTRUE(nlevels(factor(as.matrix(s$S))) == 1) == TRUE) {
            im <- S$S
            ord <- S$ord
        }
        else {
            ord <- vector()
            po <- list()
            length(po) <- length(clu)
            im <- list()
            length(im) <- length(clu)
            for (k in seq_along(clu)) {
                tmpimk <- reducs(S, cl = as.vector(clu[[k]]))
                notek <- vector()
                if (isTRUE(force == TRUE) == TRUE && (isTRUE(length(which(is.na(tmpimk))) > 
                  0) == TRUE)) {
                  notek <- append(notek, k)
                  n <- nlevels(factor(clu[[k]]))
                  cls <- list()
                  for (i in seq_len(n)) {
                    cls[[i]] <- which(clu[[k]] == i)
                  }
                  rm(i)
                  jcc <- unique(unlist(S$S[which(clu[[k]] == 
                    which(is.na(tmpimk))[1]%%nrow(tmpimk)), which(clu[[k]] == 
                    ceiling(which(is.na(tmpimk))[1]/ncol(tmpimk)))]))
                  ifelse(isTRUE(attr(S, "class")[2] == "symbolic") == 
                    TRUE, Sst <- S$st, Sst <- s$st)
                  clsna <- vector()
                  for (q in seq_along(jcc)) {
                    for (i in seq_len(length(cls))) {
                      if (isTRUE(jcc[q] %in% Sst[cls[[i]]]) == 
                        TRUE) {
                        clsna <- append(clsna, i)
                        break
                      }
                      else {
                        NA
                      }
                    }
                    rm(i)
                  }
                  rm(q)
                  tmpcl <- clu[[k]]
                  tmpcl[which(tmpcl %in% unique(clsna))] <- min(clsna)
                  for (i in seq_len(nlevels(factor(tmpcl)))) {
                    tmpcl <- replace(tmpcl, tmpcl == as.numeric(levels(factor(tmpcl))[i]), 
                      i)
                  }
                  rm(i)
                  clu[[k]] <- tmpcl
                  lb[[k]] <- list()
                  for (j in seq_along(tabulate(clu[[k]]))) {
                    lb[[k]][[j]] <- noquote(attr(which(clu[[k]] == 
                      j), "names"))
                  }
                  rm(j)
                  im[[k]] <- reducs(S, cl = as.vector(clu[[k]]))
                }
                else {
                  im[[k]] <- tmpimk
                }
                rm(tmpimk)
                if (isTRUE("Pi.rels" %in% attr(pr, "class")) == 
                  TRUE) {
                  po[[k]] <- reduc(poi[, , k], clu = as.vector(clu[[k]]), 
                    lbs = dimnames(im[[k]])[[1]])
                }
                else if (isTRUE(attr(pr, "class")[2] == "PO.Semigroup") == 
                  TRUE) {
                  po[[k]] <- reduc(poi, clu = as.vector(clu[[k]]), 
                    lbs = dimnames(im[[k]])[[1]])
                }
                else {
                  NA
                }
                ord <- append(ord, dim(im[[k]])[1])
            }
            rm(k)
        }
        ifelse(isTRUE("Pi.rels" %in% attr(pr, "class")) == TRUE || 
            isTRUE("PO.Semigroup" %in% attr(pr, "class")) == 
                TRUE, lst <- list(clu = clu, eq = lb, IM = im, 
            PO = po, ord = ord), lst <- list(clu = clu, eq = lb, 
            IM = im, ord = ord))
    }
    else {
        lst <- list(clu = clu, eq = lb)
    }
    ifelse(isTRUE(attr(pr, "class")[1] == "Congruence") == TRUE, 
        class(lst) <- c("Decomp", attr(pr, "class")[1], "cc"), 
        class(lst) <- c("Decomp", attr(pr, "class")[1], match.arg(type)))
    return(lst)
}
