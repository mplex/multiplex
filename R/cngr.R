cngr <-
function (S, PO = NULL, uniq) 
{
    flgnum <- FALSE
    if (isTRUE("Semigroup" %in% attr(S, "class")) == FALSE) {
        s <- semigroup(S, type = "numerical")
    }
    else if (isTRUE("symbolic" %in% attr(S, "class")) == TRUE) {
        s <- as.semigroup(S, numerical = TRUE)
        flgnum <- TRUE
    }
    else {
        s <- S
    }
    ns <- s$ord
    if (isTRUE(ns > 1L) == TRUE) {
        mat <- matrix(0L, nrow = ns, ncol = ns, dimnames = list(seq_len(ns), 
            seq_len(ns)))
        for (i in seq_len(ns)) {
            mat[which(s$S == i)[1]] <- i
        }
        rm(i)
        inc <- levels(factor(trnf(dichot(mat, c = 1), tolist = TRUE, 
            sep = ", ")))
        clus <- data.frame(matrix(ncol = ns, nrow = 0))
        if (isTRUE(nlevels(factor(as.matrix(s$S))) != 1) == TRUE) {
            for (k in as.vector(mat)[which(as.vector(mat) > 0)]) {
                for (i in seq_along(inc)) {
                  clus[i, ] <- as.vector(sprt(s$S, as.numeric(dhc(inc[i])[1]), 
                    as.numeric(dhc(inc[i])[2])))
                }
                rm(i)
            }
            rm(k)
            ifelse(missing(uniq) == FALSE && isTRUE(uniq == TRUE) == 
                TRUE, clus <- unique(clus), NA)
            colnames(clus) <- rownames(clus) <- NULL
            cls <- data.matrix(clus)
            cg <- list()
            for (i in seq_len(nrow(cls))) {
                cg[[i]] <- as.vector(cls[i, ])
                ifelse(isTRUE(flgnum == TRUE) == TRUE, attr(cg[[i]], 
                  "names") <- S$st, attr(cg[[i]], "names") <- dimnames(S)[[1]])
            }
            rm(i)
        }
        else {
            cg <- rep(1, s$dim)
        }
        ifelse(isTRUE(flgnum == TRUE) == TRUE, sS <- S$S, sS <- s$S)
        ifelse(isTRUE(is.null(PO)) == FALSE, lst <- list(S = sS, 
            PO = PO, clu = cg), lst <- list(S = sS, clu = cg))
        ifelse(isTRUE(flgnum == TRUE) == TRUE, Sclss <- "symbolic", 
            Sclss <- attr(s, "class")[2])
        ifelse(isTRUE(is.null(PO)) == FALSE, class(lst) <- c("Congruence", 
            "PO.Semigroup", Sclss), class(lst) <- c("Congruence", 
            "A.Semigroup", Sclss))
        return(lst)
    }
    else {
        s
    }
}
