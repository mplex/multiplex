cngr <-
function (S, PO = NULL, uniq) 
{
    if (isTRUE(attr(S, "class")[1] == "Semigroup") == FALSE || 
        isTRUE(attr(S, "class")[2] == "symbolic") == TRUE) {
        s <- as.semigroup(S, numerical = TRUE)
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
            for (i in seq_len(nrow(cls))) cg[[i]] <- as.vector(cls[i, 
                ])
        }
        else {
            cg <- rep(1, s$dim)
        }
        ifelse(isTRUE(is.null(PO)) == FALSE, lst <- list(S = S$S, 
            PO = PO, clu = cg), lst <- list(S = S$S, clu = cg))
        ifelse(isTRUE(is.null(PO)) == FALSE, class(lst) <- c("Congruence", 
            "PO.Semigroup", attr(S, "class")[2]), class(lst) <- c("Congruence", 
            "A.Semigroup", attr(S, "class")[2]))
        return(lst)
    }
    else {
        S
    }
}
