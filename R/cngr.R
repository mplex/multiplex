cngr <-
function (S, PO = NULL, unique = FALSE) 
{
    if (isTRUE(attr(S, "class")[1] == "Semigroup") == FALSE) 
        stop("\"S\" should be an object of a \"Semigroup\" class.")
    s <- S
    if (isTRUE(attr(S, "class")[2] == "symbolic") == TRUE) {
        s <- convert(s, SemigroupClass = TRUE)
    }
    else {
        NA
    }
    if (isTRUE(s$ord == 1L) == FALSE) {
        mat <- matrix(0L, nrow = s$ord, ncol = s$ord)
        for (i in seq_len(s$ord)) {
            mat[which(s$S == i)[1]] <- i
        }
        rm(i)
        inc <- levels(factor(trnf(dichot(mat, c = 1), tolist = TRUE, 
            sep = ", ")))
        clus <- data.frame(matrix(ncol = s$ord, nrow = 0))
        for (i in seq_len(length(inc))) {
            clus[i, ] <- as.vector(sprt(s$S, as.numeric(strsplit(inc[i], 
                ", ")[[1]][1]), as.numeric(strsplit(inc[i], ", ")[[1]][2])))
        }
        rm(i)
        if (isTRUE(unique == TRUE) == TRUE) {
            cls <- unique(clus)
        }
        else {
            cls <- clus
        }
        colnames(cls) <- rownames(cls) <- NULL
        cls <- data.matrix(cls)
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
