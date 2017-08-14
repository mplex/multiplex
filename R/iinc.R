iinc <-
function (inc, PO, equat = FALSE, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    ifelse(is.null(dimnames(PO)[[1]]) == TRUE, dimnames(PO)[[1]] <- dimnames(PO)[[2]] <- seq(dim(PO)[1]), 
        NA)
    lbs <- dimnames(PO)[[1]]
    INC <- transf(inc, type = "toarray", ord = dim(PO)[1], lb2lb = TRUE, 
        sep = sep, lbs = lbs)
    dimnames(INC)[[1]][which(is.na(dimnames(INC)[[1]]))] <- dimnames(INC)[[2]][which(is.na(dimnames(INC)[[1]]))] <- lbs[which(!(lbs %in% 
        dimnames(INC)[[1]]))]
    Pi <- mnplx(zbind(INC, PO))
    ls <- list()
    k <- 1
    for (i in 1:(nrow(Pi) - 1)) {
        for (j in (i + 1):nrow(Pi)) {
            if (isTRUE(all.equal(Pi[i, ], Pi[j, ])) == TRUE) {
                if (i != j) {
                  ls[[k]] <- c(i, j)
                  k <- k + 1
                  if (isTRUE(equat == TRUE) == TRUE) 
                    print(paste(lbs[i], lbs[j], sep = " = "))
                }
            }
        }
        rm(j)
    }
    rm(i)
    if (length(ls) == 0) 
        return(seq_len(dim(PO)[1]))
    if (sum(trnf(inc, tolist = FALSE, ord = dim(PO)[1], lb2lb = TRUE), 
        PO) == dim(PO)[1] * dim(PO)[2]) 
        return(rep(1, each = dim(PO)[1]))
    j <- 1L
    attr(ls[[1]], "names")[1] <- attr(ls[[1]], "names")[2] <- j
    if (length(ls) > 1L) {
        for (i in (j + 1L):length(ls)) {
            if (any(ls[[i]] %in% ls[[j]]) | any(ls[[j]] %in% 
                ls[[i]])) 
                attr(ls[[i]], "names")[1] <- attr(ls[[i]], "names")[2] <- j
        }
        rm(i)
        for (j in seq.int(2, length(ls))) {
            if (isTRUE(is.null(attr(ls[[j]], "names")) == TRUE) == 
                TRUE) {
                for (i in seq_len((j - 1L))) {
                  ifelse((any(ls[[i]] %in% ls[[j]]) | any(ls[[j]] %in% 
                    ls[[i]])), attr(ls[[j]], "names")[1] <- attr(ls[[j]], 
                    "names")[2] <- as.integer(attr(ls[[i]], "names")[1]), 
                    NA)
                }
                rm(i)
            }
        }
        rm(j)
        for (j in seq.int(2, length(ls))) if (isTRUE(is.null(attr(ls[[j]], 
            "names")) == TRUE) == TRUE) {
            for (i in j:length(ls)) {
                if (any(ls[[i]] %in% ls[[j]])) 
                  attr(ls[[i]], "names")[1] <- attr(ls[[i]], 
                    "names")[2] <- j
            }
            rm(i)
            if (j < length(ls)) {
                for (k in (j + 1L):length(ls)) {
                  if (is.null(attr(ls[[k]], "names")) == TRUE) {
                    for (i in seq_len(length(ls))) {
                      if (is.null(attr(ls[[i]], "names")) == 
                        FALSE) {
                        if (any(ls[[k]] %in% ls[[i]])) 
                          attr(ls[[k]], "names")[1] <- attr(ls[[k]], 
                            "names")[2] <- as.numeric(attr(ls[[i]], 
                            "names")[1])
                      }
                    }
                    rm(i)
                  }
                }
                rm(k)
            }
        }
        rm(j)
    }
    clu <- vector()
    for (i in seq_len(length(ls))) {
        clu[i] <- as.numeric(attr(ls[[i]], "names")[1])
    }
    rm(i)
    f <- nlevels(factor(clu))
    x <- clu
    cls <- vector()
    length(cls) <- length(clu)
    for (i in seq_len(f)) {
        cls[which(x == as.numeric(levels(factor(clu)))[i])] <- i
    }
    rm(i)
    nls <- ls
    for (i in seq_len(length(nls))) {
        attr(nls[[i]], "names")[1] <- attr(nls[[i]], "names")[2] <- cls[i]
    }
    rm(i)
    cl <- integer()
    length(cl) <- dim(PO)[1]
    for (k in seq_len(f)) {
        for (i in seq_len(length(nls))) {
            if (attr(nls[[i]], "names")[1] == as.character(k)) {
                cl[nls[[i]][1]] <- k
                cl[nls[[i]][2]] <- k
            }
        }
        rm(i)
    }
    rm(k)
    n <- as.numeric(attr(stats::na.omit(cl), "na.action"))
    for (i in seq_len(length(n))) {
        cl[n[i]] <- f + i
    }
    rm(i)
    rm(f)
    return(cl)
}
