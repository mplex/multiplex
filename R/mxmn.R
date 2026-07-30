## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
mxmn <-
function (x, type = c("numerical", "symbolic"), cmps, equat) 
{
    ifelse(missing(cmps) == FALSE && isTRUE(cmps == TRUE) == 
        TRUE, cmps <- TRUE, cmps <- FALSE)
    ifelse(missing(equat) == FALSE && isTRUE(equat == TRUE) == 
        TRUE, equat <- TRUE, equat <- FALSE)
    n <- dim(x)[1]
    z <- dim(x)[3]
    Bx <- x
    cplb <- vector()
    for (k in seq_len(z)) {
        for (l in seq_len(z)) {
            mat <- matrix(NA, nrow = n, ncol = n, byrow = TRUE, 
                dimnames = list(dimnames(x)[[1]], dimnames(x)[[2]]))
            for (i in seq_len(n)) {
                for (j in seq_len(n)) {
                  tmp1 <- x[i, , k]
                  tmp2 <- x[, j, l]
                  mintmp <- vector()
                  for (m in seq_len(length(tmp1))) {
                    mintmp <- append(mintmp, min(tmp1[m], tmp2[m]))
                  }
                  rm(m)
                  mat[i, j] <- max(mintmp)
                }
                rm(j)
            }
            rm(i)
            if (isTRUE(dim(Bx)[3] == z) == TRUE) {
                Bx <- zbind(x, mat)
                cplb <- c(dimnames(x)[[3]], paste0(dimnames(x)[[3]][k], 
                  dimnames(x)[[3]][l]))
            }
            else {
                Bx <- zbind(Bx, mat)
                cplb <- append(cplb, paste0(dimnames(x)[[3]][k], 
                  dimnames(x)[[3]][l]))
            }
        }
        rm(l)
    }
    rm(k)
    dimnames(Bx)[[3]] <- cplb
    dfBx <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
        nrow = 0))
    for (i in seq_len(dim(Bx)[3])) {
        dfBx[i, ] <- as.vector(Bx[, , i])
    }
    rm(i)
    cplb2 <- vector()
    if (any(duplicated(dfBx)) == TRUE) {
        eqlb <- jnt(cplb[as.numeric(rownames(dfBx[duplicated(dfBx) | 
            duplicated(dfBx, fromLast = TRUE), ]))], sep = "=")
        rownames(dfBx) <- cplb
        dfBx <- unique(dfBx)
        cplb2 <- cplb[which(!(cplb %in% rownames(dfBx)))]
        cplb <- rownames(dfBx)
        Bx <- array(dim = c(dim(x)[1], dim(x)[2], nrow(dfBx)))
        for (i in seq_len(nrow(dfBx))) {
            Bx[, , i][seq_len(dim(Bx)[1] * dim(Bx)[2])] <- as.numeric(dfBx[i, 
                ])
        }
        rm(i)
        dimnames(Bx)[[1]] <- dimnames(Bx)[[2]] <- dimnames(x)[[1]]
        dimnames(Bx)[[3]] <- cplb
    }
    else {
        eqlb <- vector()
    }
    z2 <- dim(Bx)[3]
    E <- data.frame(matrix(ncol = z, nrow = z2))
    for (k in seq_len(z)) {
        for (j in seq_len(z2)) {
            tmp <- mmp(Bx[, , j], Bx[, , k])
            for (i in z2:1) {
                if (isTRUE(all.equal(tmp, Bx[, , i]) == TRUE)) 
                  E[j, k] <- i
            }
            rm(i)
        }
        rm(j)
    }
    rm(k)
    dimnames(E)[[1]] <- cplb
    dimnames(E)[[2]] <- dimnames(x)[[3]]
    while (any(is.na(E)) == TRUE) {
        ei <- which(is.na(E), arr.ind = TRUE)
        for (k in seq_len(nrow(ei))) {
            Bx <- zbind(Bx, mmp(Bx[, , which(cplb == rownames(ei)[k])], 
                Bx[, , which(cplb == cplb[ei[k, 2]])]))
            cplb <- append(cplb, paste0(rownames(ei)[k], cplb[ei[k, 
                2]]))
        }
        rm(k)
        dimnames(Bx)[[3]] <- cplb
        dfBx <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
            nrow = 0))
        for (i in seq_len(dim(Bx)[3])) {
            dfBx[i, ] <- as.vector(Bx[, , i])
        }
        rm(i)
        if (any(duplicated(dfBx)) == TRUE) {
            eqlb <- append(eqlb, jnt(cplb[as.numeric(rownames(dfBx[duplicated(dfBx) | 
                duplicated(dfBx, fromLast = TRUE), ]))], sep = "="))
            rownames(dfBx) <- cplb
            dfBx <- unique(dfBx)
            cplb <- rownames(dfBx)
            Bx <- array(dim = c(dim(x)[1], dim(x)[2], nrow(dfBx)))
            for (i in seq_len(nrow(dfBx))) {
                Bx[, , i][seq_len(dim(Bx)[1] * dim(Bx)[2])] <- as.numeric(dfBx[i, 
                  ])
            }
            rm(i)
            dimnames(Bx)[[1]] <- dimnames(Bx)[[2]] <- dimnames(x)[[1]]
            dimnames(Bx)[[3]] <- cplb
        }
        else {
            NA
        }
        z2 <- dim(Bx)[3]
        E <- data.frame(matrix(ncol = z, nrow = z2))
        for (k in seq_len(z)) {
            for (j in seq_len(z2)) {
                tmp <- mmp(Bx[, , j], Bx[, , k])
                for (i in z2:1) {
                  if (isTRUE(all.equal(tmp, Bx[, , i]) == TRUE)) 
                    E[j, k] <- i
                }
                rm(i)
            }
            rm(j)
        }
        rm(k)
        dimnames(E)[[1]] <- cplb
        dimnames(E)[[2]] <- dimnames(x)[[3]]
    }
    switch(match.arg(type), numerical = S <- cbind(E, data.frame(matrix(ncol = (nrow(E) - 
        ncol(E)), nrow = nrow(E)))), symbolic = {
        e <- E
        for (i in seq_len(nrow(E))) {
            for (j in seq_len(ncol(E))) {
                e[i, j] <- dimnames(Bx)[[3]][as.matrix(E[i, j])]
            }
            rm(j)
        }
        rm(i)
        S <- cbind(as.data.frame(e), data.frame(matrix(ncol = (nrow(E) - 
            ncol(E)), nrow = nrow(E))))
        rm(e)
    })
    if (ncol(S) != ncol(E)) {
        for (j in (ncol(E) + 1L):nrow(E)) {
            for (i in seq_len(nrow(E))) {
                tmp <- mmp(Bx[, , i], mmp(Bx[, , as.numeric(which(E == 
                  j, arr.ind = TRUE)[1, ][1])], Bx[, , as.numeric(which(E == 
                  j, arr.ind = TRUE)[1, ][2])]))
                for (k in seq_len(dim(Bx)[3])) {
                  if (isTRUE(all.equal(Bx[, , k], tmp)) == TRUE) {
                    switch(match.arg(type), numerical = S[i, 
                      j] <- k, symbolic = S[i, j] <- dimnames(Bx)[[3]][k])
                    break
                  }
                }
                rm(k)
            }
            rm(i)
        }
        rm(j)
        rm(tmp)
    }
    else {
        NA
    }
    if (match.arg(type) == "symbolic") {
        colnames(S) <- rownames(S) <- dimnames(Bx)[[3]]
    }
    else {
        colnames(S) <- rownames(S) <- seq_len(nrow(S))
    }
    if (equat == TRUE) {
        lst <- list(dim = dim(x)[1], gens = x, cmps = Bx[, , 
            (z + 1):dim(Bx)[3]], equat = eqlb, ord = nrow(S), 
            st = dimnames(Bx)[[3]], S = S)
    }
    else {
        lst <- list(dim = dim(x)[1], gens = x, cmps = Bx[, , 
            (z + 1):dim(Bx)[3]], ord = nrow(S), st = dimnames(Bx)[[3]], 
            S = S)
    }
    if (isTRUE(cmps == FALSE) == TRUE) {
        lst$cmps <- NULL
    }
    else {
        NA
    }
    class(lst) <- c("Semigroup", match.arg(type), "valued")
    return(lst)
}

