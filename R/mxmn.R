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
            Bx <- zbind(Bx, mat)
            dimnames(Bx)[[3]][dim(Bx)[3]] <- paste0(dimnames(Bx)[[3]][k], 
                dimnames(Bx)[[3]][l])
        }
        rm(l)
    }
    rm(k)
    Bx
    dfBx <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
        nrow = 0))
    for (i in seq_len(dim(Bx)[3])) {
        dfBx[i, ] <- as.vector(Bx[, , i])
    }
    rm(i)
    rownames(dfBx) <- dimnames(Bx)[[3]]
    dfBxx <- dfBx
    dfBx <- unique(dfBx)
    Bx <- array(dim = c(dim(x)[1], dim(x)[2], nrow(dfBx)))
    for (i in seq_len(nrow(dfBx))) {
        Bx[, , i][seq_len(dim(Bx)[1] * dim(Bx)[2])] <- as.numeric(dfBx[i, 
            ])
    }
    rm(i)
    dimnames(Bx)[[1]] <- dimnames(Bx)[[2]] <- dimnames(x)[[1]]
    dimnames(Bx)[[3]] <- rownames(dfBx)
    Bx
    E <- data.frame(matrix(ncol = dim(x)[3], nrow = dim(Bx)[3]))
    for (k in seq_len(dim(x)[3])) {
        for (j in seq_len(dim(Bx)[3])) {
            tmp <- mmp(Bx[, , j], Bx[, , k])
            for (i in dim(Bx)[3]:1) {
                if (isTRUE(all.equal(tmp, Bx[, , i]) == TRUE)) 
                  E[j, k] <- i
            }
            rm(i)
        }
        rm(j)
    }
    rm(k)
    dimnames(E)[[1]] <- seq_len(dim(Bx)[3])
    dimnames(E)[[2]] <- seq_len(dim(x)[3])
    if (sum(as.numeric(is.na(E))) > 0) {
        Bxc <- Bx[, , (z + 1):dim(Bx)[3]]
        zz <- which(dimnames(Bx)[[3]] %in% dimnames(Bxc)[[3]])
        for (k in seq_len(z)) {
            for (l in zz) {
                mat <- matrix(NA, nrow = n, ncol = n, byrow = TRUE, 
                  dimnames = list(dimnames(x)[[1]], dimnames(x)[[2]]))
                for (i in seq_len(n)) {
                  for (j in seq_len(n)) {
                    tmp1 <- x[i, , k]
                    tmp2 <- Bx[, j, l]
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
                Bxc <- zbind(Bxc, mat)
                dimnames(Bxc)[[3]][dim(Bxc)[3]] <- paste0(dimnames(x)[[3]][k], 
                  dimnames(Bx)[[3]][l])
            }
            rm(l)
        }
        rm(k)
        Bxc
        dfBxc <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
            nrow = 0))
        for (i in seq_len(dim(Bxc)[3])) {
            dfBxc[i, ] <- as.vector(Bxc[, , i])
        }
        rm(i)
        rownames(dfBxc) <- dimnames(Bxc)[[3]]
        dfBxc
        dfBxxc <- dfBxc
        dfBxc <- unique(dfBxc)
        Bxc <- array(dim = c(dim(x)[1], dim(x)[2], nrow(dfBxc)))
        for (i in seq_len(nrow(dfBxc))) {
            Bxc[, , i][seq_len(dim(Bxc)[1] * dim(Bxc)[2])] <- as.numeric(dfBxc[i, 
                ])
        }
        rm(i)
        dimnames(Bxc)[[1]] <- dimnames(Bxc)[[2]] <- dimnames(x)[[1]]
        dimnames(Bxc)[[3]] <- rownames(dfBxc)
        Bxc
        Bx <- zbind(x, Bxc)
    }
    E <- data.frame(matrix(ncol = dim(x)[3], nrow = dim(Bx)[3]))
    for (k in seq_len(dim(x)[3])) {
        for (j in seq_len(dim(Bx)[3])) {
            tmp <- mmp(Bx[, , j], Bx[, , k])
            for (i in dim(Bx)[3]:1) {
                if (isTRUE(all.equal(tmp, Bx[, , i]) == TRUE)) 
                  E[j, k] <- i
            }
            rm(i)
        }
        rm(j)
    }
    rm(k)
    dimnames(E)[[1]] <- seq_len(dim(Bx)[3])
    dimnames(E)[[2]] <- seq_len(dim(x)[3])
    E
    zz <- seq(max(zz) + 1L, dim(Bx)[3])
    while (sum(as.numeric(is.na(E))) > 0) {
        for (k in seq_len(z)) {
            for (l in zz) {
                mat <- matrix(NA, nrow = n, ncol = n, byrow = TRUE, 
                  dimnames = list(dimnames(x)[[1]], dimnames(x)[[2]]))
                for (i in seq_len(n)) {
                  for (j in seq_len(n)) {
                    tmp1 <- x[i, , k]
                    tmp2 <- Bx[, j, l]
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
                Bxc <- zbind(Bxc, mat)
                dimnames(Bxc)[[3]][dim(Bxc)[3]] <- paste0(dimnames(x)[[3]][k], 
                  dimnames(Bx)[[3]][l])
            }
            rm(l)
        }
        rm(k)
        Bxc
        dfBxc <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
            nrow = 0))
        for (i in seq_len(dim(Bxc)[3])) {
            dfBxc[i, ] <- as.vector(Bxc[, , i])
        }
        rm(i)
        rownames(dfBxc) <- dimnames(Bxc)[[3]]
        dfBx3c <- dfBxc[(max(zz) + 1L - z):nrow(dfBxc), ]
        dfBxc <- unique(dfBxc)
        Bxc <- array(dim = c(dim(x)[1], dim(x)[2], nrow(dfBxc)))
        for (i in seq_len(nrow(dfBxc))) {
            Bxc[, , i][seq_len(dim(Bxc)[1] * dim(Bxc)[2])] <- as.numeric(dfBxc[i, 
                ])
        }
        rm(i)
        dimnames(Bxc)[[1]] <- dimnames(Bxc)[[2]] <- dimnames(x)[[1]]
        dimnames(Bxc)[[3]] <- rownames(dfBxc)
        Bx <- zbind(x, Bxc)
        zz <- seq(max(zz) + 1L, dim(Bx)[3])
        E <- data.frame(matrix(ncol = dim(x)[3], nrow = dim(Bx)[3]))
        for (k in seq_len(dim(x)[3])) {
            for (j in seq_len(dim(Bx)[3])) {
                tmp <- mmp(Bx[, , j], Bx[, , k])
                for (i in dim(Bx)[3]:1) {
                  if (isTRUE(all.equal(tmp, Bx[, , i]) == TRUE)) 
                    E[j, k] <- i
                }
                rm(i)
            }
            rm(j)
        }
        rm(k)
        dimnames(E)[[1]] <- seq_len(dim(Bx)[3])
        dimnames(E)[[2]] <- seq_len(dim(x)[3])
        dfBxxc <- rbind(dfBxxc, dfBx3c)
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
        colnames(S) <- seq_len(nrow(S))
    }
    if (equat == TRUE) {
        tmpo <- rbind(dfBxx, dfBxxc[(nrow(dfBxx) - z + 1L):nrow(dfBxxc), 
            ])
        eq <- list()
        length(eq) <- nrow(unique(tmpo))
        names(eq) <- rownames(unique(tmpo))
        for (i in which(duplicated(tmpo))) {
            for (j in which(!(duplicated(tmpo)))) {
                if (isTRUE(all(tmpo[i, ] == tmpo[j, ]) == TRUE)) {
                  eq[[which(attr(eq, "names") == rownames(tmpo[j, 
                    ]))]] <- append(eq[[which(attr(eq, "names") == 
                    rownames(tmpo[j, ]))]], rownames(tmpo)[i])
                }
            }
            rm(j)
        }
        rm(i)
        eq[sapply(eq, is.null)] <- NULL
    }
    else {
        eq <- NULL
    }
    Lst <- list(dim = dim(x)[1], gens = x, cmps = Bx[, , (z + 
        1):dim(Bx)[3]], equat = eq, ord = nrow(S), st = dimnames(Bx)[[3]], 
        S = S)
    nvc <- c(1L, 2L)
    vc <- max(nvc) + 1L
    if (isTRUE(cmps == TRUE) == TRUE) {
        nvc <- append(nvc, vc)
    }
    else {
        NA
    }
    vc <- vc + 1L
    if (isTRUE(equat == TRUE) == TRUE) {
        nvc <- append(nvc, vc)
    }
    else {
        NA
    }
    nvc <- append(nvc, seq(vc + 1L, 7L))
    lst <- Lst[nvc]
    class(lst) <- c("Semigroup", match.arg(type), "valued")
    return(lst)
}
