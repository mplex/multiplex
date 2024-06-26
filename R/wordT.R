wordT <- 
function (x) 
{
    if (is.array(x) == FALSE) 
        stop("Data must be a stacked array of square matrices.")
    if (is.na(dim(x)[3]) == TRUE) {
        s0 <- data.frame(matrix(ncol = 1, nrow = 1))
        if (isTRUE(all.equal(replace(x %*% x, x %*% x >= 1, 1), 
            x) == TRUE)) 
            s0[1, 1] <- 1
        Bx <- array(dim = c(dim(x)[1], dim(x)[2], 2))
        Bx[, , 1] <- as.matrix(x)
        Bx[, , 2] <- replace(x %*% x, x %*% x >= 1, 1)
    }
    if (is.na(dim(x)[3]) == FALSE) {
        tmp0 <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
            nrow = 0))
        for (i in seq_len(dim(x)[3])) {
            ifelse(dim(x)[3] > 1, tmp0[i, ] <- as.vector(x[, 
                , i]), tmp0 <- as.vector(x))
        }
        rm(i)
        if (isTRUE(is.null(dim(tmp0)) == FALSE) == TRUE) 
            rownames(tmp0) <- dimnames(x)[[3]]
        if (dim(x)[3] < 2) 
            x <- array(tmp0, c(dim(x)[1], dim(x)[2]))
        if (dim(x)[3] > 1) {
            tmp <- array(dim = c(dim(x)[1], dim(x)[2], nrow(unique(tmp0))))
            for (i in seq_len(nrow(unique(tmp0)))) {
                tmp[, , i][seq_len(dim(x)[1] * dim(x)[2])] <- as.numeric(unique(tmp0)[i, 
                  ])
            }
            rm(i)
            if (is.null(dimnames(tmp)[[1]]) == FALSE) 
                dimnames(tmp)[[3]] <- rownames(unique(tmp0))
            if (is.null(dimnames(x)[[1]]) == FALSE) 
                dimnames(tmp)[[1]] <- dimnames(tmp)[[2]] <- dimnames(x)[[1]]
            x <- tmp
            dimnames(x)[[3]] <- as.list(rownames(unique(tmp0)))
        }
        s0 <- data.frame(matrix(ncol = dim(x)[3], nrow = dim(x)[3]))
        for (k in seq_len(dim(x)[3])) {
            for (j in seq_len(dim(x)[3])) {
                for (i in rev(seq_len(dim(x)[3]))) {
                  if (isTRUE(all.equal(replace(x[, , j] %*% x[, 
                    , k], x[, , j] %*% x[, , k] >= 1, 1), x[, 
                    , i]) == TRUE)) 
                    s0[j, k] <- i
                }
            }
        }
        rm(i, j, k)
        rm(tmp0, tmp)
        dimnames(s0)[[1]] <- seq_len(dim(x)[3])
        dimnames(s0)[[2]] <- seq_len(dim(x)[3])
        if (sum(as.numeric(is.na(s0))) == 0) 
            Bx <- x
        if (sum(as.numeric(is.na(s0))) > 0) {
            Bx <- array(dim = c(dim(x)[1], dim(x)[2], 0))
            for (i in seq_len(nrow(s0))) {
                for (j in seq_len(length(which(is.na(s0[i, ]))))) {
                  if (length(which(is.na(s0[i, ]))) > 0) 
                    Bx <- zbnd(Bx, (replace(x[, , i] %*% x[, 
                      , which(is.na(s0[i, ]))[j]], x[, , i] %*% 
                      x[, , which(is.na(s0[i, ]))[j]] >= 1, 1)))
                }
            }
            rm(i, j)
            tmp <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
                nrow = 0))
            for (i in seq_len(dim(Bx)[3])) {
                tmp[i, ] <- as.vector(Bx[, , i])
            }
            rm(i)
            xBx <- array(dim = c(dim(x)[1], dim(x)[2], nrow(unique(tmp))))
            for (i in seq_len(nrow(unique(tmp)))) {
                xBx[, , i][seq_len(dim(Bx)[1] * dim(Bx)[2])] <- as.numeric(unique(tmp)[i, 
                  ])
            }
            rm(i)
            if (is.null(dimnames(xBx)) == FALSE) 
                dimnames(xBx)[[3]] <- (dim(x)[3] + 1):(dim(xBx)[3] + 
                  dim(x)[3])
            Bx <- zbnd(x, xBx)
            rm(xBx, tmp)
        }
    }
    while (sum(as.numeric(is.na(s0))) > 0) {
        BBx <- Bx
        for (i in seq_len(nrow(s0))) {
            for (j in seq_len(length(which(is.na(s0[i, ]))))) {
                if (length(which(is.na(s0[i, ]))) > 0) 
                  BBx <- zbnd(BBx, (replace(Bx[, , i] %*% Bx[, 
                    , which(is.na(s0[i, ]))[j]], Bx[, , i] %*% 
                    Bx[, , which(is.na(s0[i, ]))[j]] >= 1, 1)))
            }
        }
        rm(i, j)
        tmp <- data.frame(matrix(ncol = (dim(Bx)[1] * dim(Bx)[2]), 
            nrow = 0))
        for (i in seq_len(dim(BBx)[3])) {
            tmp[i, ] <- as.vector(BBx[, , i])
        }
        rm(i)
        Bx <- array(dim = c(dim(x)[1], dim(x)[2], nrow(unique(tmp))))
        for (i in seq_len(nrow(unique(tmp)))) {
            Bx[, , i][seq_len(dim(BBx)[1] * dim(BBx)[2])] <- as.numeric(unique(tmp)[i, 
                ])
        }
        rm(i)
        rm(tmp, BBx)
        if (is.na(dim(x)[3]) == TRUE) {
            s0 <- data.frame(matrix(ncol = 1, nrow = dim(Bx)[3]))
            for (j in seq_len(dim(Bx)[3])) {
                for (i in rev(seq_len(dim(Bx)[3]))) {
                  if (isTRUE(all.equal(replace(Bx[, , j] %*% 
                    Bx[, , 1], Bx[, , j] %*% Bx[, , 1] >= 1, 
                    1), Bx[, , i]) == TRUE)) 
                    s0[j, 1] <- i
                }
            }
            rm(i, j)
        }
        if (is.na(dim(x)[3]) == FALSE) {
            s0 <- data.frame(matrix(ncol = dim(x)[3], nrow = dim(Bx)[3]))
            for (k in seq_len(dim(x)[3])) {
                for (j in seq_len(dim(Bx)[3])) {
                  for (i in rev(seq_len(dim(Bx)[3]))) {
                    if (isTRUE(all.equal(replace(Bx[, , j] %*% 
                      Bx[, , k], Bx[, , j] %*% Bx[, , k] >= 1, 
                      1), Bx[, , i]) == TRUE)) 
                      s0[j, k] <- i
                  }
                }
            }
            rm(i, j, k)
        }
    }
    ifelse(isTRUE(is.na(dim(x)[3])) == TRUE, dimnames(s0)[[2]] <- 1, 
        dimnames(s0)[[2]] <- seq_len(dim(x)[3]))
    tmp0 <- data.frame(matrix(ncol = (dim(Bx)[1] * dim(Bx)[2]), 
        nrow = 0))
    for (i in seq_len(dim(Bx)[3])) {
        tmp0[i, ] <- as.vector(Bx[, , i])
    }
    rm(i)
    rownames(tmp0) <- dimnames(Bx)[[3]]
    tmp <- array(dim = c(dim(Bx)[1], dim(Bx)[2], nrow(unique(tmp0))))
    for (i in seq_len(nrow(unique(tmp0)))) {
        tmp[, , i][seq_len(dim(Bx)[1] * dim(Bx)[2])] <- as.numeric(unique(tmp0)[i, 
            ])
    }
    rm(i)
    Bx <- tmp
    dimnames(Bx)[[3]] <- as.list(rownames(unique(tmp0)))
    E <- s0
    rm(s0)
    if (dim(Bx)[3] == ncol(E)) {
        W <- rbind(cbind(seq_len(ncol(E)), NA, NA))
        colnames(W) <- c("", "n", "g")
    }
    if (dim(Bx)[3] > ncol(E)) {
        tmp <- (ncol(E) + 1):dim(Bx)[3]
        z <- vector()
        for (i in seq_len(length(tmp))) {
            z[i] <- which(t(E) == tmp[i])[1]
        }
        rm(i)
        g <- vector()
        for (i in seq_len(length(tmp))) {
            ifelse(z[i]%%ncol(E) == 0, g[i] <- ncol(E), g[i] <- z[i]%%ncol(E))
        }
        rm(i)
        n <- vector()
        for (i in seq_len(length(tmp))) {
            ifelse(z[i]%%ncol(E) == 0, n[i] <- (z[i]%/%ncol(E)), 
                n[i] <- ((z[i]%/%ncol(E)) + 1))
        }
        rm(i)
        W <- rbind(cbind(seq_len(ncol(E)), NA, NA), cbind(((ncol(E) + 
            1):nrow(E)), n, g))
        rm(z, n, g)
    }
    rm(tmp)
    wt <- list(gens = x, WT = W)
    class(wt) <- "WordTable"
    return(wt)
}
