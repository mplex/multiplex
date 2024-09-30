pfvn <-
function (x, r, q) 
{
    ifelse(isTRUE("Multilevel" %in% attr(x, "class")) == TRUE, 
        x <- x$mlnet, NA)
    mx <- norm(as.matrix(x), type = "F")
    note <- NULL
    n <- dim(x)[1]
    if (missing(q) == TRUE) {
        q <- (n - 1)
    }
    else {
        if (isTRUE(q < 2) == TRUE) {
            q <- 2
            warning("'q' is set to the minimum possible value of 2.")
        }
    }
    ifelse(missing(r) == TRUE, r <- Inf, NA)
    ifelse(isTRUE(is.data.frame(x) == TRUE) == TRUE, x <- as.matrix(x), 
        NA)
    if (isTRUE(is.array(x) == TRUE) == FALSE) 
        stop("Input data must be data frame, matrix or array type.")
    if (isTRUE(is.na(dim(x)[3]) == TRUE) == TRUE) {
        Q <- x
        D <- x
        if (isTRUE(isSymmetric(x) == TRUE) == TRUE) {
            sim <- "Symmetric"
            for (k in seq(2, q)) {
                QO <- Q
                for (i in seq_len(n)) {
                  for (j in seq_len(n)) {
                    if (r == Inf) {
                      Q[i, j] <- min(pmax(x[i, ], QO[, j]))
                    }
                    else {
                      Q[i, j] <- min(x[i, ]^r + QO[, j]^r)^(1/r)
                    }
                    if (D[i, j] > Q[i, j]) {
                      D[i, j] <- Q[i, j]
                    }
                  }
                  rm(j)
                }
                rm(i)
            }
            rm(k)
            for (i in seq_len(n)) {
                for (j in seq_len(n)) {
                  if (D[i, j] < x[i, j]) {
                    Q[i, j] <- Inf
                  }
                }
                rm(j)
            }
            rm(i)
            QQ <- Q
        }
        else {
            sim <- "NonSymmetric"
            note <- paste("For non-symmetyric arrays, triangle inequality only for r=", 
                r, "and q=", q, "(?) is supported")
            QQ <- ti(x)
        }
    }
    else if (isTRUE(is.na(dim(x)[3]) == FALSE) == TRUE) {
        sim <- NULL
        QQ <- array(NA, dim = c(dim(x)[1], dim(x)[2], dim(x)[3]), 
            dimnames = list(dimnames(x)[[1]], dimnames(x)[[2]], 
                dimnames(x)[[3]]))
        for (K in seq_len(dim(x)[3])) {
            X <- x[, , K]
            Q <- x[, , K]
            D <- x[, , K]
            if (isTRUE(isSymmetric(X) == TRUE) == TRUE) {
                ifelse(isTRUE(length(sim) == 0) == TRUE, sim <- "Symmetric", 
                  NA)
                for (k in seq(2, q)) {
                  QO <- Q
                  for (i in seq_len(n)) {
                    for (j in seq_len(n)) {
                      if (r == Inf) {
                        Q[i, j] <- min(pmax(X[i, ], QO[, j]))
                      }
                      else {
                        Q[i, j] <- min(X[i, ]^r + QO[, j]^r)^(1/r)
                      }
                      if (D[i, j] > Q[i, j]) {
                        D[i, j] <- Q[i, j]
                      }
                    }
                    rm(j)
                  }
                  rm(i)
                }
                rm(k)
                for (i in seq_len(n)) {
                  for (j in seq_len(n)) {
                    if (D[i, j] < X[i, j]) {
                      Q[i, j] <- Inf
                    }
                  }
                  rm(j)
                }
                rm(i)
            }
            else {
                sim <- "NonSymmetric"
                Q <- ti(X)
            }
            QQ[, , K] <- Q
        }
        rm(K)
    }
    ifelse(isTRUE(length(note) > 0L) == TRUE, lst <- list(max = mx, 
        r = r, q = q, Q = QQ, Note = note), lst <- list(max = mx, 
        r = r, q = q, Q = QQ))
    class(lst) <- c("pathfinder", sim)
    return(lst)
}
