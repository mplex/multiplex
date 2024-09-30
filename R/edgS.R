edgS <-
function (E, semigroupClass = FALSE) 
{
    if (isTRUE("EdgeTable" %in% attr(E, "class")) == TRUE) {
        gens <- E$gens
        E <- E$E
    }
    else {
        gens <- seq_len(ncol(E))
    }
    if (isTRUE(ncol(E) > 2) == TRUE) 
        stop("Two generators only are yet accepted.")
    n <- nrow(E)
    m <- ncol(E)
    S <- cbind(E, data.frame(matrix(ncol = (n - m), nrow = n)))
    colnames(S) <- seq_len(n)
    for (r in seq_len(n)) {
        for (s in (m + 1):n) {
            ws <- which(E == s)
            i <- ws%%n
            i <- replace(i, i == 0, n)
            j <- vector()
            length(j) <- length(i)
            j[which(ws <= n)] <- 1
            j[which(ws > n)] <- 2
            if (all(i > m) == TRUE) {
                tmp <- c(r, i[1], j[1])
                if (isTRUE(length(which(tmp > m)) == 1) == TRUE) {
                  chg <- which(tmp > m)
                }
                else {
                  chg <- which(tmp > m)[which(tmp > m)[length(which(tmp > 
                    m))]]
                }
                while (any(tmp > m) == TRUE) {
                  ifelse(isTRUE(length(which(tmp > m)) == 1) == 
                    TRUE, chg <- which(tmp > m), chg <- which(tmp > 
                    m)[which(tmp > m)[length(which(tmp > m))]])
                  ss <- tmp[chg]
                  wss <- which(E == ss)
                  ii <- wss%%n
                  ii <- replace(ii, ii == 0, n)
                  jj <- vector()
                  length(jj) <- length(ii)
                  jj[which(wss <= n)] <- 1
                  jj[which(wss > n)] <- 2
                  ifelse(isTRUE(chg > 1) == TRUE, tmp <- c(tmp[1:(chg - 
                    1)], c(ii[which(ii == min(ii))], jj[which(ii == 
                    min(ii))]), tmp[(chg + 1):length(tmp)]), 
                    tmp <- c(c(ii[which(ii == min(ii))], jj[which(ii == 
                      min(ii))]), tmp[(chg + 1):length(tmp)]))
                }
                if (any(tmp > m) == TRUE) {
                  for (k in seq_len(length(tmp) - 2)) {
                    tmp0 <- E[tmp[1], tmp[2]]
                    ifelse(isTRUE(length(tmp) > k) == TRUE, tmp <- c(tmp0, 
                      tmp[(k + 2):length(tmp)]), tmp <- c(E[tmp[1], 
                      tmp[2]], tmp[3]))
                  }
                  rm(k)
                }
                else {
                  ifelse(E[tmp[1], tmp[2]] > m, tmp1 <- E[tmp[1], 
                    tmp[2]], tmp1 <- c(tmp[1], tmp[2]))
                  tmp <- c(tmp1, tmp[3], tmp[4])
                  tmp <- c(E[tmp[1], tmp[2]], tmp[3])
                }
            }
            else {
                tmp <- c(r, i[which(i <= m)][1], j[which(i <= 
                  m)][1])
                if (E[tmp[2], tmp[3]] > m) {
                  tmp <- c(E[tmp[1], tmp[2]], tmp[3])
                }
                else {
                  tmp <- c(tmp[1], E[tmp[2], tmp[3]])
                }
            }
            S[r, s] <- S[tmp[1], tmp[2]]
        }
    }
    if (isTRUE(semigroupClass == FALSE) == TRUE) {
        S
    }
    else {
        lst <- list(ord = n, st = rownames(E), gens = gens, S = S)
        class(lst) <- "Semigroup"
        return(lst)
    }
}
