reducs <-
function (s, cl) 
{
    if (isTRUE("Semigroup" %in% attr(s, "class")) == FALSE) 
        stop("\"s\" should be an object of a \"Semigroup\" class.")
    ifelse(isTRUE("symbolic" %in% attr(s, "class")) == TRUE, 
        sx <- as.semigroup(s, numerical = TRUE), sx <- s)
    n <- nlevels(factor(cl))
    cls <- list()
    for (i in seq_len(n)) {
        cls[[i]] <- which(cl == i)
    }
    rm(i)
    bm <- array(dim = c(n, n))
    for (i in seq_len(n)) {
        k <- seq_along(cls[[i]])
        for (j in seq_len(n)) {
            for (q in seq_along(cls)) {
                if (all(unique(as.vector(unlist(sx$S[which(cl == 
                  i), which(cl == j)]))) %in% cls[[q]]) == TRUE) {
                  ifelse(isTRUE("symbolic" %in% attr(s, "class")) == 
                    TRUE, bm[i, j] <- bm[i, j] <- s$st[min(cls[[q]])], 
                    bm[i, j] <- bm[i, j] <- as.numeric(dimnames(sx$S)[[1]])[min(cls[[q]])])
                  break
                }
                else {
                  NA
                }
            }
            rm(q)
        }
        rm(j)
    }
    rm(i)
    lbs <- vector()
    for (i in seq_along(tabulate(cl))) {
        ifelse(isTRUE("symbolic" %in% attr(s, "class")) == TRUE, 
            lbs <- append(lbs, s$st[which(cl == i)[1]]), lbs <- append(lbs, 
                as.numeric(dimnames(sx$S)[[1]])[which(cl == i)[1]]))
    }
    rm(i)
    dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- as.list(lbs)
    return(as.data.frame(bm))
}
