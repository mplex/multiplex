reducs <-
function (x, cl) 
{
    if (isTRUE(attr(x, "class")[1] == "Semigroup") == FALSE) 
        stop("\"x\" should be an object of a \"Semigroup\" class.")
    ifelse(isTRUE(attr(x, "class")[2] == "symbolic") == TRUE, 
        sx <- as.semigroup(x, numerical = TRUE)$S, sx <- x$S)
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
                if (all(unique(as.vector(unlist(sx[which(cl == 
                  i), which(cl == j)]))) %in% cls[[q]]) == TRUE) {
                  if (isTRUE("symbolic" %in% attr(x, "class")) == 
                    TRUE) {
                    bm[i, j] <- bm[i, j] <- x$st[min(cls[[q]])]
                  }
                  else if (isTRUE(attr(x, "class")[2] == "numerical") == 
                    TRUE) {
                    bm[i, j] <- min(as.numeric(dimnames(sx[which(cl == 
                      i), which(cl == j)])[[1]]))
                  }
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
        ifelse(isTRUE(attr(x, "class")[2] == "symbolic") == TRUE, 
            lbs <- append(lbs, x$st[which(cl == i)[1]]), lbs <- append(lbs, 
                as.numeric(dimnames(sx)[[1]])[which(cl == i)[1]]))
    }
    rm(i)
    dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- as.list(lbs)
    return(as.data.frame(bm))
}
