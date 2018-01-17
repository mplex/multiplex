reducs <-
function (x, clu) 
{
    lngt <- nlevels(factor(clu))
    cls <- list()
    for (i in seq_len(lngt)) {
        cls[[i]] <- which(clu == i)
    }
    rm(i)
    if (isTRUE(attr(x, "class")[1] == "Semigroup") == FALSE) {
        stop("\"x\" should be an object of a \"Semigroup\" class.")
    }
    tmp <- x
    lb <- x$st
    ifelse(isTRUE(attr(tmp, "class")[2] == "symbolic") == TRUE, 
        x <- as.semigroup(tmp, lbs = seq_len(tmp$ord)), x <- tmp$S)
    ifelse(isTRUE(is.array(x)) == TRUE, xa <- x, xa <- array(x, 
        dimnames = lb))
    px <- perm(xa, clu, rev = FALSE)
    tab <- tabulate(clu)
    bm <- array(dim = c(lngt, lngt))
    y <- h <- j <- 0
    for (i in seq_len(lngt)) {
        k <- ((y + 1L):(y + tabulate(clu)[i]))
        for (j in seq_len(lngt)) {
            for (q in seq_len(length(cls))) {
                if (all(unique(as.numeric(levels(factor(as.matrix(x[which(clu == 
                  i), which(clu == j)]))))) %in% cls[[q]]) == 
                  TRUE) {
                  if (isTRUE(attr(tmp, "class")[2] == "symbolic") == 
                    TRUE) {
                    bm[i, j] <- lb[min(as.numeric(as.matrix(px[k, 
                      (h + 1L):(h + tabulate(clu)[j])])))]
                  }
                  else if (isTRUE(attr(tmp, "class")[2] == "numerical") == 
                    TRUE) {
                    bm[i, j] <- min(as.numeric(as.matrix(px[k, 
                      (h + 1L):(h + tabulate(clu)[j])])))
                  }
                }
            }
            if (isTRUE(j > 0) == TRUE) 
                h <- sum(tab[seq_len(j)])
        }
        rm(j)
        h <- 0
        y <- sum(tab[seq_len(i)])
    }
    rm(i)
    lbs <- vector()
    for (i in seq_len(length(tabulate(clu)))) {
        ifelse(isTRUE(attr(tmp, "class")[2] == "symbolic") == 
            TRUE, lbs[length(lbs) + 1L] <- lb[which(clu == i)[1]], 
            lbs[length(lbs) + 1L] <- as.numeric(dimnames(x)[[1]])[which(clu == 
                i)[1]])
    }
    rm(i)
    for (i in seq_len(dim(bm)[1])) {
        for (j in seq_len(dim(bm)[1])) {
            if (isTRUE(bm[i, j] %in% lbs) == FALSE) {
                for (l in seq_len(length(lbs))) {
                  if (isTRUE(attr(tmp, "class")[2] == "symbolic") == 
                    TRUE) {
                    if (isTRUE(bm[i, j] %in% lbs[l]) == TRUE) 
                      bm[i, j] <- lbs[l]
                  }
                  else if (isTRUE(attr(tmp, "class")[2] == "numerical") == 
                    TRUE) {
                    if (isTRUE(clu[bm[i, j]] %in% as.numeric(lbs)[l]) == 
                      TRUE) 
                      bm[i, j] <- l
                  }
                }
                rm(l)
            }
        }
        rm(j)
    }
    rm(i)
    dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- as.list(lbs)
    return(as.data.frame(bm))
}
