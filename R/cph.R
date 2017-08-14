cph <-
function (W, lbs) 
{
    if (isTRUE(attr(W, "class") == "Rel.Box") == FALSE) 
        stop("\"W$W\" must be a \"Rel.Box\" class.")
    rls <- array(dim = c(dim(W$W)[3], dim(W$W)[1], dim(W$W)[1]))
    for (i in seq_len(dim(W$W)[3])) {
        for (j in seq_len(dim(W$W)[1])) {
            rls[i, , j] <- W$W[j, , i]
        }
    }
    phs <- array(0L, dim = c(dim(W$W)[1], dim(W$W)[1], dim(W$W)[1]))
    for (k in seq_len(dim(W$W)[1])) {
        for (j in seq_len(dim(W$W)[1])) {
            for (i in seq_len(dim(W$W)[1])) {
                if ((as.numeric(any(rls[, i, k] < rls[, j, k])) == 
                  1L && as.numeric(any(rls[, j, k] < rls[, i, 
                  k])) == 0L) | as.numeric(all(rls[, i, k] == 
                  rls[, j, k])) == 1L) 
                  phs[i, j, k] <- 1L
            }
        }
    }
    for (k in seq_len(dim(W$W)[1])) {
        for (i in seq_len(dim(W$W)[1])) {
            if (sum(rls[, i, k]) == 0L) 
                phs[i, , k] <- 0L
        }
    }
    dimnames(phs)[[1]] <- dimnames(phs)[[2]] <- dimnames(W$W)[[1]]
    tmp <- data.frame(matrix(ncol = (dim(phs)[1] * dim(phs)[2]), 
        nrow = 0))
    for (i in seq_len(dim(phs)[3])) {
        ifelse(dim(phs)[3] > 1L, tmp[i, ] <- as.vector(phs[, 
            , i]), tmp <- as.vector(phs))
    }
    rm(i)
    phu <- matrix(replace(sapply(tmp, sum), sapply(tmp, sum) >= 
        1, 1), nrow = dim(W$W)[1], ncol = dim(W$W)[1])
    diag(phu) <- 1L
    for (i in seq_len(ncol(phu))) {
        tmp <- outer(phu[, i], phu[i, ], pmin.int)
        phu <- pmax(phu, tmp)
    }
    if (missing(lbs) == FALSE && isTRUE(length(lbs) == dim(phu)[1]) == 
        TRUE) {
        dimnames(phu)[[1]] <- dimnames(phu)[[2]] <- lbs
    }
    else if (missing(lbs) == TRUE) {
        dimnames(phu)[[1]] <- dimnames(phu)[[2]] <- W$lbs
    }
    class(phu) <- c("Partial.Order", "CPH")
    phu
}
