strng <-
function (x) 
{
    if (is.list(x) == TRUE && isTRUE(length(x) > 1L) == TRUE) {
        xl <- x
        x <- array(dim = c(dim(xl[[1]]), length(xl)))
        for (k in seq_len(length(xl))) {
            x[, , k] <- xl[[k]]
        }
        rm(k)
        dimnames(x)[[1]] <- dimnames(x)[[2]] <- dimnames(xl[[1]])[[1]]
        dimnames(x)[[3]] <- names(xl)
    }
    else {
        if (is.array(x) == FALSE) 
            stop("Data must be a stacked array of square matrices of a product of 'strings' or a list of of square matrices.")
    }
    if (is.array(x) == TRUE && (is.na(dim(x)[3]) == FALSE)) {
        tmp <- data.frame(matrix(ncol = (dim(x)[1] * dim(x)[2]), 
            nrow = 0))
        for (i in seq_len(dim(x)[3])) {
            tmp[i, ] <- as.vector(x[, , i])
        }
        rm(i)
        po <- as.matrix(array(0L, dim = c(dim(x)[3], dim(x)[3])))
        for (j in seq_len(dim(x)[3])) {
            for (i in seq_len(dim(x)[3])) {
                if ((as.numeric(any(tmp[i, ] < tmp[j, ])) == 
                  1 && as.numeric(any(tmp[j, ] < tmp[i, ])) == 
                  0) | as.numeric(all(tmp[i, ] == tmp[j, ])) == 
                  1) 
                  po[i, j] <- 1L
            }
        }
        rm(j)
        rownames(po) <- colnames(po) <- dimnames(x)[[3]]
    }
    else if (is.array(x) == TRUE && is.na(dim(x)[3]) == TRUE || 
        (is.list(x) == TRUE && isTRUE(length(x) == 1L) == TRUE)) {
        po <- 1L
    }
    po
}
