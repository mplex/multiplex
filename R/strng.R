strng <-
function (x) 
{
    if (is.array(x) == FALSE) 
        stop("Data must be a stacked array of square matrices if a product of 'strings'.")
    if (is.na(dim(x)[3]) == FALSE) {
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
        rm(i, j)
        rownames(po) <- colnames(po) <- dimnames(x)[[3]]
    }
    else if (is.na(dim(x)[3]) == TRUE) {
        po <- 1L
    }
    po
}
