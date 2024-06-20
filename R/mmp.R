mmp <-
function (x1, x2, valued = TRUE) 
{
    if (isTRUE(valued == TRUE) == TRUE) {
        mat <- matrix(NA, nrow = dim(x1)[1], ncol = dim(x1)[2], 
            byrow = TRUE, dimnames = list(dimnames(x1)[[1]], 
                dimnames(x1)[[2]]))
        for (i in seq_len(dim(x1)[1])) {
            for (j in seq_len(dim(x2)[2])) {
                tmp1 <- x1[i, ]
                tmp2 <- x2[, j]
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
    }
    else {
        mat <- replace(x1 %*% x2, x1 %*% x2 >= 1, 1)
    }
    mat
}
