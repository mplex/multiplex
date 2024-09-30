cscl <-
function (x, scl, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- "_", NA)
    lvl <- attr(scl, "names")
    dx <- data.frame(matrix(NA, ncol = ncol(x) * ncol(scl), nrow = 0))
    for (i in seq_len(nrow(x))) {
        tmp <- vector()
        for (j in seq_len(ncol(x))) {
            tmp <- c(tmp, as.vector(unlist(scl[which(x[i, j] == 
                lvl), ])))
        }
        rm(j)
        dx <- rbind(dx, tmp)
    }
    rm(i)
    rownames(dx) <- rownames(x)
    colnames(dx) <- paste0(rep(colnames(x), each = length(lvl)), 
        sep, lvl)
    dx
}
