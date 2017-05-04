zbind <-
function (...) 
{
    argl <- list(...)
    if (isTRUE(length(argl) < 2L) == TRUE) 
        return(argl)
    ifelse(isTRUE(dim(argl[[1]])[3] > 1) == TRUE, pvt <- argl[[1]][, 
        , 1], pvt <- argl[[1]])
    tmp <- data.frame(matrix(ncol = (dim(pvt)[1] * dim(pvt)[2]), 
        nrow = 0L))
    for (i in seq_len(length(argl))) {
        if (isTRUE(i < length(argl)) == TRUE) {
            if (all.equal(dim(argl[[i + 1L]])[1:2], dim(pvt)[1:2]) != 
                TRUE) 
                stop("Dimmensions of arrays involved are not equal.")
            pvtlbs <- dimnames(pvt)[[1]]
            if (any(dimnames(argl[[i + 1L]])[[1]] != pvtlbs) == 
                TRUE && isTRUE(dim(argl[[1]])[1] == dim(argl[[1]])[2]) == 
                TRUE) {
                prm <- seq(dim(pvt)[1])
                for (m in which(dimnames(argl[[i + 1L]])[[1]] != 
                  pvtlbs)) {
                  prm[m] <- which(dimnames(argl[[i + 1L]])[[1]][m] == 
                    pvtlbs)
                }
                rm(m)
                argl[[i + 1L]] <- perm(argl[[i + 1L]], clu = prm, 
                  rev = FALSE)
            }
        }
        if (isTRUE(dim(argl[[i]])[3] > 1L) == TRUE) {
            for (j in seq_len(dim(argl[[i]])[3])) {
                tmp[(nrow(tmp) + 1L), ] <- as.vector(as.matrix(argl[[i]][, 
                  , j]))
            }
            rm(j)
        }
        else if (isTRUE(is.na(dim(argl[[i]])[3]) == TRUE) == 
            TRUE | isTRUE(dim(argl[[i]])[3] == 1L) == TRUE) {
            tmp[(nrow(tmp) + 1L), ] <- as.vector(as.matrix(argl[[i]]))
        }
    }
    rm(i)
    arr <- array(dim = c(dim(pvt)[1], dim(pvt)[2], nrow(tmp)))
    for (i in seq_len(nrow(tmp))) {
        arr[, , i][seq_len((dim(pvt)[1] * dim(pvt)[2]))] <- as.numeric(tmp[i, 
            ])
    }
    rm(i)
    if (isTRUE(is.null(dimnames(arr)) == TRUE) == TRUE) {
        dimnames(arr)[[1]] <- dimnames(pvt)[[1]]
        dimnames(arr)[[2]] <- dimnames(pvt)[[2]]
    }
    else {
        NA
    }
    lbs <- vector()
    for (i in seq_len(length(argl))) {
        if (isTRUE(dim(argl[[i]])[3] > 1L) == TRUE) {
            ifelse(is.null(dimnames(argl[[i]])[[3]]) == FALSE, 
                lbs <- append(lbs, dimnames(argl[[i]])[[3]]), 
                lbs <- append(lbs, (length(lbs) + 1L:dim(argl[[i]])[3])))
        }
        else if (isTRUE(dim(argl[[i]])[3] > 1L) == FALSE) {
            if (isTRUE(is.na(dim(argl[[i]])[3])) == FALSE) {
                ifelse(is.null(attr(argl[[i]], "dimnames")[[3]]) == 
                  FALSE, lbs <- append(lbs, attr(argl[[i]], "dimnames")[[3]]), 
                  lbs <- append(lbs, (length(lbs) + 1L)))
            }
            else {
                lbs <- append(lbs, (length(lbs) + 1L))
            }
        }
    }
    rm(i)
    if (isTRUE(length(lbs) == dim(arr)[3]) == TRUE) 
        dimnames(arr)[[3]] <- as.list(lbs)
    arr
}
