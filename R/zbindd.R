zbindd <-
function (..., sort, force) 
{
    argl <- list(...)
    ifelse(missing(sort) == FALSE && isTRUE(sort == TRUE) == 
        TRUE, sort <- TRUE, sort <- FALSE)
    ifelse(missing(force) == FALSE && isTRUE(force == FALSE) == 
        TRUE, force <- FALSE, force <- TRUE)
    if (isTRUE(length(argl) < 2L) == TRUE) {
        if (isTRUE(sort == TRUE) == TRUE) {
            return(perm(..., sort = TRUE))
        }
        else {
            return(argl)
        }
    }
    if (isTRUE(force == TRUE) == TRUE) {
        pvtlbs <- vector()
        for (k in seq_len(length(argl))) {
            if (all(dimnames(argl[[k]])[[1]] == dimnames(argl[[k]])[[2]]) == 
                TRUE) {
                pvtlbs <- append(pvtlbs, dimnames(argl[[k]])[[1]])
            }
            else {
                stop("Dimensions 'x', 'y' must be equal")
            }
        }
        rm(k)
        pvtlbs <- unique(pvtlbs)
        if (isTRUE(dim(argl[[1]])[3] > 1) == TRUE) {
            argl1 <- array(dim = c(length(pvtlbs), length(pvtlbs), 
                dim(argl[[1]])[3]), dimnames = list(pvtlbs, pvtlbs))
            for (k in seq_len(dim(argl[[1]])[3])) {
                argl1[, , k] <- transf(argl[[1]][, , k], type = "toarray", 
                  ord = length(pvtlbs), lbs = pvtlbs)
            }
            rm(k)
            argl[[1]] <- argl1
            pvt <- transf(argl[[1]][, , 1], type = "toarray", 
                ord = length(pvtlbs), lbs = pvtlbs)
            rm(argl1)
        }
        else {
            pvt <- transf(argl[[1]], type = "toarray", ord = length(pvtlbs), 
                lbs = pvtlbs)
            argl[[1]] <- pvt
        }
    }
    else {
        ifelse(isTRUE(dim(argl[[1]])[3] > 1) == TRUE, pvt <- argl[[1]][, 
            , 1], pvt <- argl[[1]])
        pvtlbs <- dimnames(pvt)[[1]]
    }
    if (is.null(pvtlbs) == TRUE) {
        warning("Dimnames in the pivot array are NULL.")
    }
    else if (all(dimnames(pvt)[[1]] == dimnames(pvt)[[2]]) == 
        FALSE) {
        stop("Dimensions 'x', 'y' must be equal")
    }
    tmp <- data.frame(matrix(ncol = (dim(pvt)[1] * dim(pvt)[2]), 
        nrow = 0L))
    for (i in seq_along(argl)) {
        if (isTRUE(i < length(argl)) == TRUE) {
            if (any(dim(argl[[i + 1L]]) != dim(pvt)) == TRUE && 
                isTRUE(force == FALSE) == TRUE || all(attr(argl[[i + 
                1L]], "dimnames")[[1]] %in% pvtlbs) == FALSE) {
                stop("Dimensions of involved arrays differ, consider setting \"force\" to TRUE.")
            }
            else {
                argl[[i + 1L]] <- transf(argl[[i + 1L]], type = "toarray", 
                  ord = dim(pvt)[1], lbs = pvtlbs)
                dimnames(argl[[i + 1L]])[[1]][which(is.na(attr(argl[[i + 
                  1L]], "dimnames")[[1]]))] <- dimnames(argl[[i + 
                  1L]])[[2]][which(is.na(attr(argl[[i + 1L]], 
                  "dimnames")[[1]]))] <- pvtlbs[which(!(pvtlbs %in% 
                  attr(argl[[i + 1L]], "dimnames")[[1]]))]
            }
            if (any(dimnames(argl[[i + 1L]])[[1]] != pvtlbs) == 
                TRUE && isTRUE(dim(argl[[1]])[1] == dim(argl[[1]])[2]) == 
                TRUE) {
                prm <- seq(dim(pvt)[1])
                if (isTRUE(all(dimnames(argl[[i + 1L]])[[1]] == 
                  pvtlbs) == FALSE) == FALSE) {
                  for (m in which(dimnames(argl[[i + 1L]])[[1]] != 
                    pvtlbs)) {
                    prm[m] <- which(dimnames(argl[[i + 1L]])[[1]][m] == 
                      pvtlbs)
                  }
                  rm(m)
                }
                else {
                  warning("Dimnames in the input are different, use from the first array.")
                  dimnames(argl[[i + 1L]])[[1]] <- dimnames(argl[[i + 
                    1L]])[[2]] <- pvtlbs
                }
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
    arr <- array(dim = c(dim(pvt)[1], dim(pvt)[2], nrow(tmp)), 
        dimnames = list(dimnames(pvt)[[1]], dimnames(pvt)[[2]]))
    for (i in seq_len(nrow(tmp))) {
        arr[, , i][seq_len((dim(pvt)[1] * dim(pvt)[2]))] <- as.numeric(tmp[i, 
            ])
    }
    rm(i)
    lbs <- vector()
    for (i in seq_along(argl)) {
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
    if (isTRUE(sort == TRUE) == TRUE) {
        perm(arr, sort = TRUE)
    }
    else {
        arr
    }
}
