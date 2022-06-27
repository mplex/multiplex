zbind <-
function (..., sort, force) 
{
    argl <- list(...)
    ifelse(isTRUE(dim(argl[[1]])[3] > 1) == TRUE, pvt0 <- argl[[1]][, 
        , 1], pvt0 <- argl[[1]])
    if (is.null(dimnames(pvt0)[[1]]) == TRUE) {
        warning("Dimnames NULL detected.")
        return(zbnd(...))
    }
    ifelse(missing(sort) == FALSE && isTRUE(sort == TRUE) == 
        TRUE, sort <- TRUE, sort <- FALSE)
    if (isTRUE(length(argl) < 2L) == TRUE) {
        if (isTRUE(sort == TRUE) == TRUE) {
            return(perm(..., sort = TRUE))
        }
        else {
            return(argl)
        }
    }
    ifelse(missing(force) == FALSE && isTRUE(force == FALSE) == 
        TRUE, force <- FALSE, force <- TRUE)
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
    argdf <- data.frame(matrix(ncol = (dim(pvt)[1] * dim(pvt)[2]), 
        nrow = 0L))
    for (i in seq_along(argl)) {
        ifelse(isTRUE(i < length(argl)) == TRUE, k <- 1L, k <- 0)
        if (any(dim(argl[[i + k]])[1:2] != dim(pvt)) == TRUE && 
            isTRUE(force == FALSE) == TRUE || all(attr(argl[[i + 
            k]], "dimnames")[[1]] %in% pvtlbs) == FALSE) {
            stop("Dimensions of involved arrays differ, consider setting \"force\" to TRUE.")
        }
        else {
            if (isTRUE(dim(argl[[i + k]])[3] > 1) == TRUE) {
                tempargdf <- array(dim = c(dim(pvt)[1], dim(pvt)[2], 
                  dim(argl[[i + k]])[3]), dimnames = list(pvtlbs, 
                  pvtlbs))
                for (j in seq_len(dim(argl[[i + k]])[3])) {
                  tempargdf[, , j] <- transf(argl[[i + k]][, 
                    , j], type = "toarray", ord = dim(pvt)[1], 
                    lbs = pvtlbs)
                }
                rm(j)
                argl[[i + k]] <- tempargdf
            }
            else {
                argl[[i + k]] <- transf(argl[[i + k]], type = "toarray", 
                  ord = dim(pvt)[1], lbs = pvtlbs)
                dimnames(argl[[i + k]])[[1]][which(is.na(attr(argl[[i + 
                  k]], "dimnames")[[1]]))] <- dimnames(argl[[i + 
                  k]])[[2]][which(is.na(attr(argl[[i + k]], "dimnames")[[1]]))] <- pvtlbs[which(!(pvtlbs %in% 
                  attr(argl[[i + k]], "dimnames")[[1]]))]
            }
        }
        if (any(dimnames(argl[[i + k]])[[1]] != pvtlbs) == TRUE && 
            isTRUE(dim(argl[[1]])[1] == dim(argl[[1]])[2]) == 
                TRUE) {
            prm <- seq(dim(pvt)[1])
            if (isTRUE(all(dimnames(argl[[i + k]])[[1]] == pvtlbs) == 
                FALSE) == FALSE) {
                for (m in which(dimnames(argl[[i + k]])[[1]] != 
                  pvtlbs)) {
                  prm[m] <- which(dimnames(argl[[i + k]])[[1]][m] == 
                    pvtlbs)
                }
                rm(m)
            }
            else {
                warning("Dimnames are different and it uses from the first array. Otherwise set 'force' to TRUE.")
                dimnames(argl[[i + k]])[[1]] <- dimnames(argl[[i + 
                  k]])[[2]] <- pvtlbs
            }
            argl[[i + k]] <- perm(argl[[i + k]], clu = prm, rev = FALSE)
        }
        if (isTRUE(dim(argl[[i]])[3] > 1L) == TRUE) {
            for (j in seq_len(dim(argl[[i]])[3])) {
                argdf[(nrow(argdf) + 1L), ] <- as.vector(as.matrix(argl[[i]][, 
                  , j]))
            }
            rm(j)
        }
        else if (isTRUE(is.na(dim(argl[[i]])[3]) == TRUE) == 
            TRUE | isTRUE(dim(argl[[i]])[3] == 1L) == TRUE) {
            argdf[(nrow(argdf) + 1L), ] <- as.vector(as.matrix(argl[[i]]))
        }
    }
    rm(i)
    arr <- array(dim = c(dim(pvt)[1], dim(pvt)[2], nrow(argdf)), 
        dimnames = list(dimnames(pvt)[[1]], dimnames(pvt)[[2]]))
    for (i in seq_len(nrow(argdf))) {
        arr[, , i][seq_len((dim(pvt)[1] * dim(pvt)[2]))] <- as.numeric(argdf[i, 
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
