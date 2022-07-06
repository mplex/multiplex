zbind <-
function (..., sort, force) 
{
    argl <- list(...)
    if (any(unlist(lapply(lapply(argl, dim), length)) < 2L) == 
        TRUE) 
        stop("Input objects must be arrays or data frames.")
    ifelse(isTRUE(dim(argl[[1]])[3] > 1) == TRUE, pvt0 <- argl[[1]][, 
        , 1], pvt0 <- argl[[1]])
    if (is.null(dimnames(pvt0)[[1]]) == TRUE) {
        warning("Dimnames NULL detected, bind first two arrays.")
        return(zbnd(...))
    }
    ifelse(missing(sort) == FALSE && isTRUE(sort == TRUE) == 
        TRUE, sort <- TRUE, sort <- FALSE)
    ifelse(missing(force) == FALSE && isTRUE(force == TRUE) == 
        TRUE, force <- TRUE, force <- FALSE)
    if (isTRUE(length(argl) < 2L) == TRUE) {
        if (isTRUE(sort == TRUE) == TRUE) {
            return(perm(..., sort = TRUE))
        }
        else {
            return(argl)
        }
    }
    else {
        if (isTRUE(length(apply(sapply(argl, function(z) dim(z)[1:2]), 
            1, unique)) == 2) == TRUE && isTRUE(nrow(unique(as.data.frame(do.call(rbind, 
            lapply(argl, function(z) dimnames(z)[[2]]))))) == 
            1) == TRUE) {
            argz <- zbnd(argl[[1]], argl[[2]])
            dimnames(argz)[[1]] <- dimnames(argz)[[2]] <- dimnames(pvt0)[[1]]
            if (isTRUE(length(argl) == 2L) == TRUE) {
                if (is.na(dim(argl[[1]])[3]) == TRUE && is.na(dim(argl[[2]])[3]) == 
                  TRUE) {
                  NA
                }
                else if (is.na(dim(argl[[1]])[3]) == TRUE) {
                  dimnames(argz)[[3]] <- c("1", dimnames(argl[[2]])[[3]])
                }
                else if (is.na(dim(argl[[2]])[3]) == TRUE) {
                  dimnames(argz)[[3]] <- c(dimnames(argl[[1]])[[3]], 
                    dim(argl[[1]])[3] + 1L)
                }
                else {
                  dimnames(argz)[[3]] <- c(dimnames(argl[[1]])[[3]], 
                    dimnames(argl[[2]])[[3]])
                }
                return(argz)
            }
            for (k in seq(3, length(argl))) {
                argz <- zbnd(argz, argl[[k]])
            }
            rm(k)
            dimnames(argz)[[1]] <- dimnames(argz)[[2]] <- dimnames(pvt0)[[1]]
            lbs3 <- vector()
            plbs3 <- unlist(lapply(argl, function(z) {
                if (is.na(dim(z)[3]) == FALSE) {
                  lbs3 <- append(lbs3, dimnames(z)[[3]])
                }
                else {
                  lbs3 <- append(lbs3, NA)
                }
                return(lbs3)
            }))
            plbs3[which(is.na(plbs3))] <- which(is.na(plbs3))
            dimnames(argz)[[3]] <- plbs3
            return(argz)
        }
        else {
            if (isTRUE(force == TRUE) == FALSE) 
                stop("Arrays have different dimensions, set \"force\" to TRUE.")
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
            if (isTRUE(dim(argl1)[1] == dim(argl[[1]])[1]) == 
                FALSE) {
                for (k in seq_len(dim(argl[[1]])[3])) {
                  argl1[, , k] <- transf(argl[[1]][, , k], type = "toarray", 
                    ord = length(pvtlbs), lbs = pvtlbs)
                }
                rm(k)
                argl[[1]] <- argl1
            }
            pvt <- argl[[1]][, , 1]
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
    dnames <- lapply(argl, function(z) dimnames(z)[[1]])
    argdf <- data.frame(matrix(ncol = (dim(pvt)[1] * dim(pvt)[2]), 
        nrow = 0L))
    for (i in seq_along(argl)) {
        ifelse(isTRUE(i < length(argl)) == TRUE, k <- 1L, k <- 0)
        if (all(all.equal(dnames[i], dnames[i + k]) == TRUE) == 
            FALSE) {
            if (isTRUE(dim(argl[[i + k]])[3] > 1) == TRUE) {
                NA
            }
            else {
                argl[[i + k]] <- transf(argl[[i + k]], "toarray", 
                  ord = length(pvtlbs), lbs = pvtlbs)
            }
        }
        if (all.equal(dimnames(pvt)[[2]], dimnames(argl[[i]])[[2]]) == 
            TRUE) {
            argdf <- rbind(argdf, matrix(as.vector(argl[[i]]), 
                ncol = ncol(argdf), byrow = TRUE))
        }
        else {
            tempargdf <- matrix(nrow = dim(pvt)[1], ncol = dim(pvt)[2], 
                dimnames = list(pvtlbs, pvtlbs))
            tempargdf <- array(dim = c(dim(pvt)[1], dim(pvt)[2], 
                dim(argl[[i + k]])[3]), dimnames = list(pvtlbs, 
                pvtlbs))
            tempargdf[which(pvtlbs %in% dimnames(argl[[i]])[[1]]), 
                which(pvtlbs %in% dimnames(argl[[i]])[[1]]), 
                ] <- argl[[i]]
            argdf <- rbind(argdf, matrix(as.vector(tempargdf), 
                ncol = ncol(argdf), byrow = TRUE))
        }
    }
    argdf[sapply(argdf, is.na)] <- 0
    arr <- array(dim = c(dim(pvt)[1], dim(pvt)[2], nrow(argdf)), 
        dimnames = list(dimnames(pvt)[[1]], dimnames(pvt)[[2]]))
    for (i in seq_len(nrow(argdf))) {
        arr[, , i][seq_len((dim(pvt)[1] * dim(pvt)[2]))] <- as.numeric(argdf[i, 
            ])
    }
    rm(i)
    if (any(unlist(lapply(lapply(argl, dim), length)) != 3) == 
        FALSE) {
        pdim <- unlist(lapply(argl, function(z) {
            dimnames(z)[[3]]
        }))
        if (isTRUE(length(pdim) == dim(arr)[3]) == TRUE) {
            dimnames(arr)[[3]] <- pdim
        }
    }
    if (isTRUE(sort == TRUE) == TRUE) {
        perm(arr, sort = TRUE)
    }
    else {
        arr
    }
}
