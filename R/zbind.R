zbind <-
function (..., sort, force) 
{
    ifelse(missing(force) == FALSE && isTRUE(force == TRUE) == 
        TRUE, force <- TRUE, force <- FALSE)
    ifelse(missing(sort) == FALSE && isTRUE(sort == TRUE) == 
        TRUE, sort <- TRUE, sort <- FALSE)
    argl <- list(...)
    if (any(unlist(lapply(lapply(argl, dim), length)) < 2L) == 
        TRUE) 
        stop("Input objects must be arrays or data frames.")
    lapply(argl, function(z) {
        ifelse(all(dimnames(z)[[1]] == dimnames(z)[[2]]) == TRUE, 
            invisible(NA), stop("\"x\", \"y\" must be square arrays; dimensions not equal."))
    })
    ifelse(isTRUE(dim(argl[[1]])[3] > 1L) == TRUE, pvt0 <- argl[[1]][, 
        , 1], pvt0 <- argl[[1]])
    if (is.null(dimnames(pvt0)[[1]]) == TRUE) {
        warning("Dimnames NULL detected, bind first two arrays.")
        return(zbnd(...))
    }
    if (isTRUE(length(argl) < 2L) == TRUE) {
        if (isTRUE(sort == TRUE) == TRUE) {
            return(perm(..., sort = TRUE))
        }
        else {
            return(argl)
        }
    }
    dmnl <- lapply(argl, function(z) dimnames(z)[[1]])
    pvtlbs <- unique(unlist(lapply(argl, function(z) {
        dimnames(z)[[1]]
    })))
    if (isTRUE(length(unique(unlist(lapply(dmnl, length)))) == 
        1L)) {
        flgx <- FALSE
        if (isTRUE(length(unique(unlist(lapply(argl, function(z) dimnames(z)[[1]])))) == 
            unique(unlist(lapply(lapply(argl, function(z) dimnames(z)[[1]]), 
                length)))) == TRUE) {
            if (isTRUE(nrow(unique(as.data.frame(do.call(rbind, 
                dmnl)))) == 1L) == FALSE) {
                for (k in 2:length(argl)) {
                  tmplbs <- dmnl[[k]]
                  vec <- vector()
                  for (i in seq_len(length(tmplbs))) {
                    vec <- append(vec, which(pvtlbs %in% tmplbs[i]))
                  }
                  rm(i)
                  argl[[k]] <- perm(argl[[k]], clu = vec)
                }
                rm(k)
                rm(vec, tmplbs)
            }
            else {
                invisible(NA)
            }
        }
        else {
            if (isTRUE(force == TRUE) == FALSE) 
                stop("Arrays have different labeling; set \"force\" to TRUE.")
            flgx <- TRUE
        }
    }
    else {
        if (isTRUE(force == TRUE) == FALSE) 
            stop("Arrays have different dimensions; set \"force\" to TRUE.")
        flgx <- TRUE
    }
    if (isTRUE(flgx == FALSE) == TRUE) {
        argdf <- data.frame(matrix(ncol = (dim(pvt0)[1] * dim(pvt0)[2]), 
            nrow = 0L))
        for (i in seq_along(argl)) {
            ifelse(isTRUE(i < length(argl)) == TRUE, k <- 1L, 
                k <- 0)
            if (all(all.equal(dmnl[i], dmnl[i + k]) == TRUE) == 
                FALSE) {
                ifelse(isTRUE(dim(argl[[i + k]])[3] > 1) == TRUE, 
                  NA, argl[[i + k]] <- transf(argl[[i + k]], 
                    type = "toarray", ord = length(pvtlbs), lbs = pvtlbs))
            }
            if (all.equal(pvtlbs, dimnames(argl[[i]])[[2]]) == 
                TRUE) {
                argdf <- rbind(argdf, matrix(as.vector(argl[[i]]), 
                  ncol = ncol(argdf), byrow = TRUE))
            }
            else {
                tempargdf <- matrix(nrow = dim(pvt0)[1], ncol = dim(pvt0)[2], 
                  dimnames = list(pvtlbs, pvtlbs))
                if (isTRUE(i < length(argl)) == TRUE) {
                  tempargdf <- array(dim = c(dim(pvt0)[1], dim(pvt0)[2], 
                    dim(argl[[i + k]])[3]), dimnames = list(pvtlbs, 
                    pvtlbs))
                }
                tempargdf[which(pvtlbs %in% dimnames(argl[[i]])[[1]]), 
                  which(pvtlbs %in% dimnames(argl[[i]])[[1]]), 
                  ] <- argl[[i]]
                argdf <- rbind(argdf, matrix(as.vector(tempargdf), 
                  ncol = ncol(argdf), byrow = TRUE))
            }
        }
        rm(i)
        argdf[sapply(argdf, is.na)] <- 0L
        arr <- array(dim = c(dim(pvt0)[1], dim(pvt0)[2], nrow(argdf)), 
            dimnames = list(pvtlbs, pvtlbs))
        for (i in seq_len(nrow(argdf))) {
            arr[, , i][seq_len((dim(pvt0)[1] * dim(pvt0)[2]))] <- as.numeric(argdf[i, 
                ])
        }
        rm(i)
        if (any(unlist(lapply(lapply(argl, dim), length)) != 
            3L) == FALSE) {
            pdim <- unlist(lapply(argl, function(z) {
                dimnames(z)[[3]]
            }))
            ifelse(isTRUE(length(pdim) == dim(arr)[3]) == TRUE, 
                dimnames(arr)[[3]] <- pdim, NA)
            rm(pdim)
        }
        if (isTRUE(sort == TRUE) == TRUE) {
            return(perm(arr, sort = TRUE))
        }
        else {
            return(arr)
        }
    }
    else {
        message("An(some) array(s) is(are) extended.")
    }
    if (isTRUE(flgx == TRUE) == TRUE) {
        argdf <- data.frame(matrix(ncol = (length(pvtlbs) * length(pvtlbs)), 
            nrow = 0L))
        if (is.na(dim(argl[[1]])[3]) == TRUE) {
            if (all(pvtlbs %in% dimnames(pvt0)[[1]]) == TRUE) {
                argdf <- rbind(argdf, matrix(pvt0, ncol = ncol(argdf), 
                  byrow = TRUE))
            }
            else {
                tmp1 <- transf(pvt0, type = "toarray", add = pvtlbs[which(!(pvtlbs %in% 
                  dimnames(pvt0)[[1]]))])
                if (identical(dimnames(tmp1)[[1]], pvtlbs) == 
                  TRUE) {
                  argdf <- rbind(argdf, matrix(tmp1, ncol = ncol(argdf), 
                    byrow = TRUE))
                }
                else {
                  tmplbs <- dimnames(tmp1)[[1]]
                  vec <- vector()
                  for (i in seq_len(length(tmplbs))) {
                    vec <- append(vec, which(pvtlbs %in% tmplbs[i]))
                  }
                  rm(i)
                  tmpdf <- perm(tmp1, clu = vec)
                  argdf <- rbind(argdf, matrix(tmpdf, ncol = ncol(argdf), 
                    byrow = TRUE))
                }
            }
        }
        else {
            if (identical(dimnames(argl[[1]])[[1]], pvtlbs) == 
                TRUE) {
                for (j in seq_len(dim(argl[[1]])[3])) {
                  argdf <- rbind(argdf, matrix(argl[[1]][, , 
                    j], ncol = ncol(argdf), byrow = TRUE))
                }
                rm(j)
            }
            else {
                for (j in seq_len(dim(argl[[1]])[3])) {
                  tmp3 <- transf(argl[[1]][, , j], type = "toarray", 
                    add = pvtlbs[which(!(pvtlbs %in% dimnames(pvt0)[[1]]))])
                  argdf <- rbind(argdf, matrix(tmp3, ncol = ncol(argdf), 
                    byrow = TRUE))
                }
                rm(j)
            }
        }
        for (k in 2:length(argl)) {
            if (is.na(dim(argl[[k]])[3]) == TRUE) {
                tmpk <- transf(argl[[k]], type = "toarray", add = pvtlbs[which(!(pvtlbs %in% 
                  dimnames(argl[[k]])[[1]]))])
                if (identical(dimnames(tmpk)[[1]], pvtlbs) == 
                  TRUE) {
                  argdf <- rbind(argdf, matrix(tmpk, ncol = ncol(argdf), 
                    byrow = TRUE))
                }
                else {
                  tmplbs <- dimnames(tmpk)[[1]]
                  vek <- vector()
                  for (i in seq_len(length(tmplbs))) {
                    vek <- append(vek, which(pvtlbs %in% tmplbs[i]))
                  }
                  rm(i)
                  tmpdf <- perm(tmpk, clu = vek)
                  argdf <- rbind(argdf, matrix(tmpdf, ncol = ncol(argdf), 
                    byrow = TRUE))
                }
            }
            else {
                for (j in seq_len(dim(argl[[k]])[3])) {
                  tmpk <- transf(argl[[k]][, , j], type = "toarray", 
                    add = pvtlbs[which(!(pvtlbs %in% dimnames(argl[[k]])[[1]]))])
                  tmplbs <- dimnames(tmpk)[[1]]
                  vec <- vector()
                  for (i in seq_len(length(tmplbs))) {
                    vec <- append(vec, which(pvtlbs %in% tmplbs[i]))
                  }
                  rm(i)
                  tmpdf <- perm(tmpk, clu = vec)
                  argdf <- rbind(argdf, matrix(tmpdf, ncol = ncol(argdf), 
                    byrow = TRUE))
                }
                rm(j)
            }
        }
        rm(k)
        argdf[sapply(argdf, is.na)] <- 0L
        arr <- array(dim = c(length(pvtlbs), length(pvtlbs), 
            nrow(argdf)), dimnames = list(pvtlbs, pvtlbs))
        for (i in seq_len(nrow(argdf))) {
            arr[, , i][seq_len((length(pvtlbs) * length(pvtlbs)))] <- as.numeric(argdf[i, 
                ])
        }
        rm(i)
        if (any(unlist(lapply(lapply(argl, dim), length)) != 
            3L) == FALSE) {
            pdim <- unlist(lapply(argl, function(z) {
                dimnames(z)[[3]]
            }))
            ifelse(isTRUE(length(pdim) == dim(arr)[3]) == TRUE, 
                dimnames(arr)[[3]] <- pdim, NA)
            rm(pdim)
        }
        if (isTRUE(sort == TRUE) == TRUE) {
            return(perm(arr, sort = TRUE))
        }
        else {
            return(arr)
        }
    }
}
