reduc <-
function (x, clu, lbs = NULL) 
{
    if (isTRUE(is.array(x) == TRUE) == FALSE) 
        stop("'x' must be an array object.")
    if (isTRUE(length(clu) != dim(x)[1]) == TRUE) 
        stop("'clu' does not match the order of 'x'.")
    if (is.character(clu) == TRUE || is.factor(clu) == TRUE) {
        tmp <- as.vector(clu)
        for (i in seq_len(nlevels(factor(clu)))) {
            tmp[which(levels(factor(clu))[i] == tmp)] <- i
        }
        rm(i)
        clu <- as.numeric(tmp)
        rm(tmp)
    }
    else if (is.character(clu) == FALSE && is.factor(clu) == 
        FALSE) {
        ifelse(isTRUE(0L %in% as.numeric(levels(factor(clu)))) == 
            TRUE, clu <- clu + 1L, NA)
    }
    else {
        NA
    }
    ifelse(NA %in% clu, clu[which(is.na(clu))] <- max(as.numeric(levels(factor(clu)))) + 
        1L, NA)
    lngt <- nlevels(factor(clu))
    or <- list()
    for (i in as.numeric(levels(factor(clu)))) {
        or[[i]] <- which(clu == i)
    }
    rm(i)
    cls <- list()
    length(cls) <- lngt
    k <- 1L
    for (i in seq_len(length(or))) {
        if (isTRUE(is.null(or[[i]])) == FALSE) {
            cls[[k]] <- or[[i]]
            k <- k + 1L
        }
        else {
            NA
        }
    }
    rm(i, k)
    if (isTRUE(is.na(dim(x)[3]) == TRUE)) {
        if (isTRUE(is.null(dimnames(x)[[1]]) == TRUE) == TRUE) 
            dimnames(x)[[1]] <- dimnames(x)[[2]] <- seq_len(nrow(x))
        bm <- array(dim = c(lngt, lngt))
        for (i in seq_len(lngt)) {
            for (j in seq_len(lngt)) {
                bm[i, j] <- sum(x[cls[[i]], cls[[j]]])
            }
        }
        rm(i, j)
        bm <- dichot(bm)
        ifelse(is.null(lbs) == FALSE, rownames(bm) <- colnames(bm) <- lbs, 
            NA)
        return(bm)
    }
    else if (isTRUE(is.na(dim(x)[3]) == FALSE)) {
        px <- x
        bm <- array(dim = c(lngt, lngt, dim(x)[3]))
        for (k in seq_len(dim(x)[3])) {
            for (i in seq_len(lngt)) {
                for (j in seq_len(lngt)) {
                  bm[i, j, k] <- sum(px[cls[[i]], cls[[j]], k])
                }
            }
            rm(i, j)
        }
        rm(k)
        bm <- dichot(bm, c = 1L)
        if (is.null(lbs) == FALSE) {
            dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- lbs
        }
        else {
            dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- unique(clu)
        }
        if (is.null(dimnames(x)[[3]]) == FALSE) 
            dimnames(bm)[[3]] <- dimnames(x)[[3]]
        return(bm)
    }
}
