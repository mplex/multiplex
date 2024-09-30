reduc <-
function (x, clu, lbs = NULL, slbs = NULL, valued, row, col) 
{
    ifelse(missing(valued) == FALSE && isTRUE(valued == TRUE) == 
        TRUE, valued <- TRUE, valued <- FALSE)
    ifelse(missing(row) == FALSE && isTRUE(row == TRUE) == TRUE, 
        row <- TRUE, row <- FALSE)
    ifelse(missing(col) == FALSE && isTRUE(col == TRUE) == TRUE, 
        col <- TRUE, col <- FALSE)
    if (isTRUE(length(clu) != dim(x)[1]) == TRUE && isTRUE(col == 
        FALSE) == TRUE) 
        stop("'clu' does not match the order of 'x'.")
    if (isTRUE(row == FALSE) == TRUE && isTRUE(col == FALSE) == 
        TRUE) {
        if (isTRUE(is.array(x) == TRUE) == FALSE) 
            stop("'x' must be an array object.")
    }
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
        ifelse(isTRUE(is.null(dimnames(x)[[1]]) == TRUE) == TRUE, 
            dimnames(x)[[1]] <- dimnames(x)[[2]] <- seq_len(nrow(x)), 
            NA)
        if (isTRUE(row == FALSE) == TRUE && isTRUE(col == FALSE) == 
            TRUE) {
            bm <- array(dim = c(lngt, lngt))
            for (i in seq_len(lngt)) {
                for (j in seq_len(lngt)) {
                  bm[i, j] <- sum(x[cls[[i]], cls[[j]]])
                }
            }
            rm(i, j)
            ifelse(is.null(lbs) == FALSE, rownames(bm) <- colnames(bm) <- lbs, 
                NA)
        }
        else if (isTRUE(row == TRUE) == TRUE) {
            bm <- array(dim = c(lngt, ncol(x)))
            colnames(bm) <- colnames(x)
            for (i in seq_len(lngt)) {
                bm[i, ] <- colSums(x[cls[[i]], ])
            }
            rm(i)
            ifelse(is.null(lbs) == TRUE, NA, rownames(bm) <- lbs)
        }
        else if (isTRUE(col == TRUE) == TRUE) {
            bm <- array(dim = c(nrow(x), lngt))
            rownames(bm) <- rownames(x)
            for (i in seq_len(lngt)) {
                ifelse(isTRUE(length(cls[[i]]) == 1) == TRUE, 
                  bm[, i] <- x[, cls[[i]]], bm[, i] <- rowSums(x[, 
                    cls[[i]]]))
            }
            rm(i)
            ifelse(is.null(lbs) == TRUE, NA, colnames(bm) <- lbs)
        }
        else {
            NA
        }
    }
    else if (isTRUE(is.na(dim(x)[3]) == FALSE)) {
        bm <- array(dim = c(lngt, lngt, dim(x)[3]))
        if (is.null(lbs) == FALSE) {
            dimnames(bm)[[1]] <- dimnames(bm)[[2]] <- lbs
        }
        if (is.null(slbs) == FALSE) {
            dimnames(bm)[[3]] <- slbs
        }
        else {
            if (is.null(dimnames(x)[[3]]) == FALSE) 
                dimnames(bm)[[3]] <- dimnames(x)[[3]]
        }
        for (k in seq_len(dim(x)[3])) {
            for (i in seq_len(lngt)) {
                for (j in seq_len(lngt)) {
                  bm[i, j, k] <- sum(x[cls[[i]], cls[[j]], k])
                }
                rm(j)
            }
            rm(i)
        }
        rm(k)
    }
    ifelse(isTRUE(valued == TRUE) == TRUE, return(bm), return(dichot(bm, 
        c = 1L)))
}
