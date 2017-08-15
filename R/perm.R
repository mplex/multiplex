perm <-
function (x, clu, rev = FALSE, lbs) 
{
    if (isTRUE(is.array(x) == TRUE) == FALSE) 
        stop("'x' must be an array object.")
    if (isTRUE(length(clu) != dim(x)[1]) == TRUE) 
        stop("'clu' does not match the order of 'x'.")
    if (is.character(clu) == FALSE) {
        ifelse(isTRUE(0L %in% as.numeric(levels(factor(clu)))) == 
            TRUE, clu <- clu + 1L, NA)
    }
    else if (is.character(clu) == TRUE) {
        tmp <- clu
        for (i in seq_len(nlevels(factor(clu)))) {
            clu[which(levels(factor(tmp))[i] == clu)] <- i
        }
        rm(i)
        clu <- methods::as(clu, "numeric")
        rm(tmp)
    }
    else {
        NA
    }
    clu[which(is.na(clu))] <- max(as.numeric(levels(factor(clu)))) + 
        1L
    or <- list()
    for (i in as.numeric(levels(factor(clu)))) {
        or[[i]] <- which(clu == i)
    }
    rm(i)
    if (isTRUE(any(is.na(unlist(or)))) == TRUE) {
        for (i in seq_len(length(or))) {
            ifelse(isTRUE(is.null(or[[i]])) == TRUE, or[[i]] <- NA, 
                NA)
        }
        rm(i)
        nor <- as.vector(stats::na.omit(unlist(or)))
    }
    else {
        nor <- order(unlist(or))
    }
    if (isTRUE(is.na(dim(x)[3]) == TRUE)) {
        prm <- vector()
        for (i in nor) {
            prm[nor[i]] <- i
        }
        rm(i)
        ifelse(isTRUE(rev == TRUE) == TRUE, px <- x[rev(prm), 
            rev(prm)], px <- x[prm, prm])
        ifelse(missing(lbs) == FALSE && isTRUE(length(lbs) == 
            dim(x)[1]) == TRUE, dimnames(px)[[1]] <- dimnames(px)[[2]] <- lbs, 
            NA)
        return(px)
    }
    else {
        px <- x
        for (k in seq_len(dim(px)[3])) {
            prm <- vector()
            for (i in nor) {
                prm[nor[i]] <- i
            }
            rm(i)
            px[, , k] <- px[prm, prm, k]
        }
        rm(k)
        if (missing(lbs) == FALSE && isTRUE(length(lbs) == dim(x)[1]) == 
            TRUE) {
            dimnames(px)[[1]] <- dimnames(px)[[2]] <- lbs
        }
        else if (is.null(dimnames(x)[[1]]) == FALSE) {
            Lbs <- vector()
            length(Lbs) <- length(clu)
            for (i in seq_len(length(nor))) Lbs[i] <- dimnames(x)[[1]][which(nor == 
                i)]
            dimnames(px)[[1]] <- dimnames(px)[[2]] <- Lbs
        }
        else {
            NA
        }
        ifelse(isTRUE(is.null(dimnames(x)[[3]]) == FALSE), dimnames(px)[[3]] <- dimnames(x)[[3]], 
            NA)
        return(px)
    }
}
