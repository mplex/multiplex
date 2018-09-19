mlvl <-
function (x, y, binomProy = TRUE, modes) 
{
    qmd <- vector()
    if (is.array(x) == TRUE && is.na(dim(x)[3]) == FALSE) {
        xx <- list()
        for (i in seq_len(dim(x)[3])) {
            xx[[i]] <- x[, , i]
        }
        rm(i)
        attr(xx, "names") <- attr(x, "dimnames")[[3]]
        qmd <- append(qmd, rep("1M", dim(x)[3]))
    }
    else {
        if (is.list(x) == TRUE) {
            xx <- x
            ifelse(is.null(attr(x, "names")) == TRUE, attr(xx, 
                "names") <- seq_len(length(x)), NA)
            qmd <- append(qmd, rep("1M", length(x)))
        }
        else {
            xx <- zbind(x)
            attr(xx, "names") <- "1"
            qmd <- append(qmd, "1M")
        }
    }
    if (is.null(y) == TRUE) {
        stop("2-mode network should be place in \"y\".")
    }
    else {
        if (is.array(y) == TRUE && is.na(dim(y)[3]) == FALSE) {
            yy <- list()
            for (i in seq_len(dim(y)[3])) {
                yy[[i]] <- y[, , i]
            }
            rm(i)
            attr(yy, "names") <- attr(y, "dimnames")[[3]]
            qmd <- append(qmd, rep("2M", dim(y)[3]))
        }
        else {
            if (is.list(y) == TRUE && is.data.frame(y) == FALSE) {
                yy <- y
                ifelse(is.null(attr(y, "names")) == TRUE, attr(yy, 
                  "names") <- seq_len(length(y)), NA)
                qmd <- append(qmd, rep("2M", length(y)))
            }
            else {
                yy <- zbind(y)
                attr(yy, "names") <- length(xx) + 1L
                qmd <- append(qmd, "2M")
            }
        }
    }
    X <- c(xx, yy)
    if (is.list(y) == TRUE && is.data.frame(y) == FALSE) {
        ifelse(any(rownames(yy[[1]]) %in% colnames(yy[[2]])) == 
            TRUE, Lbs <- list(dm = sort(unique(c(colnames(yy[[1]]), 
            rownames(yy[[2]])))), cdm = sort(unique(c(rownames(yy[[1]]), 
            colnames(yy[[2]]))))), Lbs <- list(dm = sort(unique(c(rownames(yy[[1]]), 
            rownames(yy[[2]])))), cdm = sort(unique(c(colnames(yy[[1]]), 
            colnames(yy[[2]]))))))
    }
    else {
        if (isTRUE(length(yy) > 1) == TRUE) {
            ifelse(any(rownames(X[[which(qmd == "2M")]]) %in% 
                rownames(X[[which(qmd == "1M")]])) == TRUE, Lbs <- list(dm = sort(unique(rownames(X[[which(qmd == 
                "1M")]]))), cdm = sort(unique(rownames(t(X[[which(qmd == 
                "2M")]]))))), Lbs <- list(dm = sort(unique(rownames(X[[which(qmd == 
                "2M")]]))), cdm = sort(unique(rownames(X[[which(qmd == 
                "1M")]])))))
        }
        else {
            Lbs <- list(dm = sort(rownames(y)), cdm = sort(colnames(y)))
        }
    }
    if (isTRUE(binomProy == TRUE) == TRUE) {
        vcn <- vector()
        for (i in seq_len(length(X))) {
            vcn <- append(vcn, dimnames(X[[i]])[[1]])
            vcn <- append(vcn, dimnames(X[[i]])[[2]])
        }
        rm(i)
        bmlbs <- unique(vcn)
        bmat <- transf(X[[1]], "toarray2", lbs = bmlbs)
        for (k in seq(from = 2, to = length(X))) {
            bmat <- zbnd(bmat, transf(X[[k]], "toarray2", lbs = bmlbs))
        }
        rm(k)
        dimnames(bmat)[[1]] <- dimnames(bmat)[[2]] <- bmlbs
        dimnames(bmat)[[3]] <- attr(X, "names")
    }
    else {
        bmat <- X
    }
    if (missing(modes) == FALSE && isTRUE(modes == FALSE) == 
        TRUE) {
        class(bmat) <- "Multilevel"
        return(bmat)
    }
    else {
        lst <- list(binomProy = binomProy, bmat = bmat, modes = qmd, 
            lbs = Lbs)
        class(lst) <- "Multilevel"
        return(lst)
    }
}
