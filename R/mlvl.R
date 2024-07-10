mlvl <-
function (x = NULL, y = NULL, type = c("bpn", "cn", "cn2", "list"), 
    symCdm, diag, lbs) 
{
    if (isTRUE("pathfinder" %in% attr(x, "class")) == TRUE) {
        x <- x$Q
    }
    else {
        NA
    }
    ifelse(match.arg(type) == "cn", qmd <- "1M", NA)
    if (is.null(x) == FALSE) {
        ifelse(is.null(dimnames(x)[[1]]) == TRUE, dimnames(x)[[1]] <- seq_len(dim(x)[1]), 
            NA)
        ifelse(is.null(dimnames(x)[[2]]) == TRUE, dimnames(x)[[2]] <- seq_len(dim(x)[2]), 
            NA)
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
                xx <- list(x)
                attr(xx, "names") <- "1"
                qmd <- append(qmd, "1M")
            }
        }
        if (match.arg(type) == "bpn") {
            vcn <- vector()
            for (k in seq_len(length(xx))) {
                vcn <- append(vcn, c(dimnames(xx[[k]])[[1]], 
                  dimnames(xx[[k]])[[2]]))
            }
            rm(k)
        }
    }
    else {
        NA
    }
    if (is.null(y) == FALSE) {
        ifelse(is.null(dimnames(y)[[1]]) == TRUE, dimnames(y)[[1]] <- seq_len(dim(y)[1]), 
            NA)
        ifelse(is.null(dimnames(y)[[2]]) == TRUE, dimnames(y)[[2]] <- seq_len(dim(y)[2]), 
            NA)
        if (is.array(y) == TRUE && is.na(dim(y)[3]) == FALSE) {
            yy <- list()
            for (i in seq_len(dim(y)[3])) {
                yy[[i]] <- y[, , i]
            }
            rm(i)
            attr(yy, "names") <- attr(y, "dimnames")[[3]]
            qmd <- append(qmd, rep("2M", dim(y)[3]))
        }
        else if (is.list(y) == TRUE && is.data.frame(y) == FALSE) {
            for (k in seq_len(length(y))) {
                vcn <- append(vcn, c(dimnames(y[[k]])[[1]], dimnames(y[[k]])[[2]]))
            }
            rm(k)
            yytmp <- transf(transf(y, type = "tolist", lb2lb = TRUE), 
                type = "toarray", lbs = unique(vcn))
            yy <- list()
            for (k in seq_len(dim(yytmp)[3])) {
                yy[[k]] <- yytmp[, , k]
            }
            rm(k)
            ifelse(is.null(attr(y, "names")) == TRUE, attr(yy, 
                "names") <- seq_len(length(y)), attr(yy, "names") <- attr(y, 
                "names"))
            qmd <- append(qmd, rep("2M", length(y)))
        }
        else {
            ifelse(is.data.frame(y) == TRUE, y <- as.matrix(y), 
                NA)
            yy <- list(y)
            if (is.null(x) == FALSE) {
                attr(yy, "names") <- length(xx) + 1L
            }
            qmd <- append(qmd, "2M")
            ifelse(match.arg(type) == "bpn", vcn <- append(vcn, 
                c(dimnames(y)[[1]], dimnames(y)[[2]])), NA)
        }
    }
    else {
        stop("A 2-mode network must be placed in \"y\".")
    }
    if (match.arg(type) == "cn" || match.arg(type) == "cn2") {
        if (is.null(y) == FALSE && (is.matrix(y) == TRUE || is.array(y) == 
            TRUE || is.data.frame(y) == TRUE)) {
            cdmat <- array(0, dim = c(nrow(y), nrow(y)), dimnames = list(rownames(y), 
                rownames(y)))
            for (k in seq_len(ncol(y))) {
                af <- which(y[, k] > 0)
                for (i in af) {
                  for (j in seq_len(length(af))) {
                    cdmat[i, af[j]] <- cdmat[i, af[j]] + 1L
                  }
                  rm(j)
                }
                rm(i)
            }
            rm(k)
            ifelse(missing(diag) == FALSE && isTRUE(diag == TRUE) == 
                TRUE, NA, diag(cdmat) <- 0L)
        }
        else {
            stop("\"y\" is missing or it has a not valid format.")
        }
        if (match.arg(type) == "cn2") {
            if ((is.matrix(x) == TRUE || is.array(x) == TRUE || 
                is.null(x) == FALSE) && isTRUE(dim(cdmat)[1] == 
                dim(x)[1]) == TRUE) {
                cdmat <- zbind(x, cdmat, force = TRUE)
            }
            else {
                stop("\"x\" is missing or it has a not valid format.")
            }
            ifelse(missing(lbs) == FALSE && isTRUE(length(lbs) == 
                dim(cdmat)[3]) == TRUE, dimnames(cdmat)[[3]] <- lbs, 
                NA)
        }
    }
    if (is.null(x) == FALSE) {
        X <- list(xx, yy)
    }
    else {
        X <- yy
    }
    if (is.list(y) == TRUE && is.data.frame(y) == FALSE) {
        ifelse(any(rownames(yy[[1]]) %in% colnames(yy[[2]])) == 
            TRUE, Lbs <- list(dm = (unique(c(colnames(yy[[1]]), 
            rownames(yy[[2]])))), cdm = (unique(c(rownames(yy[[1]]), 
            colnames(yy[[2]]))))), Lbs <- list(dm = (unique(c(rownames(yy[[1]]), 
            rownames(yy[[2]])))), cdm = (unique(c(colnames(yy[[1]]), 
            colnames(yy[[2]]))))))
    }
    else {
        if (isTRUE(length(yy) > 1) == TRUE && is.list(y) == TRUE) {
            ifelse(any(rownames(X[[which(qmd == "2M")]]) %in% 
                rownames(X[[which(qmd == "1M")]])) == TRUE, Lbs <- list(dm = (unique(rownames(X[[which(qmd == 
                "1M")]]))), cdm = (unique(rownames(t(X[[which(qmd == 
                "2M")]]))))), Lbs <- list(dm = (unique(rownames(X[[which(qmd == 
                "2M")]]))), cdm = (unique(rownames(X[[which(qmd == 
                "1M")]])))))
        }
        else {
            Lbs <- list(dm = (rownames(y)), cdm = (colnames(y)))
        }
    }
    if (match.arg(type) == "bpn") {
        bmlbs <- unique(vcn)
        n <- length(bmlbs)
        bmat <- array(0, dim = c(n, n, length(X[[1]]) + length(X[[2]])), 
            dimnames = list(bmlbs, bmlbs))
        if (isTRUE(dim(X[[1]][[1]])[1] < n) == TRUE) {
            for (k in seq_len(length(X[[1]]))) {
                bmat[which(bmlbs %in% Lbs[[1]]), which(bmlbs %in% 
                  Lbs[[1]]), k] <- X[[1]][[k]]
            }
            rm(k)
        }
        else {
            for (k in seq_len(length(X[[1]]))) {
                bmat[, , k] <- X[[1]][[k]]
            }
            rm(k)
        }
        if (isTRUE(dim(X[[2]][[1]])[1] != n) == TRUE || isTRUE(dim(X[[2]][[1]])[2] != 
            n) == TRUE) {
            for (k in seq_len(length(X[[2]]))) {
                bmat[, , length(X[[1]]) + k][which(dimnames(bmat)[[1]] %in% 
                  Lbs$dm), which(dimnames(bmat)[[2]] %in% Lbs$cdm)] <- X[[2]][[k]]
            }
            rm(k)
        }
        else {
            for (k in seq_len(length(X[[2]]))) {
                bmat[, , length(X[[1]]) + k] <- X[[2]][[k]]
            }
            rm(k)
        }
        if (missing(lbs) == FALSE && isTRUE(length(lbs) == dim(bmat)[3]) == 
            TRUE) {
            dimnames(bmat)[[3]] <- lbs
        }
        else {
            dimnames(bmat)[[3]] <- attr(c(xx, yy), "names")
        }
        if (missing(symCdm) == FALSE && isTRUE(symCdm == TRUE) == 
            TRUE) {
            for (i in which(qmd == "2M")) {
                bmat[, , i] <- bmat[, , i] + t(bmat[, , i])
            }
            rm(i)
        }
    }
    else {
        bmat <- X
    }
    ifelse(missing(lbs) == FALSE && is.list(lbs) == TRUE, Lbs <- list(dm = lbs[[1]], 
        cdm = lbs[[2]]), NA)
    if (match.arg(type) == "bpn") {
        lst <- list(mlnet = bmat, lbs = Lbs, modes = qmd)
    }
    else {
        lst <- list(mlnet = cdmat, lbs = Lbs, modes = qmd)
    }
    class(lst) <- c("Multilevel", match.arg(type))
    return(lst)
}
