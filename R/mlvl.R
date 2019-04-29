mlvl <-
function (x = NULL, y = NULL, type = c("bpn", "cn", "cn2"), symCdm, 
    diag, lbs) 
{
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
            stop("\"y\" is missing or has a not valid format.")
        }
        if (match.arg(type) == "cn2") {
            ifelse(is.matrix(x) == TRUE || is.array(x) == TRUE || 
                is.null(x) == FALSE, cdmat <- zbind(x, cdmat), 
                stop("\"x\" is missing or has a not valid format."))
            ifelse(missing(lbs) == FALSE && isTRUE(length(lbs) == 
                dim(cdmat)[3]) == TRUE, dimnames(cdmat)[[3]] <- lbs, 
                NA)
        }
    }
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
    if (is.null(y) == FALSE) {
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
    else {
        stop("A 2-mode network should be placed in \"y\".")
    }
    ifelse(match.arg(type) == "cn", qmd <- "1M", NA)
    if (is.list(y) == TRUE && is.data.frame(y) == FALSE) {
        ifelse(any(rownames(yy[[1]]) %in% colnames(yy[[2]])) == 
            TRUE, Lbs <- list(dm = (unique(c(colnames(yy[[1]]), 
            rownames(yy[[2]])))), cdm = (unique(c(rownames(yy[[1]]), 
            colnames(yy[[2]]))))), Lbs <- list(dm = (unique(c(rownames(yy[[1]]), 
            rownames(yy[[2]])))), cdm = (unique(c(colnames(yy[[1]]), 
            colnames(yy[[2]]))))))
    }
    else {
        if (isTRUE(length(yy) > 1) == TRUE) {
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
    X <- list(xx, yy)
    if (match.arg(type) == "bpn") {
        vcn <- vector()
        for (i in seq_len(length(qmd))) {
            vcn <- append(vcn, dimnames(X[[2]][[1]])[[1]])
            vcn <- append(vcn, dimnames(X[[2]][[1]])[[2]])
        }
        rm(i)
        bmlbs <- unique(vcn)
        bmat <- transf(dichot(as.array(X[[1]][[1]]), c = 1), 
            type = "toarray2", lbs = bmlbs)
        bmat[1:dim(x)[1], 1:dim(x)[2]] <- as.array(X[[1]][[1]])
        for (k in seq(from = 2, to = max(which(qmd == "1M")))) {
            bmat <- zbnd(bmat, transf(dichot(as.array(X[[1]][[k]]), 
                c = 1), type = "toarray2", lbs = bmlbs))
            bmat[1:dim(x)[1], 1:dim(x)[2], k] <- as.array(X[[1]][[k]])
        }
        rm(k)
        for (k in seq_len(length(X[[2]]))) {
            bmat <- zbnd(bmat, transf(dichot(X[[2]][[k]], c = 1), 
                type = "toarray2", lbs = bmlbs))
        }
        rm(k)
        dimnames(bmat)[[1]] <- dimnames(bmat)[[2]] <- bmlbs
        dimnames(bmat)[[3]] <- attr(c(xx, yy), "names")
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
    ifelse(missing(lbs) == FALSE && isTRUE(length(lbs) == dim(bmat)[3]) == 
        TRUE, dimnames(bmat)[[3]] <- lbs, NA)
    if (match.arg(type) == "bpn") {
        lst <- list(mlnet = bmat, lbs = Lbs, modes = qmd)
    }
    else {
        lst <- list(mlnet = cdmat, lbs = Lbs, modes = qmd)
    }
    class(lst) <- c("Multilevel", match.arg(type))
    return(lst)
}
