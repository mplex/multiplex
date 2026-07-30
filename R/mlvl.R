## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
mlvl <-
function (x, y, type = c("bpn", "cn", "cn2"), symCdm, diag, lbs) 
{
    if (missing(x) == FALSE && match.arg(type) != "cn") {
        if (isTRUE("pathfinder" %in% attr(x, "class")) == TRUE) {
            x <- x$Q
        }
        else {
            if (any(class(x) %in% c("matrix", "array", "list")) == 
                FALSE) 
                stop(paste("Invalid class of ", sQuote(deparse(substitute(x))), 
                  ".", sep = ""))
        }
        if (is.null(dim(x)) == FALSE) {
            ifelse(is.null(dimnames(x)[[1]]) == TRUE, dimnames(x)[[1]] <- seq_len(dim(x)[1]), 
                NA)
            ifelse(is.null(dimnames(x)[[2]]) == TRUE, dimnames(x)[[2]] <- seq_len(dim(x)[2]), 
                NA)
        }
        if (is.null(dim(x)) == TRUE || is.list(x) == TRUE) {
            if (all(lapply(x, class) == "character") == TRUE) {
                xx <- transf(x, type = "toarray")
            }
            else {
                xx <- transf(lapply(x, function(z) {
                  trnf(z, tolist = TRUE, lb2lb = TRUE)
                }), type = "toarray")
            }
        }
        else {
            xx <- x
        }
        ifelse(is.na(dim(xx)[3]) == TRUE, qmd <- "1M", qmd <- rep("1M", 
            dim(xx)[3]))
    }
    else {
        if (missing(y) == TRUE) 
            stop(paste("Missing ", sQuote("x"), " and ", sQuote("y"), 
                ".", sep = ""))
    }
    if (missing(y) == FALSE) {
        if (is.array(y) == TRUE) {
            ifelse(is.null(dimnames(y)[[1]]) == TRUE, dimnames(y)[[1]] <- seq_len(dim(y)[1]), 
                NA)
            ifelse(is.null(dimnames(y)[[2]]) == TRUE, dimnames(y)[[2]] <- seq_len(dim(y)[2]), 
                NA)
        }
        if (match.arg(type) == "bpn" || match.arg(type) == "cn2") {
            if (is.data.frame(y) == TRUE) {
                yy <- transf(y, type = "toarray", lbs = c(rownames(y), 
                  colnames(y)))
            }
            else if (is.list(y) == FALSE) {
                yy <- transf(transf(y, type = "tolist", lb2lb = TRUE), 
                  type = "toarray", lbs = c(rownames(y), colnames(y)))
            }
            else {
                yy <- transf(lapply(y, function(z) {
                  transf(z, type = "tolist", lb2lb = TRUE)
                }), type = "toarray")
            }
            ifelse(is.na(dim(yy)[3]) == TRUE, qmd <- append(qmd, 
                "2M"), qmd <- append(qmd, rep("2M", dim(yy)[3])))
        }
        else {
            if (is.matrix(y) == TRUE || is.array(y) == TRUE || 
                is.data.frame(y) == TRUE) {
                cdmat <- array(0, dim = c(nrow(y), nrow(y)), 
                  dimnames = list(rownames(y), rownames(y)))
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
                ifelse(missing(diag) == FALSE && isTRUE(diag == 
                  TRUE) == TRUE, NA, diag(cdmat) <- 0L)
            }
            else if (is.list(y) == TRUE) {
                cdmat <- lapply(y, function(z) {
                  cdm <- array(0, dim = c(nrow(z), nrow(z)), 
                    dimnames = list(rownames(z), rownames(z)))
                  for (k in seq_len(ncol(z))) {
                    af <- which(z[, k] > 0)
                    for (i in af) {
                      for (j in seq_len(length(af))) {
                        cdm[i, af[j]] <- cdm[i, af[j]] + 1L
                      }
                      rm(j)
                    }
                    rm(i)
                  }
                  rm(k)
                  return(cdm)
                })
            }
            else {
                stop(paste(sQuote("y"), " has invalid format.", 
                  sep = ""))
            }
        }
    }
    if (missing(lbs) == FALSE && (is.list(lbs) == TRUE && isTRUE(length(lbs) == 
        2) == TRUE)) {
        if ((is.matrix(y) == TRUE && all(dimnames(y)[[2]] %in% 
            lbs[[1]]) == FALSE) || (is.data.frame(y) == TRUE && 
            all(rownames(y) %in% lbs[[1]]) == FALSE) || (is.list(y) == 
            TRUE && all(rownames(y[[1]]) %in% lbs[[1]]) == FALSE)) {
            warning(paste("Domain labels for ", sQuote(deparse(substitute(y))), 
                " do not correspond to specified labels in ", 
                sQuote("lbs"), ".", sep = ""), call. = FALSE)
        }
        if ((is.matrix(x) == TRUE && all(dimnames(x)[[1]] %in% 
            lbs[[2]]) == FALSE) || (is.array(x) == TRUE && all(unique(unlist(dimnames(x)[1:2])) %in% 
            lbs[[2]]) == FALSE) || (is.list(x) == TRUE && all(unique(unlist(lapply(x, 
            dimnames), use.names = FALSE)) %in% lbs[[2]]) == 
            FALSE)) {
            warning(paste("Codomain labels for ", sQuote(deparse(substitute(x))), 
                " do not correspond to specified labels in ", 
                sQuote("lbs"), ".", sep = ""), call. = FALSE)
        }
        Lbs <- list(dm = lbs[[1]], cdm = lbs[[2]])
    }
    else if (match.arg(type) != "cn") {
        if (is.matrix(x) == TRUE) {
            dm <- dimnames(x)[[1]]
        }
        else if (is.array(x) == TRUE) {
            dm <- unique(unlist(dimnames(x)[1:2]))
        }
        else if (is.list(x) == TRUE) {
            dm <- unique(unlist(lapply(x, dimnames), use.names = FALSE))
        }
        else {
            dm <- NULL
        }
        if (is.matrix(y) == TRUE) {
            cdm <- dimnames(y)[[2]]
        }
        else if (is.data.frame(y) == TRUE) {
            cdm <- colnames(y)
        }
        else if (is.list(y) == TRUE) {
            cdm <- unique(unlist(lapply(y, dimnames), use.names = FALSE))
        }
        else {
            cdm <- NULL
        }
        if (any(dm %in% cdm) == TRUE) {
            warning(paste("Need domain/codomain labels in ", 
                sQuote("lbs"), " for ", sQuote(deparse(substitute(x))), 
                " and ", sQuote(deparse(substitute(y))), " since `", 
                paste(intersect(dm[which(dm %in% cdm)], cdm[which(cdm %in% 
                  dm)]), collapse = "`,`"), "` appear in both domains.", 
                sep = ""), call. = FALSE)
        }
        Lbs <- list(dm = dm, cdm = cdm)
    }
    else {
        Lbs <- list(dm = rownames(y), cdm = colnames(y))
    }
    if (match.arg(type) == "bpn") {
        if (is.data.frame(yy) == TRUE) {
            bmat <- suppressMessages(zbind(xx, as.matrix(yy), 
                force = TRUE))
        }
        else {
            bmat <- suppressMessages(zbind(xx, yy, force = TRUE))
        }
        if (missing(lbs) == FALSE && (is.list(lbs) == FALSE && 
            isTRUE(length(lbs) == dim(bmat)[3]) == TRUE)) {
            dimnames(bmat)[[3]] <- lbs
        }
        else {
            NA
        }
        if (missing(symCdm) == FALSE && isTRUE(symCdm == TRUE) == 
            TRUE) {
            for (i in which(qmd == "2M")) {
                bmat[, , i] <- bmat[, , i] + t(bmat[, , i])
            }
            rm(i)
        }
        if (is.null(dimnames(bmat)[[3]]) == TRUE) {
            dimnames(bmat)[[3]] <- seq_len(dim(bmat)[3])
            if (is.na(dim(x)[3]) == FALSE && is.null(dimnames(x)[[3]]) == 
                FALSE) {
                dimnames(bmat)[[3]][1:dim(x)[3]] <- dimnames(x)[[3]]
            }
            else {
                NA
            }
            if (is.na(dim(y)[3]) == FALSE && is.null(dimnames(y)[[3]]) == 
                FALSE) {
                dimnames(bmat)[[3]][(dim(xx)[3] + 1):dim(bmat)[3]] <- dimnames(y)[[3]]
            }
            else {
                NA
            }
        }
        else {
            NA
        }
        lst <- list(mlnet = bmat, lbs = Lbs, modes = qmd)
    }
    else {
        if (match.arg(type) == "cn2") {
            if (is.null(y) == FALSE && (is.matrix(y) == TRUE || 
                is.array(y) == TRUE || is.data.frame(y) == TRUE)) {
                cdmat <- array(0, dim = c(nrow(y), nrow(y)), 
                  dimnames = list(rownames(y), rownames(y)))
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
                ifelse(missing(diag) == FALSE && isTRUE(diag == 
                  TRUE) == TRUE, NA, diag(cdmat) <- 0L)
            }
            else {
                stop(paste(sQuote("y"), " is missing or has invalid format.", 
                  sep = ""))
            }
            cdmat <- suppressMessages(zbind(xx, cdmat, force = TRUE))
            if (missing(lbs) == FALSE && isTRUE(length(lbs) == 
                dim(cdmat)[3]) == TRUE) {
                dimnames(cdmat)[[3]] <- lbs
            }
            else {
                if (is.null(dimnames(cdmat)[[3]]) == TRUE) {
                  dimnames(cdmat)[[3]] <- seq_len(dim(cdmat)[3])
                  if (is.na(dim(x)[3]) == FALSE && is.null(dimnames(x)[[3]]) == 
                    FALSE) {
                    dimnames(cdmat)[[3]][1:dim(x)[3]] <- dimnames(x)[[3]]
                  }
                  else {
                    NA
                  }
                  if (is.na(dim(y)[3]) == FALSE && is.null(dimnames(y)[[3]]) == 
                    FALSE) {
                    dimnames(cdmat)[[3]][(dim(xx)[3] + 1):dim(cdmat)[3]] <- dimnames(y)[[3]]
                  }
                  else {
                    NA
                  }
                }
                else {
                  NA
                }
            }
        }
        else if (match.arg(type) == "cn") {
            if (is.null(dim(cdmat)) == TRUE) {
                qmd <- rep("2M", length(cdmat))
            }
            else if (is.na(dim(cdmat)[3]) == FALSE) {
                qmd <- rep("2M", dim(cdmat)[3])
            }
            else {
                qmd <- "2M"
            }
        }
        lst <- list(mlnet = cdmat, lbs = Lbs, modes = qmd)
    }
    class(lst) <- c("Multilevel", match.arg(type))
    return(lst)
}

