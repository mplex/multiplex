cpath <-
function (x, values = FALSE, collapse = TRUE, directed, bonds, 
    sel, sep, lb2lb) 
{
    if (missing(directed) == FALSE && isTRUE(directed == FALSE) == 
        TRUE) {
        if (is.na(dim(x)[3]) == FALSE) {
            for (i in seq_len(dim(x)[3])) {
                x[, , i] <- mnplx(x[, , i], directed = FALSE)
            }
            rm(i)
        }
        else {
            x <- mnplx(x, directed = FALSE)
        }
    }
    else {
        NA
    }
    ifelse(missing(bonds) == TRUE, bonds <- "entire", NA)
    ifelse(missing(sep) == TRUE, sep <- ",", NA)
    ifelse(missing(sel) == FALSE && isTRUE(sel == "all") == TRUE, 
        sel <- dimnames(x)[[1]], NA)
    ifelse(missing(lb2lb) == FALSE && isTRUE(lb2lb == FALSE) == 
        TRUE, lb2lb <- FALSE, lb2lb <- TRUE)
    netrs <- transf(dichot(x, c = 1L), type = "tolist", lb2lb = lb2lb)
    qn <- list()
    if (is.na(dim(x)[3]) == FALSE) {
        cmp <- vector()
        for (k in seq_len((length(netrs) - 1L))) {
            if (length(netrs[[k]]) > 0) {
                for (i in seq_len(length(netrs[[k]]))) {
                  for (j in seq_len(length(netrs[[k + 1L]]))) {
                    if (isTRUE(any(duplicated(dhc(c(netrs[[k]][i], 
                      netrs[[k + 1]][j]))[seq(2, 3)]))) == TRUE) {
                      cmp <- append(cmp, paste0(attr(netrs[k], 
                        "names"), attr(netrs[k + 1L], "names")))
                      qn[[length(qn) + 1L]] <- as.vector(unique(dhc(c(netrs[[k]][i], 
                        netrs[[k + 1L]][j]))))
                    }
                    else if (isTRUE(any(duplicated(dhc(c(netrs[[k + 
                      1L]][j], netrs[[k]][i]))[seq(2, 3)]))) == 
                      TRUE) {
                      cmp <- append(cmp, paste0(attr(netrs[k + 
                        1L], "names"), attr(netrs[k], "names")))
                      qn[[length(qn) + 1L]] <- as.vector(unique(dhc(c(netrs[[k + 
                        1L]][j], netrs[[k]][i]))))
                    }
                  }
                  rm(j)
                }
                rm(i)
            }
        }
        rm(k)
        ifelse(isTRUE(length(cmp) != 0) == TRUE, attr(qn, "names") <- cmp, 
            NA)
    }
    else if (is.na(dim(x)[3]) == TRUE) {
        NA
    }
    qm <- list()
    cp <- vector()
    if (is.na(dim(x)[3]) == FALSE) {
        for (k in seq_len((length(netrs)))) {
            if (length(netrs[[k]]) > 1) {
                for (i in seq_len(length(netrs[[k]]) - 1L)) {
                  for (j in (i + 1L):length(netrs[[k]])) {
                    if (isTRUE(any(duplicated(dhc(c(netrs[[k]][i], 
                      netrs[[k]][j]))[seq(2, 3)]))) == TRUE) {
                      cp <- append(cp, paste0(attr(netrs[k], 
                        "names"), attr(netrs[k], "names")))
                      qm[[length(qm) + 1L]] <- as.vector(unique(dhc(c(netrs[[k]][i], 
                        netrs[[k]][j]))))
                    }
                  }
                  rm(j)
                }
                rm(i)
                for (i in length(netrs[[k]]):1) {
                  for (j in (i - 1L):1) {
                    if (isTRUE(any(duplicated(dhc(c(netrs[[k]][i], 
                      netrs[[k]][j]))[seq(2, 3)]))) == TRUE) {
                      cp <- append(cp, paste0(attr(netrs[k], 
                        "names"), attr(netrs[k], "names")))
                      qm[[length(qm) + 1L]] <- as.vector(unique(dhc(c(netrs[[k]][i], 
                        netrs[[k]][j]))))
                    }
                  }
                  rm(j)
                }
                rm(i)
            }
        }
        rm(k)
        ifelse(isTRUE(length(cp) != 0) == TRUE, attr(qm, "names") <- cp, 
            NA)
    }
    else if (is.na(dim(x)[3]) == TRUE) {
        for (i in seq_len(length(netrs) - 1L)) {
            for (j in (i + 1L):length(netrs)) {
                if (isTRUE(any(duplicated(dhc(c(netrs[i], netrs[j]))[seq(2, 
                  3)]))) == TRUE) {
                  qm[[length(qm) + 1L]] <- as.vector(unique(dhc(c(netrs[i], 
                    netrs[j]))))
                }
            }
            rm(j)
        }
        rm(i)
        for (i in length(netrs):1) {
            for (j in (i - 1L):1) {
                if (isTRUE(any(duplicated(dhc(c(netrs[i], netrs[j]))[seq(2, 
                  3)]))) == TRUE) {
                  qm[[length(qm) + 1]] <- as.vector(unique(dhc(c(netrs[i], 
                    netrs[j]))))
                }
            }
            rm(j)
        }
        rm(i)
    }
    if (missing(sel) == TRUE) {
        bnd2 <- c(qm, qn)
    }
    else {
        pths <- c(qm, qn)
        bnd2 <- list()
        bnd2s <- list()
        cpsel <- vector()
        for (i in seq_len(length(pths))) {
            if (any(sel %in% pths[[i]]) == TRUE) {
                ifelse(isTRUE(values == TRUE) == TRUE, bnd2s[[length(bnd2s) + 
                  1L]] <- pths[[i]][seq_len(3)], NA)
                bnd2[[length(bnd2) + 1L]] <- pths[[i]]
                cpsel <- append(cpsel, attr(pths, "names")[i])
            }
            else {
                NA
            }
        }
        rm(i)
        ifelse(isTRUE(length(cpsel) == 0) == TRUE, NA, attr(bnd2, 
            "names") <- cpsel)
        if (isTRUE(values == TRUE) == TRUE) {
            vsel <- list()
            length(vsel) <- length(sel)
            for (i in seq_len(length(vsel))) {
                vsel[[i]] <- which(unlist(bnd2s) %in% sel[i])%%3L
            }
            rm(i)
            attr(vsel, "names") <- sel
            rm(bnd2s)
        }
        else {
            NA
        }
    }
    if (isTRUE(length(bnd2) == 0) == TRUE) {
        return(list(0))
    }
    else {
        str2 <- attr(bnd2, "names")
        if (isTRUE(collapse == FALSE) == TRUE) {
            jbnd2 <- jnt(bnd2, sep = sep)
        }
        else if (isTRUE(collapse == TRUE) == TRUE) {
            if (is.null(str2) == FALSE) {
                str2u <- unique(attr(bnd2, "names"))
                jbnd2 <- list()
                length(jbnd2) <- length(str2u)
                for (i in seq_len(length(str2u))) {
                  jbnd2[[i]] <- as.vector(unlist(jnt(bnd2, sep = sep)[which(attr(bnd2, 
                    "names") == str2u[i])]))
                }
                rm(i)
                attr(jbnd2, "names") <- str2u
            }
            else {
                jbnd2 <- unlist(jnt(bnd2, sep = sep))
            }
        }
        if (isTRUE(values == TRUE) == TRUE) {
            if (is.na(dim(x)[3]) == FALSE) {
                str2 <- attr(bnd2, "names")
                lcp <- levels(factor(str2))
                count <- vector()
                for (i in seq_len(length(lcp))) count <- append(count, 
                  sum(str2 == lcp[i]))
                cnt <- t(as.data.frame(count))
                colnames(cnt) <- lcp
                rownames(cnt) <- ""
            }
            else {
                cnt <- length(jbnd2)
            }
            if (missing(sel) == TRUE) {
                return(list(cp = jbnd2, counts = cnt, total = sum(cnt)))
            }
            else {
                cnts <- matrix(0L, nrow = length(vsel), ncol = 3)
                colnames(cnts) <- c("orig", "intr", "trgt")
                rownames(cnts) <- attr(vsel, "names")
                for (i in seq_len(length(vsel))) {
                  cnts[i, 1] <- length(which(vsel[[i]] == 1))
                  cnts[i, 2] <- length(which(vsel[[i]] == 2))
                  cnts[i, 3] <- length(which(vsel[[i]] == 0))
                }
                rm(i)
                return(list(cp = jbnd2, sel = cnts, counts = cnt, 
                  total = sum(cnt)))
            }
        }
        else {
            jbnd2
        }
    }
}
