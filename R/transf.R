transf <-
function (x, type = c("toarray", "tolist", "toarray2", "toedgel"), 
    lbs = NULL, lb2lb, sep, ord, sort, sym, add, adc, na.rm) 
{
    if (is.list(x) == TRUE) {
        if (isTRUE(length(x) == 1L) == TRUE) {
            x <- x[[1]]
        }
        else if (is.null(unlist(x)) == TRUE) {
            return(NULL)
        }
        else {
            NA
        }
    }
    if (match.arg(type) == "toarray" && is.data.frame(x) == TRUE) {
        if (missing(na.rm) == FALSE && isTRUE(na.rm == FALSE) == 
            TRUE) {
            if (any(is.na(x)) == TRUE) 
                message("Missing information in \"x\" recorded as \"NA\".")
            x[, 1] <- as.factor(x[, 1])
            levels(x[, 1]) <- c(levels(x[, 1]), "NA")
            x[, 1][is.na(x[, 1])] <- "NA"
            x[, 2] <- as.factor(x[, 2])
            levels(x[, 2]) <- c(levels(x[, 2]), "NA")
            x[, 2][is.na(x[, 2])] <- "NA"
        }
        if (missing(add) == FALSE) {
            xadd <- suppressWarnings(edgel(x, add = add))
            diag(xadd)[which(dimnames(xadd)[[1]] %in% add)] <- 0
            return(xadd)
        }
        else {
            mat <- suppressWarnings(edgel(x, toarray = TRUE))
            if (is.null(lbs) == FALSE) {
                dimnames(mat)[[1]] <- lbs[seq_len(dim(mat)[1])]
                dimnames(mat)[[2]] <- lbs[seq_len(dim(mat)[2])]
            }
            else {
                NA
            }
            return(mat)
        }
    }
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (match.arg(type) == "toedgel") {
        if (is.array(x) == TRUE) {
            if (is.na(dim(x)[3]) == TRUE) {
                tmp <- trnf(x, tolist = TRUE, lb2lb = TRUE, lbs = dimnames(x)[[1]], 
                  sep = sep)
                tmp2 <- sapply(tmp, function(z) {
                  strsplit(z, sep)
                })
                rm(tmp)
                edgl <- data.frame(matrix(nrow = 0, ncol = 3))
                colnames(edgl) <- c("s", "r", "t")
                for (i in seq_len(length(tmp2))) {
                  edgl[nrow(edgl) + 1L, ] <- c(strsplit(tmp2[[i]], 
                    sep)[[1]], strsplit(tmp2[[i]], sep)[[2]], 
                    "1")
                }
                rm(i)
            }
            else {
                tmpl <- trnf(x, tolist = TRUE, lb2lb = TRUE, 
                  lbs = dimnames(x)[[1]], sep = sep)
                tmp <- trnf(x, tolist = TRUE, lb2lb = FALSE, 
                  sep = sep)
                tmp2 <- sapply(tmp, function(z) {
                  strsplit(z, sep)
                })
                rm(tmp)
                edgl <- data.frame(matrix(nrow = 0, ncol = (dim(x)[3]) + 
                  2))
                colnames(edgl) <- c("s", "r", dimnames(x)[[3]])
                for (l in seq_len(length(tmp2[[1]]))) {
                  edgl[nrow(edgl) + 1L, 3] <- 1L
                  edgl[nrow(edgl), 1:2] <- strsplit(tmpl[[1]][[l]], 
                    sep)[[1]]
                }
                rm(l)
                edgl1 <- Map(as.vector, data.frame(t(edgl[, 1:2])))
                for (k in 2:dim(x)[3]) {
                  for (l in seq_len(length(tmp2[[k]]))) {
                    tmpeval <- strsplit(tmpl[[k]][[l]], sep)[[1]]
                    if (any(unlist(lapply(lapply(edgl1, function(z) {
                      tmpeval %in% z
                    }), all), use.names = FALSE)) == TRUE) {
                      edgl[which(unlist(lapply(lapply(edgl1, 
                        function(z) {
                          tmpeval %in% z
                        }), all), use.names = FALSE)), (k + 2L)] <- 1L
                    }
                    else {
                      edgl[nrow(edgl) + 1L, (k + 2L)] <- 1L
                      edgl[nrow(edgl), 1:2] <- tmpeval
                    }
                  }
                  rm(l)
                  rm(tmpeval)
                }
                rm(k)
                rm(edgl1)
                edgl[sapply(edgl, is.na)] <- 0
            }
            return(edgl)
        }
        else {
            if (missing(na.rm) == FALSE && isTRUE(na.rm == FALSE) == 
                TRUE) {
                if (any(is.na(x)) == TRUE) 
                  message("Missing information in \"x\" recorded as \"NA\".")
                ifelse(is.data.frame(x) == FALSE, NA, x[is.na(x)] <- "NA")
            }
            else {
                NA
            }
            if (is.list(x) == TRUE) {
                edgl <- data.frame(matrix(nrow = 0, ncol = (2 + 
                  length(x))))
                colnames(edgl) <- c("s", "r", names(x))
                tmps <- lapply(x, function(z) {
                  strsplit(z, sep)
                })
                for (i in seq_len(length(x[[1]]))) {
                  edgl[nrow(edgl) + 1L, ] <- c(tmps[[1]][i][[1]], 
                    1L, rep(0L, length(x) - 1L))
                }
                rm(i)
                edgl <- unique(edgl)
                ttmps <- split(edgl[, 1:2], seq(nrow(edgl)))
                tmpss <- lapply(ttmps, function(z) {
                  unlist(z, use.names = FALSE)
                })
                for (k in 2:length(x)) {
                  if (any(tmps[[k]] %in% tmpss) == FALSE) {
                    for (i in seq_len(length(x[[k]]))) {
                      edgl[nrow(edgl) + 1L, ] <- c(tmps[[k]][i][[1]], 
                        rep(0L, length(x)))
                      edgl[nrow(edgl), (2L + k)] <- 1L
                    }
                    rm(i)
                  }
                  else if (any(tmps[[k]] %in% tmpss) == TRUE) {
                    if (all(tmps[[k]] %in% tmpss) == TRUE && 
                      all(tmpss %in% tmps[[k]]) == TRUE) {
                      edgl[, (2L + k)] <- 1L
                    }
                    else {
                      kual <- which(tmpss %in% tmps[[k]])
                      edgl[kual, (2L + k)] <- as.numeric(edgl[kual, 
                        (2L + k)]) + 1L
                      rm(kual)
                      kuals <- which(!(tmps[[k]] %in% tmpss))
                      for (i in kuals) {
                        edgl[nrow(edgl) + 1L, ] <- c(tmps[[k]][i][[1]], 
                          rep(0L, length(x)))
                        edgl[nrow(edgl), (2L + k)] <- 1L
                      }
                      rm(i)
                      rm(kuals)
                    }
                  }
                  ttmps <- split(edgl[, 1:2], seq(nrow(edgl)))
                  tmpss <- lapply(ttmps, function(z) {
                    unlist(z, use.names = FALSE)
                  })
                }
                rm(k)
            }
            else {
                if (any(duplicated(x)) == TRUE) {
                  x0 <- x[which(!(duplicated(x)))]
                  x1 <- x[which(duplicated(x))]
                  flgdp <- TRUE
                }
                else {
                  x0 <- x
                  flgdp <- FALSE
                }
                edgl <- data.frame(matrix(nrow = 0, ncol = 3))
                colnames(edgl) <- c("s", "r", "t")
                for (k in seq_len(length(x0))) {
                  edgl[nrow(edgl) + 1L, ] <- c(strsplit(x0[k], 
                    split = ", ")[[1]], "1")
                }
                rm(k)
                if (isTRUE(flgdp == TRUE) == TRUE) {
                  for (k in seq_len(length(x1))) {
                    tmp <- strsplit(x1[k], split = ", ")[[1]]
                    for (i in seq_len(nrow(edgl))) {
                      if (identical(tmp, as.character(edgl[i, 
                        1:2])) == TRUE) {
                        edgl[i, 3] <- as.numeric(edgl[i, 3]) + 
                          1L
                        break
                      }
                      else {
                        NA
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                else {
                  NA
                }
            }
            rownames(edgl) <- seq_len(nrow(edgl))
            return(edgl)
        }
    }
    if (match.arg(type) == "tolist") {
        if (isTRUE(is.character(x) == TRUE) == TRUE | (is.array(x) == 
            TRUE && is.null(dim(x)) == TRUE)) 
            return(x)
        if ((isTRUE(sum(x) > 0L) == FALSE | isTRUE(max(x) < 1L) == 
            TRUE) && is.array(x) == TRUE) 
            return(NULL)
        ifelse(missing(lb2lb) == FALSE && isTRUE(lb2lb == TRUE) == 
            TRUE, lb2lb <- TRUE, lb2lb <- FALSE)
        if (is.list(x) == TRUE && is.data.frame(x) == FALSE) {
            inc <- list()
            for (k in seq_len(length(x))) {
                inc[[k]] <- trnf(x[[k]], tolist = TRUE, lb2lb = lb2lb)
            }
            rm(k)
            attr(inc, "names") <- names(x)
            return(inc)
        }
        if (is.null(lbs) == FALSE | isTRUE(lb2lb == TRUE) == 
            FALSE) {
            lbsr <- lbsc <- lbs
        }
        else {
            ifelse(is.null(dimnames(x)[[1]]) == TRUE, lbsr <- seq_len(dim(x)[1]), 
                lbsr <- dimnames(x)[[1]])
            ifelse(is.null(dimnames(x)[[2]]) == TRUE, lbsc <- seq_len(dim(x)[2]), 
                lbsc <- dimnames(x)[[2]])
        }
        rws <- vector()
        cls <- vector()
        if (is.na(dim(x)[3]) == TRUE) {
            inc <- list()
            for (l in seq_len(max(x))) {
                xd <- dichot(x, c = l)
                for (i in seq_len(length(which((xd) == 1L)))) {
                  cls[i] <- (ceiling(which((xd) == 1L)/dim(x)[1]))[i]
                  ifelse((which((xd) == 1L)%%dim(x)[1])[i] == 
                    0L, rws[i] <- (which((xd) == 1L)%%dim(x)[1])[i] + 
                    dim(x)[1], rws[i] <- (which((xd) == 1L)%%dim(x)[1])[i])
                  ifelse(isTRUE(lb2lb == TRUE) == TRUE, inc[[length(inc) + 
                    1L]] <- paste(lbsr[rws[i]], lbsc[cls[i]], 
                    sep = sep), inc[[length(inc) + 1L]] <- paste(rws[i], 
                    cls[i], sep = sep))
                }
                rm(i)
            }
            rm(l)
            return(unlist(inc))
        }
        else {
            Inc <- vector("list", length = dim(x)[3])
            names(Inc) <- dimnames(x)[[3]]
            for (k in seq_len(dim(x)[3])) {
                inc <- list()
                if (isTRUE(max(x[, , k]) >= 1L) == TRUE) {
                  for (l in seq_len(max(x[, , k]))) {
                    xd <- dichot(x[, , k], c = l)
                    for (i in seq_len(length(which((xd) == 1L)))) {
                      cls[i] <- (ceiling(which((xd) == 1L)/dim(x[, 
                        , k])[1]))[i]
                      ifelse((which((xd) == 1L)%%dim(x[, , k])[1])[i] == 
                        0L, rws[i] <- (which((xd) == 1L)%%dim(x[, 
                        , k])[1])[i] + dim(x[, , k])[1], rws[i] <- (which((xd) == 
                        1L)%%dim(x[, , k])[1])[i])
                      ifelse(isTRUE(lb2lb == TRUE) == TRUE, inc[[length(inc) + 
                        1L]] <- paste(lbsr[rws[i]], lbsc[cls[i]], 
                        sep = sep), inc[[length(inc) + 1L]] <- paste(rws[i], 
                        cls[i], sep = sep))
                    }
                    rm(i)
                  }
                  rm(l)
                  Inc[[k]] <- unlist(inc)
                }
                else {
                  NA
                }
            }
            rm(k)
            return(Inc)
        }
    }
    else {
        ifelse(missing(lb2lb) == FALSE && isTRUE(lb2lb == FALSE) == 
            TRUE, lb2lb <- FALSE, lb2lb <- TRUE)
    }
    if (match.arg(type) == "toarray") {
        if (missing(add) == FALSE) {
            if (is.list(x) == TRUE) {
                if (is.list(add) == TRUE) {
                  for (i in seq_len(length(add))) {
                    x[[i]] <- append(x[[i]], add[[i]])
                  }
                  rm(i)
                }
            }
            else if (is.vector(x) == TRUE) {
                ifelse(is.vector(add) == TRUE, x <- c(x, add), 
                  NA)
            }
            else if (is.array(x) == TRUE) {
                lbs <- c(dimnames(x)[[1]], add)
                ord <- dim(x)[1] + length(add)
            }
        }
        if ((is.vector(x) == FALSE && isTRUE(dim(x)[1] == dim(x)[2]) == 
            FALSE)) 
            return(x)
        if ((is.list(x) == TRUE && isTRUE(length(x) > 1L) == 
            TRUE) && is.matrix(x[[1]]) == TRUE) 
            return(x)
        if (missing(ord) == TRUE) {
            if (is.vector(x) == TRUE) {
                ord <- length(unique(dhc(unlist(x, use.names = FALSE))))
            }
            else {
                ifelse(is.null(lbs) == FALSE, ord <- length(dhc(lbs, 
                  sep = sep)), ord <- dim(x)[1])
            }
        }
        else {
            ord <- as.numeric(ord)
            if ((is.array(x) == FALSE & is.data.frame(x) == FALSE) && 
                isTRUE(nlevels(factor(unlist(dhc(x, sep = sep)))) > 
                  ord) == TRUE) {
                ord <- nlevels(factor(unlist(dhc(x, sep = sep))))
                warning("\"ord\" is ignored since value is less than the number of factor levels in pairwise list.")
            }
            else {
                NA
            }
        }
        if (is.array(x) == TRUE) {
            ifelse(is.null(lbs) == TRUE, Lbs <- dimnames(x)[[1]][seq_len(ord)], 
                Lbs <- lbs[seq_len(ord)])
            if (any(duplicated(Lbs)) == TRUE) {
                Lbs <- unique(Lbs)
                ord <- length(Lbs)
            }
            else {
                NA
            }
        }
        else if (is.array(x) == FALSE) {
            if (is.null(lbs) == FALSE | (is.null(lbs) == FALSE && 
                isTRUE(lb2lb == TRUE) == TRUE)) {
                Lbs <- lbs[seq_len(ord)]
            }
            else {
                ifelse(missing(sort) == FALSE && isTRUE(sort == 
                  TRUE) == TRUE, Lbs <- sort(unique(unlist(dhc(x, 
                  sep = sep))))[seq_len(ord)], Lbs <- unique(unlist(dhc(x, 
                  sep = sep)))[seq_len(ord)])
            }
        }
        if (is.list(x) == TRUE) {
            mat <- array(0L, dim = c(ord, ord, length(x)), dimnames = list(Lbs, 
                Lbs, names(x)))
            for (i in seq_len(length(x))) {
                if (is.null(lbs) == FALSE) {
                  mat[, , i] <- trnf(x[[i]], tolist = FALSE, 
                    ord = ord)
                }
                else {
                  mat[, , i] <- trnf(x[[i]], tolist = FALSE, 
                    ord = ord, lbs = Lbs)
                }
            }
            rm(i)
        }
        else if (is.vector(x) == TRUE) {
            mat <- matrix(0L, ncol = ord, nrow = ord, dimnames = list(Lbs, 
                Lbs))
            for (i in seq_len(length(x))) {
                mat[which(Lbs == dhc(x[i], sep = sep)[1]), which(Lbs == 
                  dhc(x[i], sep = sep)[2])] <- mat[which(Lbs == 
                  dhc(x[i], sep = sep)[1]), which(Lbs == dhc(x[i], 
                  sep = sep)[2])] + 1L
            }
            rm(i)
        }
        else if (is.array(x) == TRUE) {
            ifelse(isTRUE(NA %in% Lbs) == TRUE && is.null(lbs) == 
                FALSE, Lbs <- lbs, NA)
            if (is.na(dim(x)[3]) == TRUE) {
                mat <- trnf(trnf(x, tolist = TRUE, lb2lb = TRUE), 
                  tolist = FALSE, ord = ord, lbs = Lbs)
            }
            else {
                arr <- array(dim = c(length(Lbs), length(Lbs), 
                  dim(x)[3]), dimnames = list(Lbs, Lbs, dimnames(x)[[3]]))
                for (k in seq_len(dim(x)[3])) {
                  arr[, , k] <- trnf(trnf(x[, , k], tolist = TRUE, 
                    lb2lb = TRUE), tolist = FALSE, ord = ord, 
                    lbs = Lbs)
                }
                rm(k)
                mat <- arr
            }
        }
        else if (is.null(x) == TRUE) {
            mat <- matrix(0L, nrow = ord, ncol = ord)
        }
        else {
            stop("Input for \"toarray\" must be vector, pairwise list, edge list, or array.")
        }
        if (isTRUE(lb2lb == TRUE) == FALSE && is.null(lbs) == 
            TRUE) {
            dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- NULL
        }
        else {
            NA
        }
        if (missing(sym) == FALSE && isTRUE(sym == TRUE) == TRUE) {
            if (is.na(dim(mat)[3]) == TRUE) {
                mat <- mat + t(mat)
            }
            else {
                for (i in seq_len(dim(mat)[3])) {
                  mat[, , i] <- mat[, , i] + t(mat[, , i])
                }
                rm(i)
            }
        }
        if (is.null(lbs) == FALSE && missing(add) == TRUE) {
            dimnames(mat)[[1]] <- lbs[seq_len(ord)]
            dimnames(mat)[[2]] <- lbs[seq_len(ord)]
        }
        else {
            NA
        }
        return(mat)
    }
    if (match.arg(type) == "toarray2") {
        if (is.vector(x) == TRUE) {
            vec <- dhc(x, sep = sep)
            if (missing(add) == FALSE && isTRUE(is.vector(add) == 
                TRUE) == TRUE) {
                if (is.list(vec) == TRUE) {
                  for (k in seq_len(length(vec))) {
                    for (i in seq_len(length(add[[k]]))) {
                      vec[[k]] <- append(vec[[k]], c(add[[k]][i], 
                        NA))
                    }
                    rm(i)
                  }
                  rm(k)
                }
                else if (is.vector(vec) == TRUE) {
                  vec <- append(vec, dhc(paste(add, NA, sep = sep)))
                  vec[which(vec == "NA")] <- NA
                }
                else {
                  NA
                }
            }
            if (missing(adc) == FALSE && isTRUE(is.vector(adc) == 
                TRUE) == TRUE) {
                if (is.list(vec) == TRUE) {
                  for (k in seq_len(length(vec))) {
                    for (i in seq_len(length(adc[[k]]))) {
                      vec[[k]] <- append(vec[[k]], c(NA, adc[[k]][i]))
                    }
                    rm(i)
                  }
                  rm(k)
                }
                else if (is.vector(vec) == TRUE) {
                  vec <- append(vec, dhc(paste(NA, adc, sep = sep)))
                  vec[which(vec == "NA")] <- NA
                }
                else {
                  NA
                }
            }
            if (is.list(vec) == TRUE) {
                dfr <- list()
                vec1 <- list()
                length(vec1) <- length(vec)
                vec2 <- list()
                length(vec2) <- length(vec)
                for (k in seq_len(length(vec))) {
                  vec1[[k]] <- vec[[k]][which(seq_len(length(vec[[k]]))%%2L == 
                    1L)]
                  vec2[[k]] <- vec[[k]][which(seq_len(length(vec[[k]]))%%2L == 
                    0L)]
                  if (is.null(lbs) == FALSE) {
                    if (is.list(lbs) == FALSE) 
                      warning("\"lbs\" should be a list for \"toarray2\".")
                    vc1 <- unique(c(vec1[[k]], lbs[[k]][[1]]))
                    vc2 <- unique(c(vec2[[k]], lbs[[k]][[2]]))
                  }
                  else {
                    vc1 <- as.vector(stats::na.omit(unique(vec1[[k]])))
                    vc2 <- as.vector(stats::na.omit(unique(vec2[[k]])))
                  }
                  if (missing(sort) == FALSE && isTRUE(sort == 
                    TRUE) == TRUE) {
                    vc1 <- sort(vc1)
                    vc2 <- sort(vc2)
                  }
                  else {
                    NA
                  }
                  temp <- data.frame(matrix(0L, ncol = length(vc2), 
                    nrow = length(vc1), dimnames = list(vc1, 
                      vc2)))
                  for (i in seq_len(length(vec1[[k]]))) {
                    temp[which(vc1 == vec1[[k]][i]), which(vc2 == 
                      vec2[[k]][i])] <- 1L
                  }
                  rm(i)
                  dfr[[k]] <- temp
                }
                rm(k)
                attr(dfr, "names") <- attr(x, "names")
            }
            else {
                vec1 <- vec[which(seq_len(length(vec))%%2L == 
                  1L)]
                vec2 <- vec[which(seq_len(length(vec))%%2L == 
                  0L)]
                if (is.null(lbs) == FALSE) {
                  if (is.list(lbs) == FALSE) 
                    warning("\"lbs\" should be a list for \"toarray2\".")
                  vc1 <- unique(c(vec1, lbs[[1]]))
                  vc2 <- unique(c(vec2, lbs[[2]]))
                }
                else {
                  vc1 <- stats::na.omit(unique(vec1))
                  vc2 <- stats::na.omit(unique(vec2))
                }
                if (missing(sort) == FALSE && isTRUE(sort == 
                  TRUE) == TRUE) {
                  vc1 <- sort(vc1)
                  vc2 <- sort(vc2)
                }
                else {
                  NA
                }
                dfr <- data.frame(matrix(0L, ncol = length(vc2), 
                  nrow = length(vc1), dimnames = list(vc1, vc2)))
                for (i in seq_len(length(vec1))) {
                  dfr[which(vc1 == vec1[i]), which(vc2 == vec2[i])] <- 1L
                }
                rm(i)
            }
        }
        else {
            ifelse(is.null(lbs) == TRUE, dfr <- as.data.frame(x), 
                dfr <- trnf(trnf(x, tolist = TRUE, lb2lb = TRUE), 
                  tolist = FALSE, ord = length(lbs), lbs = lbs))
        }
        return(dfr)
    }
}
