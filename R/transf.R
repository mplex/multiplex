## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
transf <-
function (x, type = c("toarray", "tolist", "toarray2", "toedgel"), 
    lbs = NULL, lb2lb, valued, sep, ord, sort, sym, add, adc, 
    rm.isol, na.rm) 
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
    else if (is.array(x) == TRUE && match.arg(type) != "toarray") {
        ifelse(missing(valued) == FALSE && isTRUE(valued == TRUE) == 
            TRUE, NA, x <- dichot(x, c = 1))
    }
    ifelse(missing(na.rm) == FALSE && isTRUE(na.rm == TRUE) == 
        TRUE, na.rm <- TRUE, na.rm <- FALSE)
    ifelse(missing(sort) == FALSE && isTRUE(sort == TRUE) == 
        TRUE, sort <- TRUE, sort <- FALSE)
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (match.arg(type) == "toarray") {
        if (is.array(x) == TRUE && missing(add) == TRUE && is.null(lbs) == 
            TRUE) {
            if (missing(rm.isol) == FALSE && isTRUE(rm.isol == 
                TRUE) == TRUE) {
                return(rm.isol(x))
            }
            else {
                message("No transformation.")
                return(x)
            }
        }
        if (is.matrix(x) == TRUE && isTRUE(length(unique(dim(x))) == 
            1) == TRUE) {
            if (is.null(lbs) == TRUE) {
                lbs <- dimnames(x)[[1]]
            }
            else {
                NA
            }
        }
        if (is.data.frame(x) == TRUE || (is.matrix(x) == TRUE && 
            identical(dimnames(x)[[1]], dimnames(x)[[2]]) == 
                FALSE)) {
            mat <- rbind(as.matrix(cbind(array(0L, dim = c(nrow(x), 
                nrow(x))), x)), matrix(0L, nrow = ncol(x), ncol = sum(dim(x))))
            dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- c(rownames(x), 
                colnames(x))
            if (missing(add) == FALSE) {
                warning(paste("Option ", sQuote("add"), " not supported for non-square arrays.", 
                  sep = ""), call. = FALSE)
            }
            else {
                NA
            }
            return(as.data.frame(mat))
        }
        if (missing(add) == FALSE) {
            if (is.list(x) == TRUE) {
                if (is.list(add) == TRUE) {
                  for (i in seq_len(length(add))) {
                    x[[i]] <- append(x[[i]], add[[i]])
                  }
                  rm(i)
                }
                else if (is.vector(add) == TRUE) {
                  for (i in seq_len(length(add))) {
                    x[[i]] <- append(x[[i]], add)
                  }
                  rm(i)
                }
            }
            else if (is.vector(x) == TRUE && is.array(x) == FALSE && 
                is.vector(add) == TRUE) {
                x <- c(x, add)
            }
            else if (is.array(x) == TRUE) {
                lbs <- c(dimnames(x)[[1]], add)
            }
        }
        if ((is.list(x) == TRUE && isTRUE(length(x) > 1L) == 
            TRUE) && is.matrix(x[[1]]) == TRUE) 
            return(x)
        if (missing(ord) == FALSE) {
            ord <- as.numeric(ord)
            if ((is.array(x) == FALSE & is.data.frame(x) == FALSE) && 
                isTRUE(nlevels(factor(unlist(dhc(x, sep = sep)))) > 
                  ord) == TRUE) {
                ord <- nlevels(factor(unlist(dhc(x, sep = sep))))
                warning(paste("Value in ", sQuote("ord"), " is ignored since the number of factor levels in pairwise list is higher.", 
                  sep = ""), call. = FALSE)
            }
            else {
                NA
            }
        }
        else if (missing(ord) == TRUE) {
            if (is.null(lbs) == FALSE) {
                ord <- length(lbs)
            }
            else {
                if (is.vector(x) == TRUE) {
                  if (isTRUE("data.frame" %in% unlist(lapply(x, 
                    is), use.names = FALSE)) == TRUE) {
                    ord <- length(unique(unlist(lapply(x, dimnames), 
                      use.names = FALSE)))
                  }
                  else {
                    ord <- length(unique(dhc(unlist(x, use.names = FALSE))))
                  }
                }
                else {
                  ord <- dim(x)[1]
                }
            }
        }
        if (is.array(x) == TRUE) {
            if (isTRUE(length(unique(dim(x))) > 1) == TRUE) {
                Lbs <- lbs
            }
            else {
                ifelse(is.null(lbs) == TRUE, Lbs <- dimnames(x)[[1]][seq_len(ord)], 
                  Lbs <- lbs[seq_len(ord)])
            }
            if (any(duplicated(Lbs)) == TRUE) {
                Lbs <- unique(Lbs)
                ord <- length(Lbs)
            }
            else {
                NA
            }
        }
        else if (is.array(x) == FALSE) {
            if (is.null(lbs) == FALSE) {
                if (any(unique(unlist(dhc(x))) %in% lbs) == FALSE && 
                  all(suppressWarnings(is.na(as.numeric(unique(unlist(dhc(x))))))) == 
                    FALSE) {
                  Lbs <- seq_len(ord)
                }
                else {
                  Lbs <- lbs[seq_len(ord)]
                }
            }
            else {
                if (isTRUE("data.frame" %in% unlist(lapply(x, 
                  is), use.names = FALSE)) == TRUE) {
                  ifelse(isTRUE(sort == TRUE) == TRUE, Lbs <- sort(unique(unlist(lapply(x, 
                    dimnames), use.names = FALSE))), Lbs <- unique(unlist(lapply(x, 
                    dimnames), use.names = FALSE)))
                }
                else {
                  ifelse(isTRUE(sort == TRUE) == TRUE, Lbs <- sort(unique(unlist(dhc(x, 
                    sep = sep))))[seq_len(ord)], Lbs <- levels(factor(unlist(dhc(x, 
                    sep = sep))))[seq_len(ord)])
                }
            }
        }
        if (is.list(x) == TRUE) {
            arr <- array(0L, dim = c(ord, ord, length(x)), dimnames = list(Lbs, 
                Lbs, names(x)))
            if (isTRUE("data.frame" %in% unlist(lapply(x, methods::is), 
                use.names = FALSE)) == TRUE) {
                for (k in seq_len(length(x))) {
                  lst <- vector(mode = "list", length = length(Lbs))
                  names(lst) <- Lbs
                  sco <- rownames(x[[k]])[which(x[[k]] == 1)%%nrow(x[[k]])]
                  for (i in unique(which(x[[k]] == 1)%%nrow(x[[k]]))) {
                    rco <- colnames(x[[k]])[which(x[[k]][i, ] == 
                      1)]
                    for (j in seq_len(length(rco))) {
                      ifelse(is.null(unlist(lst[which(Lbs %in% 
                        rco[j])])) == FALSE, lst[[which(Lbs %in% 
                        rco[j])]] <- append(as.vector(unlist(lst[which(Lbs %in% 
                        rco[j])])), rownames(x[[k]][i, ])), lst[which(Lbs %in% 
                        rco[j])] <- rownames(x[[k]][i, ]))
                    }
                    rm(j)
                  }
                  rm(i)
                  tmp <- matrix(0L, ncol = as.numeric(ord), nrow = as.numeric(ord), 
                    dimnames = list(Lbs, Lbs))
                  for (j in seq_len(length(lst))) {
                    if (is.null(lst[[j]]) == FALSE) {
                      for (i in seq_len(length(lst[[j]]))) {
                        tmp[which(rownames(tmp) == lst[[j]][i]), 
                          which(colnames(tmp) == names(lst)[j])] <- tmp[which(rownames(tmp) == 
                          lst[[j]][i]), which(colnames(tmp) == 
                          names(lst)[j])] + 1L
                      }
                      rm(i)
                    }
                  }
                  rm(j)
                  arr[, , k] <- tmp
                }
                rm(k)
            }
            else {
                for (i in seq_len(length(x))) {
                  for (k in seq_len(length(x[[i]]))) {
                    crs <- dhc(x[[i]][k], sep = sep)
                    arr[which(Lbs %in% crs[1]), which(Lbs %in% 
                      crs[2]), i] <- 1L
                  }
                  rm(k)
                }
                rm(i)
            }
            if (missing(sym) == FALSE && isTRUE(sym == TRUE) == 
                TRUE) {
                for (i in seq_len(dim(arr)[3])) {
                  arr[, , i] <- arr[, , i] + t(arr[, , i])
                }
                rm(i)
            }
            if (is.null(lbs) == FALSE && isTRUE(length(lbs) == 
                dim(arr)[1]) == TRUE) {
                dimnames(arr)[[1]] <- dimnames(arr)[[2]] <- lbs
            }
            else {
                NA
            }
            if (isTRUE(sort == TRUE) == TRUE) {
                return(perm(arr, sort = TRUE))
            }
            else {
                return(arr)
            }
        }
        else if (is.vector(x) == TRUE && is.array(x) == FALSE) {
            mat <- matrix(0L, ncol = ord, nrow = ord, dimnames = list(Lbs, 
                Lbs))
            for (k in seq_len(length(x))) {
                crs <- dhc(x[k], sep = sep)
                mat[which(Lbs %in% crs[1]), which(Lbs %in% crs[2])] <- 1L
            }
            rm(k)
            ifelse(missing(sym) == FALSE && isTRUE(sym == TRUE) == 
                TRUE, mat <- mat + t(mat), NA)
            if (isTRUE(sort == TRUE) == TRUE) {
                return(perm(mat, sort = TRUE))
            }
            else {
                return(mat)
            }
        }
        else if (is.array(x) == TRUE) {
            ifelse(isTRUE(NA %in% Lbs) == TRUE && is.null(lbs) == 
                FALSE, Lbs <- lbs, NA)
            if (is.na(dim(x)[3]) == TRUE && isTRUE(length(unique(dim(x))) == 
                1) == TRUE) {
                mat <- trnf(x, tolist = FALSE, ord = ord, lbs = Lbs)
            }
            else if (is.na(dim(x)[3]) == FALSE) {
                arr <- array(0L, dim = c(length(Lbs), length(Lbs), 
                  dim(x)[3]), dimnames = list(Lbs, Lbs, dimnames(x)[[3]]))
                for (k in seq_len(dim(x)[3])) {
                  arr[, , k] <- trnf(x[, , k], tolist = FALSE, 
                    lbs = Lbs, ord = ord)
                }
                rm(k)
            }
            else {
                mat <- matrix(0L, ncol = length(Lbs), nrow = length(Lbs), 
                  dimnames = list(Lbs, Lbs))
                for (i in seq_len(nrow(x))) {
                  mat[i, which(colnames(mat) %in% names(which(x[i, 
                    ] > 0)))] <- x[i, which(colnames(x) %in% 
                    names(which(x[i, ] > 0)))]
                }
                rm(i)
            }
        }
        else if (is.null(x) == TRUE) {
            mat <- matrix(0L, nrow = ord, ncol = ord)
        }
        else {
            stop(paste("Input for ", sQuote("toarray"), " must be vector, pairwise list, edge list, or array.", 
                sep = ""))
        }
        if (missing(sym) == FALSE && isTRUE(sym == TRUE) == TRUE) {
            if (is.na(dim(x)[3]) == TRUE) {
                mat <- mat + t(mat)
            }
            else {
                for (i in seq_len(dim(x)[3])) {
                  arr[, , i] <- arr[, , i] + t(arr[, , i])
                }
                rm(i)
            }
        }
        if (is.null(lbs) == FALSE && missing(add) == TRUE) {
            ifelse(is.na(dim(x)[3]) == TRUE, dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- lbs[seq_len(ord)], 
                dimnames(arr)[[2]] <- dimnames(arr)[[2]] <- lbs[seq_len(ord)])
        }
        else {
            NA
        }
        if (isTRUE(sort == TRUE) == TRUE) {
            if (is.na(dim(x)[3]) == TRUE) {
                mat <- perm(mat, sort = TRUE)
            }
            else {
                arr <- perm(arr, sort = TRUE)
            }
        }
        else {
            NA
        }
        if (missing(rm.isol) == FALSE && isTRUE(rm.isol == TRUE) == 
            TRUE) {
            if (is.na(dim(x)[3]) == TRUE) {
                slct <- which((apply(mat, 1, sum) + apply(mat, 
                  2, sum)) > 0)
                return(mat[slct, slct])
            }
            else {
                return(arr[slct, slct, ])
            }
        }
        else {
            if (is.na(dim(x)[3]) == TRUE) {
                return(mat)
            }
            else {
                return(arr)
            }
        }
    }
    if (match.arg(type) == "toedgel") {
        if (is.array(x) == TRUE) {
            return(edgel(x, toarray = FALSE))
        }
        else if (is.data.frame(x) == FALSE) {
            if (isTRUE(na.rm == FALSE) == TRUE) {
                if (any(is.na(x)) == TRUE) 
                  message(paste("Missing information in ", sQuote("x"), 
                    " recorded as ", sQuote("NA"), ".", sep = ""))
                x[is.na(x)] <- "NA"
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
        else {
            return(x)
        }
    }
    if (match.arg(type) == "tolist") {
        ifelse(missing(lb2lb) == FALSE && isTRUE(lb2lb == TRUE) == 
            TRUE, lb2lb <- TRUE, lb2lb <- FALSE)
        if (is.data.frame(x) == TRUE && all(is.numeric(unlist(x, 
            use.names = FALSE))) == FALSE) {
            return(apply(x, 1, function(w) {
                jnt(unlist(w, use.names = FALSE), uniq = TRUE)
            }))
        }
        if (is.list(x) == TRUE && is.data.frame(x) == FALSE) {
            inc <- list()
            for (k in seq_len(length(x))) {
                inc[[k]] <- trnf(x[[k]], tolist = TRUE, lb2lb = lb2lb, 
                  sep = sep)
            }
            rm(k)
            attr(inc, "names") <- names(x)
            return(inc)
        }
        else {
            if (isTRUE(is.character(x) == TRUE) == TRUE | (is.array(x) == 
                TRUE && is.null(dim(x)) == TRUE)) 
                return(x)
            if ((isTRUE(sum(x) > 0L) == FALSE | isTRUE(max(x) < 
                1L) == TRUE) && is.array(x) == TRUE) 
                return(NULL)
        }
        if (is.na(dim(x)[3]) == TRUE) {
            if (identical(dimnames(x)[[1]], dimnames(x)[[2]]) == 
                TRUE) {
                return(trnf(x, tolist = TRUE, lb2lb = lb2lb, 
                  sep = sep))
            }
            else {
                ifelse(is.matrix(x) == FALSE, x <- as.matrix(x), 
                  NA)
                x0 <- cbind(matrix(0, nrow = nrow(x) + ncol(x), 
                  ncol = nrow(x)), rbind(x, matrix(0, nrow = ncol(x), 
                  ncol = ncol(x))))
                dimnames(x0)[[1]] <- dimnames(x0)[[2]] <- c(rownames(x), 
                  colnames(x))
                return(trnf(x0, tolist = TRUE, lb2lb = lb2lb, 
                  sep = sep))
            }
        }
        else {
            inc <- vector(mode = "list", length = dim(x)[3])
            for (k in seq_len(dim(x)[3])) {
                if (identical(dimnames(x[, , k])[[1]], dimnames(x[, 
                  , k])[[2]]) == TRUE) {
                  inc[[k]] <- trnf(x[, , k], tolist = TRUE, lb2lb = lb2lb, 
                    sep = sep)
                }
                else {
                  ifelse(is.matrix(x[, , k]) == FALSE, x[, , 
                    k] <- as.matrix(x[, , k]), NA)
                  x0k <- cbind(matrix(0, nrow = nrow(x[, , k]) + 
                    ncol(x[, , k]), ncol = nrow(x[, , k])), rbind(x[, 
                    , k], matrix(0, nrow = ncol(x[, , k]), ncol = ncol(x[, 
                    , k]))))
                  dimnames(x0k)[[1]] <- dimnames(x0k)[[2]] <- c(rownames(x[, 
                    , k]), colnames(x[, , k]))
                  inc[[k]] <- trnf(x0k, tolist = TRUE, lb2lb = lb2lb, 
                    sep = sep)
                }
            }
            rm(k)
            attr(inc, "names") <- dimnames(x)[[3]]
            return(inc)
        }
    }
    else {
        ifelse(missing(lb2lb) == FALSE && isTRUE(lb2lb == FALSE) == 
            TRUE, lb2lb <- FALSE, lb2lb <- TRUE)
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
                      warning(paste(sQuote("lbs"), " should be a list for ", 
                        sQuote("toarray2"), ".", sep = ""))
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
                    warning(paste(sQuote("lbs"), " should be a list for ", 
                      sQuote("toarray2"), ".", sep = ""))
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

