transf <-
function (x, type = c("toarray", "tolist", "toarray2"), lbs = NULL, 
    lb2lb, sep, ord, sort, add, adc) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (match.arg(type) == "tolist") {
        if (isTRUE(is.character(x) == TRUE) == TRUE | is.null(dim(x)[3]) == 
            TRUE) 
            return(x)
        if (isTRUE(sum(x) > 0L) == FALSE | isTRUE(max(x) < 1L) == 
            TRUE) 
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
            lbsr <- dimnames(x)[[1]]
            lbsc <- dimnames(x)[[2]]
        }
        rws <- vector()
        cls <- vector()
        if (is.na(dim(x)[3]) == TRUE) {
            inc <- list()
            for (l in seq_len(max(x))) {
                X <- dichot(x, c = l)
                for (i in seq_len(length(which((X) == 1L)))) {
                  cls[i] <- (ceiling(which((X) == 1L)/dim(x)[1]))[i]
                  ifelse((which((X) == 1L)%%dim(x)[1])[i] == 
                    0L, rws[i] <- (which((X) == 1L)%%dim(x)[1])[i] + 
                    dim(x)[1], rws[i] <- (which((X) == 1L)%%dim(x)[1])[i])
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
            Inc <- list()
            length(Inc) <- dim(x)[3]
            names(Inc) <- dimnames(x)[[3]]
            for (k in seq_len(dim(x)[3])) {
                inc <- list()
                if (isTRUE(max(x[, , k]) >= 1L) == TRUE) {
                  for (l in seq_len(max(x[, , k]))) {
                    X <- dichot(x[, , k], c = l)
                    for (i in seq_len(length(which((X) == 1L)))) {
                      cls[i] <- (ceiling(which((X) == 1L)/dim(x[, 
                        , k])[1]))[i]
                      ifelse((which((X) == 1L)%%dim(x[, , k])[1])[i] == 
                        0L, rws[i] <- (which((X) == 1L)%%dim(x[, 
                        , k])[1])[i] + dim(x[, , k])[1], rws[i] <- (which((X) == 
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
        }
        if ((is.vector(x) == FALSE && isTRUE(dim(x)[1] == dim(x)[2]) == 
            FALSE)) 
            return(x)
        if (missing(ord) == TRUE) {
            if (is.vector(x) == TRUE) {
                ifelse(is.null(lbs) == FALSE, ord <- length(lbs), 
                  ord <- length(dhc(jnt(unlist(x), sep = sep), 
                    sep = sep)))
            }
            else {
                ifelse(is.null(lbs) == FALSE, ord <- length(lbs), 
                  ord <- dim(x)[1])
            }
        }
        else {
            ord <- as.numeric(ord)
            if ((is.array(x) == FALSE & is.data.frame(x) == FALSE) && 
                isTRUE(nlevels(factor(unlist(dhc(x, sep = sep)))) > 
                  ord) == TRUE) {
                ord <- nlevels(factor(unlist(dhc(x, sep = sep))))
                warning("'ord' is ignored, value is less than the number of factor levels in the pairwise list.")
            }
            else {
                NA
            }
        }
        if (is.array(x) == TRUE) {
            ifelse(is.null(lbs) == TRUE, Lbs <- dimnames(x)[[1]][seq_len(ord)], 
                Lbs <- lbs[seq_len(ord)])
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
                mat[, , i] <- trnf(x[[i]], tolist = FALSE, ord = ord, 
                  lbs = Lbs)
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
            mat <- trnf(trnf(x, tolist = TRUE, lb2lb = TRUE), 
                tolist = FALSE, ord = ord, lbs = Lbs)
        }
        else if (is.null(x) == TRUE) {
            mat <- matrix(0L, nrow = ord, ncol = ord)
        }
        else {
            stop("Input for 'toarray' must be a vector, a list, ar an array.")
        }
        if (isTRUE(lb2lb == TRUE) == FALSE && is.null(lbs) == 
            TRUE) {
            dimnames(mat)[[1]] <- dimnames(mat)[[2]] <- NULL
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
                      warning("\"lbs\" should be a list with this option.")
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
                    warning("\"lbs\" should be a list with this option.")
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
