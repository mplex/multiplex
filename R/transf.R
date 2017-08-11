transf <-
function (x, type = c("tolist", "toarray"), lbs = NULL, lb2lb, 
    sep, ord) 
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
            return(sort(unlist(inc)))
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
    if (match.arg(type) == "toarray") {
        ifelse(missing(lb2lb) == FALSE && isTRUE(lb2lb == FALSE) == 
            TRUE, lb2lb <- FALSE, lb2lb <- TRUE)
        if (missing(ord) == TRUE) {
            if (is.array(x) == TRUE | (is.character(x) == FALSE && 
                is.list(x) == FALSE) | is.data.frame(x) == TRUE) 
                return(x)
            ifelse(is.null(lbs) == FALSE, ord <- length(lbs), 
                ord <- length(dhc(jnt(unlist(x), sep = sep), 
                  sep = sep)))
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
            Lbs <- dimnames(x)[[1]][seq_len(ord)]
        }
        else if (is.array(x) == FALSE) {
            ifelse(is.null(lbs) == FALSE | (is.null(lbs) == FALSE && 
                isTRUE(lb2lb == TRUE) == TRUE), Lbs <- lbs[seq_len(ord)], 
                Lbs <- levels(factor(unlist(dhc(x, sep = sep))))[seq_len(ord)])
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
            lx <- trnf(x, tolist = TRUE, lb2lb = TRUE)
            mat <- trnf(trnf(x, tolist = TRUE, lb2lb = TRUE), 
                tolist = FALSE, ord = ord, lbs = Lbs)
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
}
