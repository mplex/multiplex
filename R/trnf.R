## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
trnf <-
function (x, lbs = NULL, lb2lb = FALSE, tolist = FALSE, ord, 
    sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    ifelse(isTRUE(tolist == TRUE) == TRUE && is.na(dim(x)[3]) == 
        TRUE, x <- as.matrix(x), NA)
    if (is.vector(x) == FALSE && is.na(dim(x)[3]) == TRUE) {
        co <- rev(sort(unique(as.vector(as.matrix(x))))[-1])
        lst <- vector(mode = "list", length = dim(x)[1])
        names(lst) <- dimnames(x)[[1]]
        if (isTRUE(suppressWarnings(max(co)) == 1L) == TRUE || 
            isTRUE(length(co) == 0) == TRUE) {
            for (k in seq_len(length(lst))) {
                lst[[k]] <- names(which(x[, k] == 1))
            }
            rm(k)
        }
        else {
            for (i in seq_along(co)) {
                rpvt <- which(x == co[i])%%dim(x)[1]
                rpvt[rpvt == 0] <- dim(x)[2]
                sco <- rownames(x)[rpvt]
                if (isTRUE(length(sco) > 1L) == TRUE) {
                  tmp <- unique(x[rpvt, ])
                  sco <- vector()
                  rco <- vector()
                  for (l in seq_len(nrow(tmp))) {
                    sco <- append(sco, rep(rownames(tmp)[l], 
                      length(which(tmp[l, ] == co[i]))))
                    rco <- append(rco, names(which(tmp[l, ] == 
                      co[i])))
                  }
                  rm(l)
                  for (j in seq_len(length(rco))) {
                    ifelse(is.null(lst[[which(names(lst) == rco[j])]]) == 
                      FALSE, lst[[which(names(lst) == rco[j])]] <- append(lst[[which(names(lst) == 
                      rco[j])]], rep(sco[j], as.numeric(co[i]))), 
                      lst[[which(names(lst) == rco[j])]] <- rep(sco[j], 
                        as.numeric(co[i])))
                  }
                  rm(j)
                }
                else if (isTRUE(length(sco) == 1L) == TRUE) {
                  ifelse(is.null(lst[[which(x[rpvt, ] == co[i])]]) == 
                    FALSE, lst[[which(x[rpvt, ] == co[i])]] <- append(lst[[which(x[rpvt, 
                    ] == co[i])]], rep(names(lst)[rpvt], as.numeric(co[i]))), 
                    lst[[which(x[rpvt, ] == co[i])]] <- rep(names(lst)[rpvt], 
                      as.numeric(co[i])))
                }
            }
            rm(i)
        }
    }
    else {
        if (is.vector(x) == FALSE || isTRUE(class(x) %in% c("matrix", 
            "array")) == FALSE) 
            stop(paste(sQuote(deparse(substitute(x))), " should be a matrix or low-dimensional array.", 
                sep = ""), call. = FALSE)
    }
    if (isTRUE(tolist == TRUE) == TRUE) {
        if (isTRUE(sum(x) > 0L) == FALSE) 
            return(paste(0, 0, sep = sep))
        if (isTRUE(lb2lb == FALSE) == TRUE) {
            sct <- which(!(unlist(lapply(lst, is.null), use.names = FALSE)))
            lst <- lst[sct]
            names(lst) <- sct
            lst <- lapply(lst, function(z) {
                as.vector(factor(z, dimnames(x)[[1]], 1:dim(x)[1]))
            })
        }
        else {
            NA
        }
        llst <- lst
        for (k in seq_len(length(lst))) {
            ifelse(isTRUE(length(lst[[k]]) == 0) == TRUE, NA, 
                llst[[k]] <- paste(lst[[k]], names(lst)[k], sep = sep))
        }
        rm(k)
        return(unlist(llst, use.names = FALSE))
    }
    else {
        if (missing(ord) == TRUE && is.array(x) == FALSE) {
            ord <- length(dhc(jnt(x, sep = sep), sep = sep))
        }
        else {
            ifelse(missing(ord) == FALSE, ord <- as.numeric(ord), 
                ord <- dim(x)[1])
        }
        if (is.vector(x) == FALSE) {
            ifelse(is.array(x) == TRUE && missing(ord) == TRUE, 
                return(x), NA)
            ifelse(is.null(lbs) == TRUE, lbs <- dimnames(x)[[1]], 
                NA)
            ifelse(is.null(lbs) == FALSE, NA, lbs <- lbs[seq_len(ord)])
            mat <- matrix(0L, ncol = ord, nrow = ord, dimnames = list(lbs, 
                lbs))
            for (j in seq_len(length(lst))) {
                if (is.null(lst[[j]]) == FALSE) {
                  for (i in seq_len(length(lst[[j]]))) {
                    mat[which(rownames(mat) == lst[[j]][i]), 
                      which(colnames(mat) == names(lst)[j])] <- mat[which(rownames(mat) == 
                      lst[[j]][i]), which(colnames(mat) == names(lst)[j])] + 
                      1L
                  }
                  rm(i)
                }
            }
            rm(j)
            return(mat)
        }
        else {
            lbs <- unique(dhc(unlist(x), sep = sep))
            if (is.list(x) == TRUE) {
                arr <- array(0L, dim = c(ord, ord, length(x)), 
                  dimnames = list(lbs, lbs, names(x)))
                for (i in seq_len(length(x))) {
                  for (k in seq_len(length(x[[i]]))) {
                    crs <- dhc(x[[i]][k], sep = sep)
                    ifelse(all(suppressWarnings(is.na(as.numeric(crs)))) == 
                      FALSE, arr[which(lbs %in% lbs[as.numeric(crs)][1]), 
                      which(lbs %in% lbs[as.numeric(crs)][2]), 
                      i] <- 1L, arr[which(lbs %in% crs[1]), which(lbs %in% 
                      crs[2]), i] <- 1L)
                  }
                  rm(k)
                }
                rm(i)
                return(arr)
            }
            else {
                mat <- matrix(0L, ncol = ord, nrow = ord, dimnames = list(lbs, 
                  lbs))
                for (k in seq_len(length(x))) {
                  crs <- dhc(x[k], sep = sep)
                  mat[which(lbs %in% crs[1]), which(lbs %in% 
                    crs[2])] <- 1L
                }
                rm(k)
                return(mat)
            }
        }
    }
}

