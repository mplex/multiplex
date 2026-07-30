## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
rel.sys <-
function (x, type = c("tolist", "toarray"), bonds = c("entire", 
    "strong", "weak", "asym", "recp", "txch", "tent", "mixd", 
    "full"), loops = FALSE, sel = NULL, att = NULL, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    ifelse(missing(bonds) == TRUE, bonds <- "entire", NA)
    if (isTRUE(att == 0L) == TRUE) {
        att <- NULL
    }
    else {
        NA
    }
    if (isTRUE(any(c("entire", "strong", "weak", "asym", "recp", 
        "txch", "tent", "mixd", "full") %in% bonds)) == FALSE) 
        stop(paste(sQuote("bonds"), " type should be ", dQuote("entire"), 
            ", ", dQuote("strong"), ", ", dQuote("weak"), ", ", 
            dQuote("asym"), ", ", dQuote("recp"), ", ", dQuote("txch"), 
            ", ", dQuote("tent"), ", ", dQuote("mixd"), " or ", 
            dQuote("full"), ".", sep = ""))
    if (all(c("strong", "weak") %in% bonds) == TRUE | isTRUE("entire" %in% 
        bonds) == TRUE) {
        bnds <- "entire"
    }
    else if (any(c("entire", "weak", "strong") %in% bonds) == 
        TRUE | isTRUE(length(bonds) > 1L) == TRUE) {
        bnds <- "Mixed"
    }
    else {
        bnds <- bonds
    }
    if (isTRUE("Rel.System" %in% attr(x, "class")) == FALSE) {
        if (is.array(x) == TRUE && is.null(dimnames(x)) == TRUE) {
            dimnames(x)[[1]] <- dimnames(x)[[2]] <- seq_len(dim(x)[1])
        }
        else {
            NA
        }
        if (is.null(att) == FALSE) {
            if (is.numeric(att) == FALSE) 
                stop(paste("Attribute(s) in ", sQuote(deparse(substitute(att))), 
                  " must be numeric pointing the array(s) representing it(them).", 
                  sep = ""))
            if (is.na(dim(x)[3]) == FALSE) {
                if (isTRUE(max(att) > dim(x)[3]) == TRUE) 
                  stop(paste("Value of ", sQuote(deparse(substitute(att))), 
                    " greater than third dimension in ", sQuote(deparse(substitute(x))), 
                    ".", sep = ""))
            }
            else if (is.na(dim(x)[3]) == TRUE) {
                if (isTRUE(max(att) > 1L) == TRUE) 
                  stop(paste("Value of ", sQuote(deparse(substitute(att))), 
                    " greater than third dimension in ", sQuote(deparse(substitute(x))), 
                    ".", sep = ""))
            }
            ats <- bundles(x, collapse = FALSE, loops = TRUE, 
                sep = sep)[[7]][att]
        }
        else if (is.null(att) == TRUE) {
            ats <- logical(0)
        }
    }
    if (match.arg(type) == "tolist") {
        if (is.array(x) == FALSE) {
            if (isTRUE("Rel.System" %in% attr(x, "class")) == 
                FALSE) {
                stop(paste("For ", sQuote("tolist"), " type, ", 
                  sQuote(deparse(substitute(x))), " must be a square array or a ", 
                  dQuote("Rel.System"), " class object.", sep = ""))
            }
            else if (isTRUE("Rel.System" %in% attr(x, "class")) == 
                TRUE) {
                message(paste(dQuote("Rel.System"), " class input is yet to implement.", 
                  sep = ""))
                return(x)
            }
        }
        else {
            if (isTRUE(dim(x)[1] == dim(x)[2]) == FALSE) 
                stop(paste("Arrays in ", sQuote(deparse(substitute(x))), 
                  " must be square.", sep = ""))
        }
        if (is.na(dim(x)[3]) == FALSE && isTRUE(length(ats) == 
            0) == TRUE) {
            if (isTRUE(all(seq(dim(x)[3]) %in% att)) == FALSE) {
                x3D <- x[, , which(!(seq(dim(x)[3]) %in% att))]
                bd <- bundles(x3D, collapse = FALSE, loops = loops, 
                  sep = sep)
            }
            else if (isTRUE(all(seq(dim(x)[3]) %in% att)) == 
                TRUE) {
                bd <- NULL
            }
        }
        else {
            bd <- bundles(x, collapse = FALSE, loops = loops, 
                sep = sep)
        }
        if (isTRUE(length(unlist(bd)) == 0L) == TRUE) 
            stop("Relational system chosen is empty.")
        if ((bnds) == "entire") {
            lbd <- bd
        }
        else if ((bnds) == "strong") {
            lbd <- list(bd$recp, bd$txch, bd$mixd, bd$full)
        }
        else if ((bnds) == "weak") {
            lbd <- list(bd$asym, bd$tent)
        }
        else {
            if ((bnds) == "Mixed") {
                ifelse(isTRUE("weak" %in% bonds) == TRUE, bonds <- unique(c(bonds, 
                  c("asym", "tent"))), NA)
                ifelse(isTRUE("strong" %in% bonds) == TRUE, bonds <- unique(c(bonds, 
                  c("txch", "mixd", "full"))), NA)
            }
            else {
                NA
            }
            lbd <- bd[which(attr(bd, "names") %in% bonds)]
        }
        if (is.null(sel) == FALSE) {
            if (isTRUE(length(sel) == 1) == TRUE) {
                if (isTRUE(sel %in% dimnames(x)[[1]]) == FALSE) {
                  return(NULL)
                }
                else {
                  if (is.na(dim(x)[3]) == TRUE) {
                    stb <- unlist(jnt(Filter(function(z) sel %in% 
                      z, strsplit(trnf(x, tolist = TRUE, lb2lb = TRUE, 
                      sep = sep), split = sep)), sep = sep))
                    if (is.null(stb) == TRUE) {
                      stb <- c(paste(sel, names(which(lapply(x[which(dimnames(x)[[1]] == 
                        sel), ], sum) > 0)), sep = sep), paste(names(which(lapply(x[, 
                        which(dimnames(x)[[1]] == sel)], sum) > 
                        0)), sel, sep = sep))
                    }
                    else {
                      NA
                    }
                  }
                  else {
                    stb <- vector()
                    for (k in seq_len(dim(x)[3])) {
                      tmpstb <- unlist(jnt(Filter(function(z) sel %in% 
                        z, strsplit(trnf(x[, , k], tolist = TRUE, 
                        lb2lb = TRUE, sep = sep), split = sep)), 
                        sep = sep))
                      if (is.null(tmpstb) == TRUE) {
                        stb <- c(paste(sel, names(which(lapply(x[which(dimnames(x)[[1]] == 
                          sel), , k], sum) > 0)), sep = sep), 
                          paste(names(which(lapply(x[, which(dimnames(x)[[1]] == 
                            sel), k], sum) > 0)), sel, sep = sep))
                      }
                      else {
                        NA
                      }
                      stb <- append(stb, tmpstb)
                    }
                    rm(k)
                    stb <- unique(stb)
                  }
                  ties <- unique(dhc(stb))
                }
            }
            else if (isTRUE(length(sel) > 1) == TRUE) {
                inn <- vector()
                out <- vector()
                if (is.na(dim(x)[3]) == TRUE) {
                  for (i in sel) {
                    if (any(dimnames(x)[[1]] == i) == TRUE) {
                      inn <- append(inn, paste(i, names(which(lapply(x[which(dimnames(x)[[1]] == 
                        i), ], sum) > 0)), sep = sep))
                      out <- append(out, paste(names(which(lapply(x[, 
                        which(dimnames(x)[[1]] == i)], sum) > 
                        0)), i, sep = sep))
                    }
                    else {
                      NA
                    }
                  }
                  rm(i)
                }
                else if (is.na(dim(x)[3]) == FALSE) {
                  for (k in seq_len(dim(x)[3])) {
                    for (i in sel) {
                      if (any(dimnames(x)[[1]] == i) == TRUE) {
                        inn <- append(inn, paste(i, names(which(lapply(x[which(dimnames(x)[[1]] == 
                          i), , k], sum) > 0)), sep = sep))
                        out <- append(out, paste(names(which(lapply(x[, 
                          which(dimnames(x)[[1]] == i), k], sum) > 
                          0)), i, sep = sep))
                      }
                      else {
                        NA
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                inn <- inn[which(unlist(lapply(lapply(as.list(inn), 
                  dhc), length)) == 2L)]
                out <- out[which(unlist(lapply(lapply(as.list(swp(out)), 
                  dhc), length)) == 2L)]
                stb <- unique(c(inn, out))
                ties <- unique(dhc(stb))
            }
            else {
                NA
            }
            if (is.null(att) == FALSE) {
                if (is.na(dim(x)[3]) == FALSE && isTRUE((dim(x)[3] - 
                  length(att)) == 0L) == FALSE) {
                  stb <- list()
                  for (k in seq_len(dim(x)[3] - length(att))) {
                    tmp <- vector()
                    for (i in seq_len(length(lbd))) {
                      if (isTRUE(length(lbd[[i]]) > 0L) == TRUE) {
                        ifelse(is.na(dim(x[, , which(!(seq(dim(x)[3]) %in% 
                          att))])[3]) == TRUE, tmp <- append(tmp, 
                          lbd[[i]]), tmp <- append(tmp, lbd[[i]][k]))
                      }
                    }
                    rm(i)
                    stb[[k]] <- as.vector(unlist(tmp))
                  }
                  rm(k)
                }
                else {
                  stb <- vector()
                  for (i in seq_len(length(lbd))) {
                    stb <- append(stb, lbd[[i]])
                  }
                  rm(i)
                }
                if (is.array(sel) == TRUE) {
                  ifelse(is.na(dim(sel)[3]) == TRUE | isTRUE(dim(sel)[3] == 
                    1L) == TRUE, sel <- diag(sel), sel <- diag(mnplx(sel)))
                  sel <- as.vector(attr(which(!(sel == 0)), "names"))
                }
                if (is.null(dimnames(x)) == FALSE) {
                  ifelse(isTRUE(is.numeric(sel) == TRUE) == TRUE, 
                    sel <- as.character(sel), NA)
                }
                else {
                  NA
                }
                ntsel <- list()
                length(ntsel) <- length(stb)
                for (k in seq_len(length(stb))) {
                  tss <- which(dhc(stb[[k]], sep = sep) %in% 
                    sel)
                  if (isTRUE(length(tss) > 0) == TRUE) {
                    tmpsel <- vector()
                    for (i in seq_len(length(tss))) {
                      if (isTRUE((tss[i]%%2L) == 1L) == TRUE) {
                        tmpsel <- append(tmpsel, stb[[k]][ceiling(tss[i]/2L)])
                      }
                      else if (isTRUE((tss[i]%%2L) == 1L) == 
                        FALSE) {
                        tmpsel <- append(tmpsel, stb[[k]][floor(tss[i]/2L)])
                      }
                    }
                    rm(i)
                    ntsel[[k]] <- unique(as.vector(unlist(tmpsel)))
                  }
                }
                rm(k)
                rm(tss)
                stb <- ntsel
            }
            else {
                NA
            }
        }
        else if (is.null(sel) == TRUE) {
            if (is.null(att) == FALSE) {
                stb <- transf(x[, , which(!(seq_len(dim(x)[3]) %in% 
                  att))], type = "tolist", lb2lb = TRUE, sep = sep)
            }
            else {
                ifelse(is.na(dim(x)[3]) == TRUE, stb <- trnf(x, 
                  tolist = TRUE, lb2lb = TRUE, sep = sep), stb <- transf(x, 
                  type = "tolist", lb2lb = TRUE, sep = sep, valued = TRUE))
            }
        }
        if (length(stb) > 0L && is.null(unlist(stb)) == FALSE) {
            ties <- vector()
            for (k in seq_len(length(stb))) {
                for (i in seq_len(length(stb[[k]]))) {
                  if (isTRUE(length(stb[[k]]) > 0L) == TRUE) {
                    ties <- append(ties, dhc(stb[[k]][i], sep = sep))
                  }
                }
                rm(i)
            }
            rm(k)
        }
        else {
            ties <- stb <- character(0)
        }
        ifelse(is.na(dim(x)[3]) == TRUE, stb <- unlist(stb), 
            NA)
        if (is.na(dim(x)[3]) == FALSE) {
            if (is.null(att) == TRUE) {
                ifelse(is.null(dimnames(x)[[3]]) == TRUE, attr(stb, 
                  "names") <- seq_len(dim(x)[3] - length(att)), 
                  attr(stb, "names") <- dimnames(x)[[3]])
            }
            else if (is.null(att) == FALSE) {
                ifelse(is.null(dimnames(x)[[3]]) == TRUE, attr(stb, 
                  "names") <- which(!(seq(dim(x)[3]) %in% att)), 
                  attr(stb, "names") <- dimnames(x)[[3]][which(!(seq(dim(x)[3]) %in% 
                    att))])
            }
        }
        if (is.null(dimnames(x)[[1]]) == TRUE) {
            warning(paste("Input labels in ", sQuote(deparse(substitute(x))), 
                " are NULL.", sep = ""), call. = FALSE)
            lbs <- seq_len(dim(x)[1])
        }
        else {
            lbs <- dimnames(x)[[1]]
        }
        if (isTRUE(length(ats) > 0L) == TRUE) {
            rs <- (list(ord = dim(x)[1], nodes = lbs, sel = sel, 
                sys.ord = nlevels(factor(ties)), incl = lbs[which(lbs %in% 
                  levels(factor(ties)))], excl = lbs[which(!(lbs %in% 
                  levels(factor(ties))))], bond.type = bonds, 
                size = length(unlist(stb)), sep = sep, Ties = as.vector(stb), 
                Attrs.ord = length(unlist(ats)), Attrs = jnt(dhc(ats, 
                  sep = sep), sep = sep)))
        }
        else {
            rs <- (list(ord = dim(x)[1], nodes = lbs, sel = sel, 
                sys.ord = nlevels(factor(ties)), incl = lbs[which(lbs %in% 
                  levels(factor(ties)))], excl = lbs[which(!(lbs %in% 
                  levels(factor(ties))))], bond.type = bonds, 
                size = length(unlist(stb)), sep = sep, Ties = as.vector(stb)))
        }
        if (is.null(sel) == TRUE) {
            rs$sel <- NULL
        }
        else {
            NA
        }
        class(rs) <- "Rel.System"
        return(rs)
    }
    else if (match.arg(type) == "toarray") {
        if (is.null(att) == FALSE && isTRUE(length(ats) > 0L) == 
            TRUE) {
            return(transf(ats$a, type = "toarray", sort = TRUE))
        }
        else {
            NA
        }
        if (isTRUE("Rel.System" %in% attr(x, "class")) == FALSE) {
            ifelse(isTRUE(is.array(x) == TRUE) == TRUE, arr <- x, 
                arr <- transf(x, type = "toarray", sort = TRUE))
            if (is.null(sel) == FALSE) {
                if (isTRUE(length(sel) == 1) == TRUE || any(sel %in% 
                  dimnames(arr)[[1]]) == FALSE) {
                  return(NULL)
                }
                else {
                  kual <- which(dimnames(arr)[[1]] %in% sel)
                  ifelse(is.na(dim(arr)[3]) == TRUE, arr <- arr[kual, 
                    kual], arr <- arr[kual, kual, ])
                }
            }
            else {
                NA
            }
            if (isTRUE(bnds == "entire") == TRUE) {
                return(arr)
            }
            else {
                bd <- bundles(arr, collapse = FALSE, loops = loops, 
                  sep = sep)
                if ((bnds) == "strong") {
                  lbd <- list(bd$recp, bd$txch, bd$mixd, bd$full)
                }
                else if ((bnds) == "weak") {
                  lbd <- list(bd$asym, bd$tent)
                }
                else {
                  if ((bnds) == "Mixed") {
                    ifelse(isTRUE("weak" %in% bonds) == TRUE, 
                      bonds <- unique(c(bonds, c("asym", "tent"))), 
                      NA)
                    ifelse(isTRUE("strong" %in% bonds) == TRUE, 
                      bonds <- unique(c(bonds, c("txch", "mixd", 
                        "full"))), NA)
                  }
                  else {
                    NA
                  }
                  lbd <- bd[which(attr(bd, "names") %in% bonds)]
                }
                ifelse(is.na(dim(arr)[3]) == FALSE, tmp <- vector(mode = "list", 
                  length = dim(arr)[3]), tmp <- vector(mode = "list", 
                  length = 1))
                for (k in seq_len(length(tmp))) {
                  tmpp <- lapply(lbd, function(z) {
                    z[[k]]
                  })
                  tmp[[k]] <- unlist(tmpp)
                }
                rm(k, tmpp)
                attr(tmp, "names") <- dimnames(arr)[[3]]
                return(transf(tmp, type = "toarray", sort = TRUE))
            }
        }
        else if (isTRUE("Rel.System" %in% attr(x, "class")) == 
            TRUE) {
            if (isTRUE(x$sys.ord == 0L) == TRUE) 
                stop("Relational system chosen is empty.")
            if (is.null(sel) == TRUE) {
                n <- x$sys.ord
                r <- length(x$Ties)
                lbs <- x$incl
                lbst <- attr(x$Ties, "names")
            }
            else if (is.null(sel) == FALSE) {
                if (isTRUE(sel == "att") == TRUE) {
                  sel <- x$nodes[which(x$nodes %in% unlist(dhc(x$Attrs, 
                    sep = sep)))]
                }
                else if (isTRUE(sel == "noatt") == TRUE) {
                  sel <- x$nodes[which(!(x$nodes %in% unlist(dhc(x$Attrs, 
                    sep = sep))))]
                }
                else if (isTRUE(any(sel %in% x$nodes)) == FALSE) {
                  warning(paste("Selection is not part of ", 
                    sQuote("x"), ".", sep = ""), call. = FALSE)
                  return(tmp)
                }
                else {
                  NA
                }
                lbst <- vector()
                ntsel <- list()
                for (k in seq_len(length(x$Ties))) {
                  tss <- which(dhc(x$Ties[[k]], sep = sep) %in% 
                    sel)
                  if (isTRUE(length(tss) > 0L) == TRUE) {
                    tmpsel <- vector()
                    for (i in seq_len(length(tss))) {
                      if (isTRUE((tss[i]%%2L) == 1L) == TRUE) {
                        tmpsel <- append(tmpsel, x$Ties[[k]][ceiling(tss[i]/2L)])
                      }
                      else {
                        tmpsel <- append(tmpsel, x$Ties[[k]][floor(tss[i]/2L)])
                      }
                    }
                    rm(i)
                    ntsel[[k]] <- as.vector(unlist(tmpsel))
                    lbst <- append(lbst, attr(x$Ties, "names")[k])
                  }
                  else {
                    NA
                  }
                }
                rm(k)
                ntsel <- ntsel[unlist(lapply(ntsel, length) != 
                  0L)]
                attr(ntsel, "names") <- lbst
                x$Ties <- ntsel
                lbs <- unique(dhc(unlist(ntsel), sep = sep))
                n <- length(lbs)
                r <- length(lbst)
            }
            else if (is.null(sel) == FALSE && isTRUE(attr(x, 
                "class")[1] == "array") == TRUE) {
            }
            else {
                n <- length(x$sel)
                r <- 1L
                lbs <- x$sel
                lbst <- NULL
            }
            arr <- array(0L, dim = c(n, n, r))
            dimnames(arr)[[1]] <- dimnames(arr)[[2]] <- lbs
            if (isTRUE(n > 0L) == TRUE) 
                dimnames(arr)[[3]] <- lbst
            for (i in seq_len(r)) {
                if (isTRUE(length(x$Ties[[i]]) > 0L) == TRUE && 
                  isTRUE(n > 0L) == TRUE) {
                  arr[, , i] <- transf(x$Ties[[i]], type = "toarray", 
                    ord = n, lbs = lbs, lb2lb = TRUE)
                }
                else {
                  NA
                }
            }
            rm(i)
            if (is.null(x$Attrs) == FALSE) {
                arra <- array(0L, dim = c(n, n, length(x$Attrs)))
                dimnames(arra)[[1]] <- dimnames(arra)[[2]] <- lbs
                if (isTRUE(n > 0L) == TRUE) 
                  dimnames(arra)[[3]] <- attr(x$Attrs, "names")
                for (i in seq_len(length(x$Attrs))) {
                  act <- dhc(x$Attrs[[i]], sep = sep)
                  if (isTRUE(length(act) > 0L) == TRUE) {
                    diag(arra[, , i])[which(lbs %in% dhc(x$Attrs[[i]], 
                      sep = sep))] <- 1L
                  }
                }
                rm(i)
                attrs <- dim(arr)[3]
                arr <- zbind(arr, arra)
                if (isTRUE(dim(arra)[3] > 1L) == TRUE) {
                  class(arr) <- c("array", paste("Attrs.", paste(attrs + 
                    1L, dim(arr)[3], sep = ","), sep = " : "))
                }
                else {
                  class(arr) <- c("array", paste("Attrs.", dim(arr)[3], 
                    sep = " : "))
                }
            }
            return(arr)
        }
    }
}

