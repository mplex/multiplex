rel.sys <-
function (x, type = c("tolist", "toarray"), bonds = c("entire", 
    "strong", "weak", "asym", "recp", "txch", "tent", "mixd", 
    "full"), sel = NULL, loops = FALSE, att = NULL, sep) 
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
        stop("Invalid \"bonds\" type.")
    if (all(c("strong", "weak") %in% bonds) == TRUE | isTRUE("entire" %in% 
        bonds) == TRUE) {
        bnds <- "entire"
    }
    else if (isTRUE(any(c("entire", "weak", "strong") %in% bonds)) == 
        FALSE | isTRUE(length(bonds) > 1L) == TRUE) {
        bnds <- "Mixed"
    }
    else {
        bnds <- bonds
    }
    if (match.arg(type) == "tolist") {
        if (is.array(x) == FALSE) {
            if (isTRUE(attr(x, "class") == "Rel.System") == FALSE) {
                stop("'x' must be an array or a \"Rel.System\" class object.")
            }
            else if (isTRUE(attr(x, "class") == "Rel.System") == 
                TRUE) {
                return(x)
            }
        }
        else {
            if (isTRUE(dim(x)[1] == dim(x)[2]) == FALSE) 
                stop("'x' must be a square array.")
        }
        if (is.null(att) == FALSE) {
            if (is.numeric(att) == FALSE) 
                stop("'att' must be numeric pointing the array(s) representing the attribute(s).")
            if (is.na(dim(x)[3]) == FALSE) {
                if (isTRUE(max(att) > dim(x)[3]) == TRUE) 
                  stop("Value of 'att' greater than dim(x)[3]")
            }
            else if (is.na(dim(x)[3]) == TRUE) {
                if (isTRUE(max(att) > 1L) == TRUE) 
                  stop("Value of 'att' greater than dim(x)[3]")
            }
            ats <- bundles(x, collapse = FALSE, loops = TRUE, 
                sep = sep)[[7]][att]
        }
        else if (is.null(att) == TRUE) {
            ats <- logical(0)
        }
        if (is.na(dim(x)[3]) == FALSE) {
            if (isTRUE(all(seq(dim(x)[3]) %in% att)) == FALSE) {
                bd <- bundles(x[, , which(!(seq(dim(x)[3]) %in% 
                  att))], collapse = FALSE, loops = loops, sep = sep)
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
            stop("Relational system chosen is empty!")
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
        if (is.null(lbd) == FALSE) {
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
        }
        else {
            stb <- lbd
        }
        if (is.null(sel) == FALSE) {
            if (is.array(sel) == TRUE) {
                ifelse(is.na(dim(sel)[3]) == TRUE | isTRUE(dim(sel)[3] == 
                  1L) == TRUE, sel <- diag(sel), sel <- diag(mnplx(sel)))
                sel <- as.vector(attr(which(!(sel == 0)), "names"))
            }
            if (is.null(dimnames(x)) == FALSE) {
                ifelse(isTRUE(is.numeric(sel) == TRUE) == TRUE, 
                  Sel <- dimnames(x)[[1]][sel], Sel <- sel)
            }
            else {
                Sel <- sel
            }
            ntsel <- list()
            length(ntsel) <- length(stb)
            for (k in seq_len(length(stb))) {
                tss <- which(dhc(stb[[k]], sep = sep) %in% Sel)
                if (isTRUE(length(tss) > 0) == TRUE) {
                  tmpsel <- vector()
                  for (i in seq_len(length(tss))) {
                    if (isTRUE((tss[i]%%2L) == 1L) == TRUE) {
                      tmpsel <- append(tmpsel, stb[[k]][ceiling(tss[i]/2L)])
                    }
                    else if (isTRUE((tss[i]%%2L) == 1L) == FALSE) {
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
        if (length(stb) > 0L) {
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
            note <- "Input labels in 'x' are NULL."
            lbs <- seq_len(dim(x)[1])
        }
        else {
            note <- NULL
            lbs <- dimnames(x)[[1]]
        }
        if (isTRUE(length(ats) > 0L) == TRUE) {
            ifelse(length(note) > 0L, RS <- (list(ord = dim(x)[1], 
                nodes = lbs, sel = sel, sys.ord = nlevels(factor(ties)), 
                incl = lbs[which(lbs %in% levels(factor(ties)))], 
                excl = lbs[which(!(lbs %in% levels(factor(ties))))], 
                bond.type = bonds, size = length(unlist(stb)), 
                Note = note, sep = sep, Ties = stb, Attrs.ord = length(unlist(ats)), 
                Attrs = jnt(dhc(ats, sep = sep), sep = sep))), 
                RS <- (list(ord = dim(x)[1], nodes = lbs, sel = sel, 
                  sys.ord = nlevels(factor(ties)), incl = lbs[which(lbs %in% 
                    levels(factor(ties)))], excl = lbs[which(!(lbs %in% 
                    levels(factor(ties))))], bond.type = bonds, 
                  size = length(unlist(stb)), sep = sep, Ties = stb, 
                  Attrs.ord = length(unlist(ats)), Attrs = jnt(dhc(ats, 
                    sep = sep), sep = sep))))
        }
        else {
            ifelse(isTRUE(length(note) > 0L) == TRUE, RS <- (list(ord = dim(x)[1], 
                nodes = lbs, sel = sel, sys.ord = nlevels(factor(ties)), 
                incl = lbs[which(lbs %in% levels(factor(ties)))], 
                excl = lbs[which(!(lbs %in% levels(factor(ties))))], 
                bond.type = bonds, size = length(unlist(stb)), 
                Note = note, sep = sep, Ties = stb)), RS <- (list(ord = dim(x)[1], 
                nodes = lbs, sel = sel, sys.ord = nlevels(factor(ties)), 
                incl = lbs[which(lbs %in% levels(factor(ties)))], 
                excl = lbs[which(!(lbs %in% levels(factor(ties))))], 
                bond.type = bonds, size = length(unlist(stb)), 
                sep = sep, Ties = stb)))
        }
        class(RS) <- "Rel.System"
        return(RS)
    }
    else if (match.arg(type) == "toarray") {
        tmp <- x
        if (isTRUE(attr(x, "class") == "Rel.System") == FALSE) {
            if (is.null(sel) == FALSE) {
                if (is.array(sel) == TRUE) {
                  ifelse(is.na(dim(sel)[3]) == TRUE | isTRUE(dim(sel)[3] == 
                    1L) == TRUE, sel <- diag(sel), sel <- diag(mnplx(sel)))
                  sel <- as.vector(attr(which(!(sel == 0L)), 
                    "names"))
                }
                else {
                  NA
                }
                ifelse(isTRUE(is.numeric(sel) == TRUE) == TRUE, 
                  Sel <- dimnames(x)[[1]][sel], Sel <- sel)
                if (isTRUE(Sel == "att") == TRUE | isTRUE(Sel == 
                  "noatt") == TRUE) {
                  x <- rel.sys(tmp, type = "tolist", bonds = bonds, 
                    loops = loops, att = att)
                }
                else {
                  ifelse(is.na(dim(tmp)[3]) == TRUE, return(tmp[which(dimnames(tmp)[[1]] %in% 
                    Sel), which(dimnames(tmp)[[1]] %in% Sel)]), 
                    return(tmp[which(dimnames(tmp)[[1]] %in% 
                      Sel), which(dimnames(tmp)[[1]] %in% Sel), 
                      ]))
                }
            }
            else {
                x <- rel.sys(tmp, type = "tolist", bonds = bonds, 
                  loops = loops, att = att)
            }
            ifelse(is.na(dim(tmp)[3]) == TRUE, x$Ties <- unlist(x$Ties), 
                NA)
        }
        if (isTRUE(attr(x, "class") == "Rel.System") == TRUE) {
            if (isTRUE(x$sys.ord == 0L) == TRUE) 
                stop("Relational system chosen is empty!")
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
                  warning("selection is not part of 'x'.")
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
                  arr[, , i] <- trnf(x$Ties[[i]], tolist = FALSE, 
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
    else {
        stop("Input not recognizable!!")
    }
}
