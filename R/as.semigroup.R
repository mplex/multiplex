## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
as.semigroup <-
function (x, gens, lbs, numerical, edgeT) 
{
    ifelse(is.list(x) == TRUE && isTRUE(length(x) == 1L) == TRUE, 
        x <- x[[1]], NA)
    ifelse(missing(numerical) == FALSE && isTRUE(numerical == 
        TRUE) == TRUE, numerical <- TRUE, numerical <- FALSE)
    if (is.array(x) == TRUE && is.na(dim(x)[3]) == FALSE && isTRUE(dim(x)[1] == 
        dim(x)[2]) == TRUE) {
        ifelse(isTRUE(numerical == TRUE) == TRUE, return(semigroup(x, 
            type = "numerical")), return(semigroup(x, type = "symbolic")))
    }
    if (isTRUE("partial.order" %in% tolower(attr(x, "class"))) == 
        TRUE) {
        stop(paste("Class of ", sQuote(deparse(substitute(x))), 
            " not supported.", sep = ""))
    }
    else {
        if (isTRUE("Semigroup" %in% attr(x, "class")) == TRUE) {
            scls <- TRUE
        }
        else {
            if (isTRUE(class(x) == "EdgeTable") == FALSE && is.null(dimnames(x)) == 
                TRUE) 
                stop(paste("Dimnames in ", sQuote(deparse(substitute(x))), 
                  " not found.", sep = ""))
            scls <- FALSE
        }
    }
    if (isTRUE(scls == TRUE) == TRUE) {
        warning(paste("Semigroup in ", sQuote(deparse(substitute(x))), 
            " is a ", dQuote(attr(x, "class")[2]), " type object.", 
            sep = ""), call. = FALSE)
        if (isTRUE(is.array(x$gens) == TRUE) == TRUE) {
            ifelse(missing(gens) == FALSE, NA, gens <- x$st[seq_len(dim(x$gens)[3])])
        }
        else {
            ifelse(missing(gens) == FALSE, NA, gens <- x$gens)
        }
        if (isTRUE(dim(x)[1] != dim(x)[2]) == TRUE) {
            xdim <- NA
        }
        else {
            xdim <- x$dim
        }
    }
    else {
        if ((missing(gens) == FALSE && all(gens %in% unlist(dimnames(x))) == 
            FALSE) && isTRUE("EdgeTable" %in% attr(x, "class")) == 
            FALSE) {
            stop(paste("Generators are not part of ", sQuote(deparse(substitute(x))), 
                ".", sep = ""))
        }
        else {
            if (missing(gens) == TRUE) {
                if (isTRUE(length(which(nchar(rownames(x)) == 
                  1L) > 0)) == TRUE) {
                  gens <- rownames(x)[which(nchar(rownames(x)) == 
                    1L)]
                  warning(paste("No generators provided in semigroup ", 
                    sQuote(deparse(substitute(x))), " and take single character string(s).", 
                    sep = ""), call. = FALSE)
                }
                else {
                  gens <- NA
                  warning(paste("No generators provided in semigroup ", 
                    sQuote(deparse(substitute(x))), ".", sep = ""), 
                    call. = FALSE)
                }
            }
        }
        if (is.null(dimnames(x)) == TRUE && isTRUE(numerical == 
            TRUE) == TRUE && (isTRUE("EdgeTable" %in% attr(x, 
            "class")) == FALSE & isTRUE("Semigroup" %in% attr(x, 
            "class")) == FALSE)) {
            dimnames(x)[[1]] <- seq_len(nrow(x))
            dimnames(x)[[2]] <- seq_len(ncol(x))
        }
        else {
            NA
        }
        if (isTRUE("EdgeTable" %in% attr(x, "class")) == TRUE || 
            isTRUE(dim(x)[1] != dim(x)[2]) == TRUE || is.matrix(x) == 
            TRUE) {
            xdim <- NULL
        }
        else {
            xdim <- dim(x)[1]
        }
    }
    if ((missing(edgeT) == FALSE && isTRUE(edgeT == TRUE) == 
        TRUE) || isTRUE("EdgeTable" %in% attr(x, "class")) == 
        TRUE) {
        ifelse(isTRUE("EdgeTable" %in% attr(x, "class")) == TRUE, 
            x <- edgS(x$ET), x <- edgS(x))
    }
    else if (is.matrix(x) == TRUE && isTRUE(dim(x)[1] != dim(x)[2]) == 
        TRUE) {
        warning(paste("Edge-list is assumed for ", sQuote(deparse(substitute(x))), 
            ".", sep = ""), call. = FALSE)
        x <- edgS(x)
    }
    if (missing(lbs) == TRUE) {
        if (isTRUE(numerical == TRUE) == FALSE) {
            if (isTRUE(scls == TRUE) == TRUE) {
                if (isTRUE("symbolic" %in% attr(x, "class")) == 
                  TRUE) {
                  return(x)
                }
                else if (isTRUE("numerical" %in% attr(x, "class")) == 
                  TRUE) {
                  s <- x$S
                  colnames(s) <- rownames(s) <- x$st
                  for (i in seq_along(dimnames(x$S)[[1]])) {
                    s[s == i] <- x$st[i]
                  }
                  rm(i)
                  lst <- list(ord = length(x$st), st = x$st, 
                    gens = x$gens, S = as.data.frame(s))
                  class(lst) <- c("Semigroup", "symbolic")
                  return(lst)
                }
            }
            else if (isTRUE(scls == FALSE) == TRUE) {
                if (is.null(attr(x, "class")) == TRUE) {
                  ifelse(is.null(dimnames(x)) == TRUE, s <- as.matrix(x), 
                    s <- as.data.frame(x))
                  if (all(rownames(x) %in% levels(unlist(x))) == 
                    TRUE || all(as.character(as.matrix(s)) %in% 
                    unlist(dimnames(s))) == TRUE) {
                    Sst <- rownames(x)
                  }
                  else {
                    ifelse(is.numeric(x) == TRUE, Sst <- unique(c(unlist(x), 
                      unique(c(rownames(x), colnames(x))))), 
                      Sst <- c(unique(unlist(x)), unique(c(rownames(x), 
                        colnames(x)))))
                  }
                  ifelse(is.numeric(gens) == TRUE && any(gens %in% 
                    unique(unlist(dimnames(x)))) == FALSE, gens <- unique(unlist(dimnames(x)))[gens], 
                    NA)
                  ifelse(all(x %in% unique(unlist(dimnames(x)))) == 
                    TRUE, lst <- list(ord = nrow(x), st = unique(unlist(dimnames(x))), 
                    gens = gens, S = s), lst <- list(ord = length(Sst), 
                    st = Sst, gens = gens, S = s))
                  class(lst) <- "Semigroup"
                  ifelse(is.character(lst$st) == TRUE, class(lst) <- c(class(lst), 
                    "symbolic"), class(lst) <- c(class(lst), 
                    "numerical"))
                  return(lst)
                }
                else {
                  lbs <- dimnames(x)[[1]]
                }
            }
        }
        else if (isTRUE(numerical == TRUE) == TRUE) {
            if (isTRUE("numerical" %in% attr(x, "class")) == 
                TRUE) {
                return(x)
            }
            else if (isTRUE(scls == TRUE) == TRUE) {
                lbs <- seq_along(x$st)
            }
            else {
                ifelse(isTRUE(dim(x)[1] == dim(x)[2]) == TRUE, 
                  lbs <- seq_len(dim(x)[1]), lbs <- NULL)
            }
        }
    }
    else if (missing(lbs) == FALSE) {
        if (isTRUE(scls == FALSE) == TRUE) {
            if (is.null(lbs) == FALSE && (any(is.na(gens) == 
                FALSE) == TRUE & is.numeric(gens) == FALSE)) {
                gens <- lbs[which(dimnames(x)[[1]] %in% gens)]
            }
            else if (is.numeric(gens) == TRUE && any(gens %in% 
                lbs) == FALSE) {
                gens <- lbs[gens]
            }
            else {
                NA
            }
        }
    }
    if (isTRUE(scls == TRUE) == TRUE) {
        s <- as.matrix(x$S)
        ord <- x$ord
        if (is.null(lbs) == FALSE && isTRUE(numerical == TRUE) == 
            TRUE) {
            Sst <- lbs
        }
        else if (isTRUE(numerical == FALSE) == TRUE && all(rownames(s) == 
            x$st) == FALSE) {
            Sst <- rownames(s)
        }
        else {
            Sst <- x$st
        }
    }
    else if (isTRUE(scls == TRUE) == FALSE) {
        if (is.array(x) == FALSE && is.data.frame(x) == FALSE) 
            stop(paste("Input data in ", sQuote(deparse(substitute(x))), 
                " must be a square matrix or data frame.", sep = ""))
        s <- as.matrix(x, rownames.force = TRUE)
        if (all(as.character(s) %in% unlist(dimnames(s))) == 
            TRUE || any(is.na(s)) == TRUE) {
            Sst <- rownames(s)
        }
        else {
            if (is.numeric(s) == TRUE) {
                Sst <- unique(c(unlist(x), unique(c(rownames(x), 
                  colnames(x)))))
            }
            else {
                ifelse(is.null(levels(unlist(x))) == TRUE, Sst <- unique(c(rownames(x), 
                  colnames(x))), Sst <- unique(levels(unlist(x)), 
                  unique(c(rownames(x), colnames(x)))))
            }
        }
        ord <- length(Sst)
    }
    if (all(unique(as.vector(s)) %in% Sst) == FALSE && isTRUE(numerical == 
        FALSE) == TRUE) 
        warning(paste("Input data in ", sQuote(deparse(substitute(x))), 
            " is not a closed semigroup.", sep = ""), call. = FALSE)
    if (is.null(dimnames(s)) == TRUE && isTRUE(dim(s)[1] == dim(s)[2]) == 
        TRUE) {
        ifelse(isTRUE(length(Sst) == dim(s)[1]) == TRUE, dimnames(s)[[1]] <- dimnames(s)[[2]] <- seq_along(Sst), 
            NA)
    }
    if (is.null(lbs) == FALSE) {
        z <- vector(length = nrow(s) * ncol(s))
        for (i in seq_along(as.matrix(s))) {
            if (isTRUE(numerical == TRUE) == TRUE && isTRUE(scls == 
                TRUE) == TRUE) {
                ifelse(any(x$st == as.matrix(s)[i]) == FALSE, 
                  z[i] <- NA, z[i] <- lbs[which(x$st == as.matrix(s)[i])])
            }
            else {
                ifelse(any(Sst == as.matrix(s)[i]) == FALSE, 
                  z[i] <- NA, z[i] <- lbs[which(Sst == as.matrix(s)[i])])
            }
        }
        rm(i)
        suppressWarnings(s <- matrix(z, nrow = nrow(s), ncol = ncol(s)))
        rm(z)
    }
    if ((is.null(dimnames(s)) == TRUE && isTRUE(dim(s)[1] == 
        dim(s)[2]) == TRUE) || isTRUE(numerical == TRUE) == TRUE) {
        rownames(s) <- colnames(s) <- lbs
    }
    else if (isTRUE(numerical == FALSE) == TRUE || isTRUE(dim(s)[1] == 
        dim(s)[2]) == FALSE) {
        if (isTRUE(all.equal(lbs, Sst) == TRUE) == TRUE && isTRUE(rownames(x) == 
            colnames(x)) == TRUE) {
            dimnames(s)[[1]] <- dimnames(s)[[2]] <- as.list(lbs)
        }
        else if (isTRUE(all.equal(lbs, Sst) == TRUE) == TRUE && 
            isTRUE(rownames(x) == colnames(x)) == FALSE) {
            rownames(s) <- lbs[(nlevels(factor(s)) + 1L):(nlevels(factor(s)) + 
                nrow(s))]
            colnames(s) <- lbs[((nlevels(factor(s)) + nrow(s) + 
                1L)):(nlevels(factor(s)) + nrow(s) + ncol(s))]
        }
        else {
            if (isTRUE(length(lbs) != length(Sst)) == TRUE) {
                ifelse(all(rownames(x) %in% lbs) == FALSE, rownames(s) <- Sst[(nlevels(factor(s)) + 
                  1L):(nlevels(factor(s)) + nrow(s))], NA)
                ifelse(all(colnames(x) %in% lbs) == FALSE, colnames(s) <- Sst[((nlevels(factor(s)) + 
                  nrow(s) + 1L)):(nlevels(factor(s)) + nrow(s) + 
                  ncol(s))], NA)
            }
            else {
                ifelse(all(rownames(x) %in% lbs) == FALSE, rownames(s) <- lbs[(nlevels(factor(s)) + 
                  1L):(nlevels(factor(s)) + nrow(s))], NA)
                ifelse(all(colnames(x) %in% lbs) == FALSE, colnames(s) <- lbs[((nlevels(factor(s)) + 
                  nrow(s) + 1L)):(nlevels(factor(s)) + nrow(s) + 
                  ncol(s))], NA)
            }
        }
    }
    else {
        NA
    }
    Sdf <- as.data.frame(s)
    if (isTRUE(numerical == TRUE) == TRUE || is.null(dimnames(s)) == 
        TRUE && isTRUE(dim(s)[1] == dim(s)[2]) == TRUE) {
        dimnames(Sdf)[[1]] <- dimnames(Sdf)[[2]] <- lbs
    }
    else {
        NA
    }
    ifelse(isTRUE(numerical == TRUE) == TRUE, lst <- list(dim = xdim, 
        ord = ord, st = as.numeric(unique(unlist(dimnames(Sdf)))), 
        gens = gens, S = Sdf), lst <- list(dim = xdim, ord = ord, 
        st = unique(unlist(dimnames(Sdf))), gens = gens, S = Sdf))
    if ((missing(edgeT) == FALSE && isTRUE(edgeT == TRUE) == 
        TRUE) || isTRUE("EdgeTable" %in% attr(x, "class")) == 
        TRUE || is.matrix(x) == TRUE || is.null(xdim) == TRUE) {
        lst$dim <- NULL
    }
    class(lst) <- "Semigroup"
    ifelse(is.character(lst$st) == TRUE, class(lst) <- c(class(lst), 
        "symbolic"), class(lst) <- c(class(lst), "numerical"))
    return(lst)
}

