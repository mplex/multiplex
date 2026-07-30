## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
edgel <-
function (x, toarray = TRUE, header = TRUE, sep = "\t", valued = FALSE, 
    inc = FALSE, rownames = TRUE, add = NULL, na.rm) 
{
    if (is.array(x) != TRUE && is.data.frame(x) != TRUE && is.list(x) != 
        TRUE) {
        x <- utils::read.table(x, header = header, sep = sep)
        x0 <- "x"
    }
    else {
        x0 <- deparse(substitute(x))
        if (is.array(x) == TRUE && isTRUE(dim(x)[1] == dim(x)[2]) == 
            TRUE) {
            if (isTRUE(toarray == TRUE) == TRUE) {
                warning(paste("Array ", sQuote(x0), " to array.", 
                  sep = ""), call. = FALSE)
                return(x)
            }
            else if (isTRUE(inc == TRUE) == TRUE) {
                inc <- FALSE
                warning(paste(sQuote("inc"), " is set to FALSE since ", 
                  sQuote(x0), " is array.", sep = ""), call. = FALSE)
            }
        }
        else if ((is.list(x) == TRUE && is.data.frame(x) == FALSE) && 
            isTRUE(toarray == TRUE) == TRUE) {
            stop("Use a transformation function for a list to array.", 
                call. = FALSE)
        }
        else {
            NA
        }
    }
    if (isTRUE("edgelist" %in% class(x)) == TRUE) {
        inc <- FALSE
        warning(paste(sQuote(deparse(substitute(x))), " is an ", 
            dQuote("edgelist"), " class object.", sep = ""), 
            call. = FALSE)
    }
    if (isTRUE(inc == TRUE) == TRUE) {
        if (isTRUE(is.data.frame(x) == TRUE) == TRUE && is.character(x[, 
            1]) == TRUE) {
            if (isTRUE(toarray == FALSE) == TRUE && isTRUE(rownames == 
                TRUE) == TRUE) {
                x2 <- x[, 2:ncol(x)]
                rownames(x2) <- x[, 1]
                ifelse(isTRUE(valued == FALSE) == TRUE, x <- dichot(x2, 
                  c = 1L), x <- x2)
                rm(x2)
            }
            else {
                NA
            }
        }
        else {
            if (isTRUE(is.data.frame(x) == TRUE) == TRUE && isTRUE(toarray == 
                FALSE) == TRUE) {
                return(x)
            }
            else {
                if (any(duplicated(x[, 1])) == TRUE && isTRUE(rownames == 
                  FALSE) == TRUE) {
                  stop(paste("Duplicated objects/actors in incidence matrix: ", 
                    sQuote(x0), " not allowed.", sep = ""), call. = FALSE)
                }
                else {
                  NA
                }
            }
        }
        if (isTRUE(toarray == FALSE) == TRUE) {
            ifelse(isTRUE(rownames == TRUE) == TRUE, warning(paste("Objects/actors in incidence matrix ", 
                sQuote(x0), " are the ", dQuote("rownames"), 
                ".", sep = ""), call. = FALSE), warning(paste("Objects/actors in incidence matrix ", 
                sQuote(x0), " are in column: ", dQuote(colnames(x)[1]), 
                ".", sep = ""), call. = FALSE))
        }
    }
    else if (isTRUE(inc == FALSE) == TRUE) {
        if (isTRUE(is.data.frame(x) == TRUE) == TRUE || isTRUE(dim(x)[1] != 
            dim(x)[2]) == TRUE) {
            NA
        }
    }
    if (is.null(add) == FALSE) {
        levels(x[, 1]) <- c(levels(x[, 1]), add)
        levels(x[, 2]) <- c(levels(x[, 2]), add)
        for (i in seq_len(length(add))) {
            x <- rbind(x, c(add[i], add[i], NA))
        }
        rm(i)
    }
    if (isTRUE(toarray == TRUE) == TRUE) {
        if (missing(na.rm) == FALSE && isTRUE(na.rm == FALSE) == 
            TRUE) {
            if (any(is.na(x)) == TRUE) 
                warning(paste("Missing information in ", sQuote(x0), 
                  " recorded as ", sQuote("NA"), ".", sep = ""), 
                  call. = FALSE)
            ifelse(is.data.frame(x) == FALSE, NA, x[is.na(x)] <- "NA")
        }
        else {
            NA
        }
        if (isTRUE(inc == FALSE) == TRUE) {
            warning(paste("Edge-list ", deparse(substitute(x0)), 
                " to array.", sep = ""), call. = FALSE)
            if (isTRUE((ncol(x) - 2L) == 0L) == TRUE) {
                warning(paste("One type of relation assumed in ", 
                  sQuote(x0), ".", sep = ""), call. = FALSE)
                x <- cbind(x, t = rep(1L, nrow(x)))
            }
            else {
                warning(paste("In ", sQuote(x0), ", sender - receiver relations are: ", 
                  paste(dQuote(colnames(x)[1]), dQuote(colnames(x)[2]), 
                    sep = " - "), ".", sep = ""), call. = FALSE)
            }
            r <- (ncol(x) - 2L)
            x <- x[stats::complete.cases(x[, seq_len(2)]), ]
            lbs <- unique(c(as.vector(x[, 1]), as.vector(x[, 
                2])))
            if (isTRUE(lbs == "") == TRUE) {
                warning("Node labels in the input are empty!")
                lbs <- seq_len(nrow(x))
            }
            else {
                NA
            }
            n <- length(lbs)
        }
        else if (isTRUE(inc == TRUE) == TRUE) {
            warning(paste("Incidence matrix ", sQuote(x0), " to array.", 
                sep = ""), call. = FALSE)
            if (is.data.frame(x) == TRUE && any(is.na(suppressWarnings(as.numeric(rownames(x))))) == 
                FALSE && any(duplicated(x[, 1])) == FALSE) {
                lbs <- x[, 1]
                r <- ncol(x) - 1L
                rownames <- FALSE
            }
            else {
                lbs <- rownames(x)
                r <- ncol(x)
            }
            n <- nrow(x)
        }
        if (isTRUE(r == 1L) == TRUE) {
            MAT <- array(0L, dim = c(n, n))
        }
        else if (isTRUE(r > 1L) == TRUE) {
            MAT <- array(0L, dim = c(n, n, r))
            if (isTRUE(inc == TRUE) == TRUE) {
                if (isTRUE(rownames == TRUE) == TRUE || (isTRUE(rownames == 
                  FALSE) == TRUE && is.null(attr(x, "names")) == 
                  TRUE)) {
                  dimnames(MAT)[[3]] <- colnames(x)
                }
                else {
                  ifelse(is.null(attr(x, "names")) == FALSE, 
                    dimnames(MAT)[[3]] <- attr(x, "names")[2:ncol(x)], 
                    NA)
                }
            }
            else {
                ifelse(is.null(attr(x, "names")) == TRUE, dimnames(MAT)[[3]] <- colnames(x)[3:ncol(x)], 
                  dimnames(MAT)[[3]] <- attr(x, "names")[3:ncol(x)])
            }
        }
        dimnames(MAT)[[1]] <- dimnames(MAT)[[2]] <- lbs
        if (isTRUE(inc == TRUE) == TRUE) {
            if (isTRUE(r == 1L) == TRUE) {
                diag(MAT) <- as.numeric(x[, 2])
            }
            else if (isTRUE(r > 1L) == TRUE) {
                if (isTRUE(rownames == TRUE) == TRUE || (isTRUE(rownames == 
                  FALSE) == TRUE && is.null(attr(x, "names")) == 
                  TRUE)) {
                  for (k in seq_len(r)) {
                    diag(MAT[, , k]) <- x[, k]
                  }
                  rm(k)
                }
                else if (isTRUE(isTRUE(rownames == FALSE) == 
                  TRUE) == TRUE) {
                  for (k in seq_len(r)) {
                    diag(MAT[, , k]) <- x[, k + 1L]
                  }
                  rm(k)
                }
            }
        }
        else {
            if (isTRUE(ncol(x) > 3L) == TRUE) {
                for (k in seq_len(r)) {
                  rel <- which(x[, (k + 2L)] != 0L)
                  rrel <- x[rel, ]
                  if (isTRUE(length(rel) > 1L) == TRUE) {
                    X <- n
                    for (i in seq_len(n)) {
                      X[i] <- sum(as.numeric(rrel[, 1] == lbs[i]))
                    }
                    rm(i)
                    attr(X, "names") <- lbs
                    xx <- vector()
                    for (i in seq_len(n)) {
                      ifelse(X[i] != 0L, xx[i] <- i, xx[i] <- NA)
                    }
                    rm(i)
                    attr(xx, "names") <- lbs
                    xx <- (stats::na.omit(xx))
                    xx <- as.vector(attr(xx, "names"))
                    nX <- X[which(X > 0L)]
                    YY <- vector()
                    if (isTRUE(length(xx) > 1L) == TRUE) {
                      for (i in seq_len(length(xx))) {
                        YY <- rrel[, 2][which(rrel[, 1] == attr(nX, 
                          "names")[i])]
                        if (isTRUE(length(YY) > 1L) == TRUE) {
                          for (j in seq_len(length(YY))) {
                            tmp <- MAT[(which((as.vector(rownames(MAT)) == 
                              xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                              YY[j]), arr.ind = TRUE)), (k)]
                            MAT[(which((as.vector(rownames(MAT)) == 
                              xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                              YY[j]), arr.ind = TRUE)), (k)] <- tmp + 
                              as.numeric(as.vector(rrel[, (k + 
                                2L)][which(rrel[, 1] == attr(nX, 
                                "names")[i])]))[j]
                          }
                          rm(j)
                        }
                        else if (isTRUE(length(YY) == 1L) == 
                          TRUE) {
                          tmp <- MAT[(which((as.vector(rownames(MAT)) == 
                            xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                            YY), arr.ind = TRUE)), (k)]
                          MAT[(which((as.vector(rownames(MAT)) == 
                            xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                            YY), arr.ind = TRUE)), (k)] <- tmp + 
                            as.numeric(as.vector(rrel[, (k + 
                              2L)][which(rrel[, 1] == attr(nX, 
                              "names")[i])]))
                        }
                      }
                      rm(i)
                      rm(xx, YY)
                    }
                    else if (isTRUE(length(xx) == 1L) == TRUE) {
                      YY <- rrel[, 2][which(rrel[, 1] == attr(nX, 
                        "names"))]
                      if (isTRUE(length(YY) > 1L) == TRUE) {
                        for (j in seq_len(length(YY))) {
                          tmp <- MAT[(which((as.vector(rownames(MAT)) == 
                            xx), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                            YY[j]), arr.ind = TRUE)), (k)]
                          MAT[(which((as.vector(rownames(MAT)) == 
                            xx), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                            YY[j]), arr.ind = TRUE)), (k)] <- tmp + 
                            as.numeric(as.vector(rrel[, (k + 
                              2L)][which(rrel[, 1] == attr(nX, 
                              "names"))]))[j]
                        }
                        rm(j)
                      }
                      else if (isTRUE(length(YY) == 1L) == TRUE) {
                        tmp <- MAT[(which((as.vector(rownames(MAT)) == 
                          xx), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                          YY), arr.ind = TRUE)), (k)]
                        MAT[(which((as.vector(rownames(MAT)) == 
                          xx), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                          YY), arr.ind = TRUE)), (k)] <- tmp + 
                          as.numeric(as.vector(rrel[, (k + 2L)][which(rrel[, 
                            1] == attr(nX, "names"))]))
                      }
                      rm(xx, YY)
                    }
                  }
                  else if (isTRUE(length(rel) == 1L) == TRUE) {
                    tmp <- MAT[which(rownames(MAT) == x[rel, 
                      1]), which(colnames(MAT) == x[rel, 2]), 
                      (k)]
                    MAT[which(rownames(MAT) == x[rel, 1]), which(colnames(MAT) == 
                      x[rel, 2]), (k)] <- tmp + as.numeric(rel)
                  }
                }
                rm(k)
            }
            else if (isTRUE(ncol(x) == 3L) == TRUE) {
                rel <- which(x[, 3] != 0L)
                rrel <- x[rel, ]
                X <- integer(n)
                for (i in seq_len(n)) {
                  X[i] <- sum(as.numeric(rrel[, 1] == lbs[i]))
                }
                rm(i)
                attr(X, "names") <- lbs
                xx <- vector()
                for (i in seq_len(n)) {
                  ifelse(X[i] != 0L, xx[i] <- i, xx[i] <- NA)
                }
                rm(i)
                attr(xx, "names") <- lbs
                xx <- (stats::na.omit(xx))
                xx <- as.vector(attr(xx, "names"))
                nX <- X[which(X > 0L)]
                if (isTRUE(length(xx) > 1L) == TRUE) {
                  YY <- vector()
                  for (i in seq_len(length(xx))) {
                    kual0 <- which(rrel[, 1] == attr(nX, "names")[i])
                    kual1 <- which((as.vector(rownames(MAT)) == 
                      xx[i]), arr.ind = TRUE)
                    YY <- rrel[, 2][kual0]
                    if (isTRUE(length(YY) > 1L) == TRUE) {
                      for (j in seq_len(length(YY))) {
                        kual2 <- which(as.vector(colnames(MAT) == 
                          YY[j]), arr.ind = TRUE)
                        tmp <- MAT[kual1, kual2]
                        MAT[kual1, kual2] <- tmp + as.numeric(as.vector(factor(rrel[, 
                          3][kual0])))[j]
                      }
                      rm(j)
                    }
                    else if (isTRUE(length(YY) == 1L) == TRUE) {
                      kual3 <- which(as.vector(colnames(MAT) == 
                        YY), arr.ind = TRUE)
                      tmp <- MAT[kual1, kual3]
                      MAT[kual1, kual3] <- tmp + as.numeric(as.vector(factor(rrel[, 
                        3][kual0])))
                    }
                  }
                  rm(i)
                }
                else if (isTRUE(length(xx) == 1L) == TRUE) {
                  YY <- rrel[, 2][which(rrel[, 1] == attr(nX, 
                    "names"))]
                  if (isTRUE(length(YY) > 1L) == TRUE) {
                    kual4 <- which((as.vector(rownames(MAT)) == 
                      xx), arr.ind = TRUE)
                    kual5 <- which(rrel[, 1] == attr(nX, "names"))
                    for (j in seq_len(length(YY))) {
                      kual2 <- (which(as.vector(colnames(MAT) == 
                        YY[j]), arr.ind = TRUE))
                      MAT[kual4, kual2] <- as.numeric(as.vector(rrel[, 
                        3][kual5]))[j]
                    }
                    rm(j)
                  }
                  else if (isTRUE(length(YY) == 1L) == TRUE) {
                    MAT[which(dimnames(MAT)[1][[1]] == YY), which(dimnames(MAT)[2][[1]] == 
                      YY)] <- as.numeric(as.vector(rrel[, 3]))
                  }
                  rm(xx, YY)
                }
            }
        }
        if (isTRUE(valued == FALSE) == TRUE) {
            MAT <- dichot(MAT, c = 1)
        }
        if (isTRUE(inc == FALSE) == TRUE && isTRUE(r == 1L) == 
            TRUE) {
            return(MAT[, sort(colnames(MAT))][sort(rownames(MAT)), 
                ])
        }
        else {
            return(MAT)
        }
    }
    else if (isTRUE(toarray == FALSE) == TRUE) {
        if (is.array(x) == TRUE) {
            if (is.na(dim(x)[3]) == TRUE) {
                if (identical(dimnames(x)[[1]], dimnames(x)[[2]]) == 
                  TRUE) {
                  tmp <- trnf(x, tolist = TRUE, lb2lb = TRUE, 
                    lbs = dimnames(x)[[1]], sep = sep)
                }
                else {
                  if (isTRUE(ncol(x) > nrow(x)) == TRUE) {
                    x0 <- x
                    colnames(x0) <- seq_len(ncol(x0))
                    x0 <- rbind(x0, matrix(0, nrow = ncol(x) - 
                      nrow(x), ncol = ncol(x)))
                  }
                  else if (isTRUE(ncol(x) < nrow(x)) == TRUE) {
                    x0 <- x
                    rownames(x0) <- seq_len(nrow(x0))
                    x0 <- cbind(x0, matrix(0, nrow = nrow(x), 
                      ncol = nrow(x) - ncol(x)))
                  }
                  tmp <- trnf(x0, tolist = TRUE, lb2lb = FALSE, 
                    sep = sep)
                }
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
                edgl <- data.frame(matrix(nrow = 0, ncol = (dim(x)[3]) + 
                  2L))
                colnames(edgl) <- c("s", "r", dimnames(x)[[3]])
                lb <- trnf(x[, , 1], tolist = TRUE, lb2lb = TRUE, 
                  lbs = dimnames(x)[[1]], sep = sep)
                nd <- trnf(x[, , 1], tolist = TRUE, lb2lb = FALSE, 
                  sep = sep)
                for (l in seq_len(length(lb))) {
                  edgl[nrow(edgl) + 1L, 3] <- 1L
                  edgl[nrow(edgl), 1:2] <- strsplit(lb[l], sep)[[1]]
                }
                rm(l)
                lb1 <- Map(as.vector, data.frame(t(edgl[, 1:2])))
                for (k in 2:dim(x)[3]) {
                  lbk <- trnf(x[, , k], tolist = TRUE, lb2lb = TRUE, 
                    lbs = dimnames(x)[[1]], sep = sep)
                  ndk <- trnf(x[, , k], tolist = TRUE, lb2lb = FALSE, 
                    sep = sep)
                  for (j in seq_len(length(ndk))) {
                    lbe <- strsplit(lbk[[j]], sep)[[1]]
                    if (any(unlist(lapply(lapply(jnt(lb1, uniq = TRUE), 
                      function(z) {
                        jnt(lbe, uniq = TRUE) %in% z
                      }), all), use.names = FALSE)) == TRUE) {
                      edgl[which(unlist(lapply(lapply(jnt(lb1, 
                        uniq = TRUE), function(z) {
                        jnt(lbe, uniq = TRUE) %in% z
                      }), all), use.names = FALSE)), (k + 2L)] <- 1L
                    }
                    else {
                      edgl[nrow(edgl) + 1L, (k + 2L)] <- 1L
                      edgl[nrow(edgl), 1:2] <- lbe
                    }
                    lb1 <- Map(as.vector, data.frame(t(edgl[, 
                      1:2])))
                  }
                  rm(j)
                }
                rm(k)
                edgl[sapply(edgl, is.na)] <- 0
            }
            return(edgl)
        }
        else {
            return(x)
        }
    }
}

