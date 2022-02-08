read.srt <-
function (file, header = TRUE, sep = "\t", toarray = TRUE, dichot = FALSE, 
    attr = FALSE, rownames = FALSE, add = NULL) 
{
    ifelse(is.array(file) == TRUE | is.data.frame(file) == TRUE, 
        x <- file, x <- utils::read.table(file, header = header, 
            sep = sep))
    if (isTRUE(attr == TRUE) == TRUE) {
        xa <- x
        ifelse(isTRUE(rownames == FALSE) == TRUE, x <- as.data.frame(cbind(as.vector(x[, 
            1]), as.vector(x[, 1]), as.vector(x[, 2:ncol(x)]))), 
            x <- as.data.frame(cbind(rownames(x), rownames(x), 
                x[, seq_len(ncol(x))])))
        attr(x, "names")[1:2] <- c("n", "n")
    }
    else {
        NA
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
        if (isTRUE((ncol(x) - 2L) == 0L) == TRUE) {
            warning("One type of relation assumed.")
            x <- cbind(x, t = rep(1L, nrow(x)))
        }
        r <- (ncol(x) - 2L)
        x <- x[complete.cases(x[, seq_len(2)]), ]
        lbs <- unique(c(as.vector(x[, 1]), as.vector(x[, 2])))
        if (isTRUE(lbs == "") == TRUE) {
            warning("Node labels in the input are empty!")
            lbs <- seq_len(nrow(x))
        }
        else {
            NA
        }
        n <- length(lbs)
        ifelse(isTRUE(r == 1L) == TRUE, MAT <- array(0L, dim = c(n, 
            n)), MAT <- array(0L, dim = c(n, n, r)))
        dimnames(MAT)[[1]] <- dimnames(MAT)[[2]] <- lbs
        ifelse(isTRUE(r > 1L) == TRUE, dimnames(MAT)[[3]] <- attr(x, 
            "names")[3:ncol(x)], NA)
        if (isTRUE(ncol(x) > 3L) == TRUE) {
            for (k in seq_len(r)) {
                rel <- which(x[, (k + 2L)] != 0L)
                rrel <- x[rel, ]
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
                  for (i in 1:length(xx)) {
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
                          as.numeric(as.vector(rrel[, (k + 2L)][which(rrel[, 
                            1] == attr(nX, "names")[i])]))[j]
                      }
                      rm(j)
                    }
                    else if (isTRUE(length(YY) == 1L) == TRUE) {
                      tmp <- MAT[(which((as.vector(rownames(MAT)) == 
                        xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                        YY), arr.ind = TRUE)), (k)]
                      MAT[(which((as.vector(rownames(MAT)) == 
                        xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                        YY), arr.ind = TRUE)), (k)] <- tmp + 
                        as.numeric(as.vector(rrel[, (k + 2L)][which(rrel[, 
                          1] == attr(nX, "names")[i])]))
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
                      MAT[(which((as.vector(rownames(MAT)) == 
                        xx), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                        YY[j]), arr.ind = TRUE)), (k)] <- as.numeric(as.vector(rrel[, 
                        (k + 2L)][which(rrel[, 1] == attr(nX, 
                        "names"))]))[j]
                    }
                    rm(j)
                  }
                  else if (isTRUE(length(YY) == 1L) == TRUE) {
                    MAT[(which((as.vector(rownames(MAT)) == xx), 
                      arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                      YY), arr.ind = TRUE)), (k)] <- as.numeric(as.vector(rrel[, 
                      (k + 2L)][which(rrel[, 1] == attr(nX, "names"))]))
                  }
                  rm(xx, YY)
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
                YY <- rrel[, 2][which(rrel[, 1] == attr(nX, "names"))]
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
        if (isTRUE(dichot == TRUE) == TRUE) {
            MAT <- dichot(MAT)
        }
        ifelse(isTRUE(attr == FALSE) == TRUE, return(MAT[, sort(colnames(MAT))][sort(rownames(MAT)), 
            ]), return(MAT))
    }
    else if (isTRUE(toarray == FALSE) == TRUE) {
        if (isTRUE(attr == TRUE) == TRUE) {
            if (isTRUE(dichot == TRUE) == TRUE) {
                ifelse(isTRUE(rownames == TRUE) == TRUE, xa[, 
                  seq_len(ncol(xa))] <- dichot(xa[, seq_len(ncol(xa))]), 
                  x[, 3:ncol(x)] <- dichot(x[, 3:ncol(x)]))
            }
            if (isTRUE(rownames == TRUE) == TRUE) {
                rownames(xa) <- xa[, 1]
                xa <- xa[, 2:ncol(xa)]
            }
            else {
                NA
            }
            return(xa)
        }
        else {
            return(x)
        }
    }
}
