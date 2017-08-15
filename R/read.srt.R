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
                x[, 1:ncol(x)])))
        attr(x, "names")[1:2] <- c("n", "n")
    }
    else {
        NA
    }
    if (is.null(add) == FALSE) {
        levels(x[, 1]) <- c(levels(x[, 1]), add)
        levels(x[, 2]) <- c(levels(x[, 2]), add)
        for (i in 1:length(add)) {
            x <- rbind(x, c(add[i], add[i], NA))
        }
        rm(i)
    }
    if (isTRUE(toarray == TRUE) == TRUE) {
        R <- (ncol(x) - 2L)
        if (R == 0L) 
            stop("You must specify at least one relation.")
        lbs <- unique(c(as.vector(x[, 1]), as.vector(x[, 2])))
        if (isTRUE(lbs == "") == TRUE) {
            warning("Node labels in the input are empty!")
            lbs <- 1:nrow(x)
        }
        else {
            NA
        }
        n <- length(lbs)
        ifelse(isTRUE(R == 1L) == TRUE, MAT <- array(0L, dim = c(n, 
            n)), MAT <- array(0L, dim = c(n, n, R)))
        dimnames(MAT)[[1]] <- dimnames(MAT)[[2]] <- lbs
        ifelse(isTRUE(R == 1L) == FALSE, dimnames(MAT)[[3]] <- attr(x, 
            "names")[3:ncol(x)], NA)
        if (isTRUE(ncol(x) > 3L) == TRUE) {
            for (k in 1:R) {
                rel <- which(x[, (k + 2L)] != 0L)
                rrel <- x[rel, ]
                X <- n
                for (i in 1:n) {
                  X[i] <- sum(as.numeric(rrel[, 1] == lbs[i]))
                }
                rm(i)
                attr(X, "names") <- lbs
                xx <- vector()
                for (i in 1:n) {
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
                      for (j in 1:length(YY)) {
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
                    for (j in 1:length(YY)) {
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
            for (i in 1:n) {
                X[i] <- sum(as.numeric(rrel[, 1] == lbs[i]))
            }
            rm(i)
            attr(X, "names") <- lbs
            xx <- vector()
            for (i in 1:n) {
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
                    for (j in 1:length(YY)) {
                      tmp <- MAT[(which((as.vector(rownames(MAT)) == 
                        xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                        YY[j]), arr.ind = TRUE))]
                      MAT[(which((as.vector(rownames(MAT)) == 
                        xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                        YY[j]), arr.ind = TRUE))] <- tmp + as.numeric(as.vector(factor(rrel[, 
                        3][which(rrel[, 1] == attr(nX, "names")[i])])))[j]
                    }
                    rm(j)
                  }
                  else if (isTRUE(length(YY) == 1L) == TRUE) {
                    tmp <- MAT[(which((as.vector(rownames(MAT)) == 
                      xx[i]), arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                      YY), arr.ind = TRUE))]
                    MAT[(which((as.vector(rownames(MAT)) == xx[i]), 
                      arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                      YY), arr.ind = TRUE))] <- tmp + as.numeric(as.vector(factor(rrel[, 
                      3][which(rrel[, 1] == attr(nX, "names")[i])])))
                  }
                }
                rm(i)
            }
            else if (isTRUE(length(xx) == 1L) == TRUE) {
                YY <- rrel[, 2][which(rrel[, 1] == attr(nX, "names"))]
                if (isTRUE(length(YY) > 1L) == TRUE) {
                  for (j in 1:length(YY)) {
                    MAT[(which((as.vector(rownames(MAT)) == xx), 
                      arr.ind = TRUE)), (which(as.vector(colnames(MAT) == 
                      YY[j]), arr.ind = TRUE)), 1] <- as.numeric(as.vector(rrel[, 
                      3][which(rrel[, 1] == attr(nX, "names"))]))[j]
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
        return(MAT)
    }
    else if (isTRUE(toarray == FALSE) == TRUE) {
        if (isTRUE(attr == TRUE) == TRUE) {
            if (isTRUE(dichot == TRUE) == TRUE) {
                ifelse(isTRUE(rownames == TRUE) == TRUE, xa[, 
                  1:ncol(xa)] <- dichot(xa[, 1:ncol(xa)]), x[, 
                  3:ncol(x)] <- dichot(x[, 3:ncol(x)]))
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
