partial.order <-
function (x, type = c("strings", "galois"), lbs, labels) 
{
    if (match.arg(type) == "strings") {
        if (isTRUE(attr(x, "class") == "Strings") == TRUE) {
            x <- x$wt
            if (is.array(x) == FALSE) 
                stop("Data must be a stacked array of square matrices if a product of 'strings'.")
            if (is.na(dim(x)[3]) == FALSE) {
                tmp <- data.frame(matrix(ncol = (dim(x)[1] * 
                  dim(x)[2]), nrow = 0))
                for (i in seq_len(dim(x)[3])) {
                  tmp[i, ] <- as.vector(x[, , i])
                }
                rm(i)
                po <- as.matrix(array(0L, dim = c(dim(x)[3], 
                  dim(x)[3])))
                for (j in seq_len(dim(x)[3])) {
                  for (i in seq_len(dim(x)[3])) {
                    if ((as.numeric(any(tmp[i, ] < tmp[j, ])) == 
                      1 && as.numeric(any(tmp[j, ] < tmp[i, ])) == 
                      0) | as.numeric(all(tmp[i, ] == tmp[j, 
                      ])) == 1) 
                      po[i, j] <- 1L
                  }
                }
                rm(i, j)
                rownames(po) <- colnames(po) <- dimnames(x)[[3]]
            }
            else if (is.na(dim(x)[3]) == TRUE) {
                po <- 1L
            }
        }
        else if (isTRUE(attr(x, "class") == "Strings") == FALSE) {
            stop("\"x\" should be an object of a \"Strings\" class.")
        }
    }
    if (match.arg(type) == "galois") {
        if (isTRUE(attr(x, "class")[1] == "Galois") == TRUE) {
            if (isTRUE(attr(x, "class")[2] == "full") == TRUE) {
                po <- matrix(0L, nrow = length(x), ncol = length(x))
                for (j in seq_len(length(x))) {
                  for (i in seq_len(length(x))) {
                    ifelse(isTRUE(all(dhc(x[[i]], sep = ", ") %in% 
                      dhc(x[[j]], sep = ", "))) == TRUE, po[i, 
                      j] <- 1L, NA)
                  }
                }
                rm(i, j)
            }
            else if (isTRUE(attr(x, "class")[2] == "reduced") == 
                TRUE) {
                po <- matrix(0L, nrow = length(x$full), ncol = length(x$full))
                for (j in seq_len(length(x$full))) {
                  for (i in seq_len(length(x$full))) {
                    ifelse(isTRUE(all(dhc(x$full[[i]], sep = ", ") %in% 
                      dhc(x$full[[j]], sep = ", "))) == TRUE, 
                      po[i, j] <- 1L, NA)
                  }
                }
                rm(i, j)
            }
            lb <- list()
            if (isTRUE(attr(x, "class")[2] == "full") == TRUE) {
                length(lb) <- length(x)
                for (i in seq_len(length(x))) {
                  if (isTRUE(is.na(attr(x, "names")[i])) == FALSE) {
                    lb[[i]] <- paste(paste("{", x[[i]], sep = ""), 
                      paste(attr(x, "names")[i], "}", sep = ""), 
                      sep = "} {")
                  }
                  else {
                    lb[[i]] <- paste(paste("{", x[[i]], sep = ""), 
                      paste(" ", "}", sep = ""), sep = "} {")
                  }
                }
                rm(i)
                colnames(po) <- rownames(po) <- lb
            }
            else if (isTRUE(attr(x, "class")[2] == "reduced") == 
                TRUE) {
                length(lb) <- length(x$reduc)
                for (i in seq_len(length(x$reduc))) {
                  if (isTRUE(is.na(attr(x$reduc, "names")[i])) == 
                    FALSE) {
                    lb[[i]] <- paste(paste("{", attr(x$reduc, 
                      "names")[i], sep = ""), paste(x$reduc[[i]], 
                      "}", sep = ""), sep = "} {")
                  }
                  else {
                    lb[[i]] <- paste(paste("{", " ", sep = ""), 
                      paste(x$reduc[[i]], "}", sep = ""), sep = "} {")
                  }
                }
                rm(i)
                colnames(po) <- rownames(po) <- lb
            }
            cp <- which(dimnames(po)[[1]] == "{} {}")
            dimnames(po)[[1]][cp] <- dimnames(po)[[2]][cp] <- cp
        }
        else if (isTRUE(attr(x, "class")[1] == "Galois") == FALSE) {
            stop("\"x\" should be an object of a \"Galois\" class.")
        }
    }
    if (missing(lbs) == FALSE && isTRUE(length(lbs) == dim(po)[1]) == 
        TRUE) {
        dimnames(po)[[2]] <- dimnames(po)[[1]] <- lbs
    }
    else if (missing(labels) == FALSE && isTRUE(length(labels) == 
        dim(po)[1]) == TRUE) {
        dimnames(po)[[2]] <- dimnames(po)[[1]] <- labels
    }
    else {
        NA
    }
    class(po) <- c("Partial.Order", match.arg(type))
    po
}
