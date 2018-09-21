partial.order <-
function (x, type = c("strings", "galois", "pi.rels"), lbs, sel, 
    po.incl) 
{
    if (match.arg(type) == "strings") {
        if (isTRUE(attr(x, "class") == "Strings") == TRUE) {
            X <- x$wt
            po <- strng(X)
        }
        else if (isTRUE(attr(x, "class") == "Strings") == FALSE) {
            stop("\"x\" should be an object of a \"Strings\" class.")
        }
    }
    if (match.arg(type) == "galois") {
        if (isTRUE(attr(x, "class")[1] == "Galois") == TRUE) {
            if (isTRUE(attr(x, "class")[2] == "full") == TRUE) {
                po <- matrix(0L, nrow = length(x), ncol = length(x))
                for (j in seq_along(x)) {
                  for (i in seq_along(x)) {
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
                for (j in seq_along(x$full)) {
                  for (i in seq_along(x$full)) {
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
                for (i in seq_along(x)) {
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
                for (i in seq_along(x$reduc)) {
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
    if (match.arg(type) == "pi.rels") {
        if (isTRUE(attr(x, "class")[1] == "Pi.rels") == TRUE) {
            if (missing(sel) == FALSE && isTRUE(is.vector(sel) == 
                TRUE) == TRUE) {
                if (isTRUE(sel <= 0) == TRUE) {
                  stop("Value of \"sel\" must be positive integer.")
                }
                else {
                  NA
                }
            }
            else {
                if (missing(sel) == FALSE && is.null(sel) == 
                  TRUE) {
                  stop("Selection in 'sel' is NULL.")
                }
                else {
                  sel <- FALSE
                }
            }
            if (isTRUE(sel == FALSE) == TRUE) {
                xpi <- x$pi
            }
            else {
                xpi <- x$pi[, , unique(sel)]
            }
            if (missing(po.incl) == FALSE && isTRUE(po.incl == 
                TRUE) == TRUE) {
                ifelse(is.null(x$po) == TRUE, po <- strng(xpi), 
                  po <- strng(suppressWarnings(zbind(xpi, x$po))))
                dimnames(po)[[2]][dim(po)[2]] <- dimnames(po)[[1]][dim(po)[1]] <- "PO"
            }
            else {
                po <- strng(xpi)
            }
        }
        else if (isTRUE(attr(x, "class")[1] == "Pi.rels") == 
            FALSE) {
            stop("\"x\" should be an object of a \"Pi.rels\" class.")
        }
    }
    ifelse(missing(lbs) == FALSE && isTRUE(length(lbs) == dim(po)[1]) == 
        TRUE, dimnames(po)[[2]] <- dimnames(po)[[1]] <- lbs, 
        NA)
    class(po) <- c("Partial.Order", match.arg(type))
    po
}
