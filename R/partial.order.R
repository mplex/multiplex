## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
partial.order <-
function (x, type = c("strings", "galois", "pi.rels"), lbs, sel, 
    po.incl, valued) 
{
    if (match.arg(type) == "strings") {
        if (isTRUE(attr(x, "class")[1] == "Strings") == TRUE) {
            if (missing(valued) == FALSE && isTRUE(valued == 
                FALSE) == TRUE) {
                po <- strng(strings(dichot(x$wt, c = 1))$wt)
            }
            else {
                po <- strng(x$wt)
            }
        }
        else if (isTRUE(attr(x, "class")[1] == "Strings") == 
            FALSE) {
            stop(paste(sQuote("x"), " is not a ", dQuote("Strings"), 
                " class object.", sep = ""))
        }
    }
    if (match.arg(type) == "galois") {
        if (isTRUE(attr(x$gc, "class")[1] == "Galois") == TRUE) {
            ifelse(isTRUE(x$sep == ", ") == TRUE, sep2 <- "} {", 
                sep2 <- "}{")
            if (isTRUE(attr(x$gc, "class")[2] == "full") == TRUE) {
                po <- matrix(0L, nrow = length(x$gc), ncol = length(x$gc))
                for (j in seq_along(x$gc)) {
                  for (i in seq_along(x$gc)) {
                    ifelse(isTRUE(all(dhc(x$gc[[i]], sep = x$sep) %in% 
                      dhc(x$gc[[j]], sep = x$sep))) == TRUE, 
                      po[i, j] <- 1L, NA)
                  }
                }
                rm(i, j)
                lb <- list()
                length(lb) <- length(x$gc)
                for (i in seq_along(x$gc)) {
                  if (isTRUE(is.na(attr(x$gc, "names")[i])) == 
                    FALSE) {
                    lb[[i]] <- paste(paste("{", attr(x$gc, "names")[i], 
                      sep = ""), paste(x$gc[[i]], "}", sep = ""), 
                      sep = sep2)
                  }
                  else {
                    lb[[i]] <- paste(paste("{", x$gc[[i]], sep = ""), 
                      paste(" ", "}", sep = ""), sep = sep2)
                  }
                }
                rm(i)
                colnames(po) <- rownames(po) <- lb
            }
            else if (isTRUE(attr(x$gc, "class")[2] == "reduced") == 
                TRUE) {
                po <- matrix(0L, nrow = length(x$gc$reduc), ncol = length(x$gc$reduc))
                for (j in seq_along(x$gc$reduc)) {
                  for (i in seq_along(x$gc$reduc)) {
                    ifelse(isTRUE(all(dhc(x$gc$full[[i]], sep = x$sep) %in% 
                      dhc(x$gc$full[[j]], sep = x$sep))) == TRUE, 
                      po[i, j] <- 1L, NA)
                  }
                }
                rm(i, j)
                lb <- list()
                length(lb) <- length(x$gc$reduc)
                for (i in seq_along(x$gc$reduc)) {
                  if (isTRUE(is.na(attr(x$gc$reduc, "names")[i])) == 
                    FALSE) {
                    lb[[i]] <- paste(paste("{", attr(x$gc$reduc, 
                      "names")[i], sep = ""), paste(x$gc$reduc[[i]], 
                      "}", sep = ""), sep = sep2)
                  }
                  else {
                    lb[[i]] <- paste(paste("{", " ", sep = ""), 
                      paste(x$gc$reduc[[i]], "}", sep = ""), 
                      sep = sep2)
                  }
                }
                rm(i)
                colnames(po) <- rownames(po) <- lb
            }
            ifelse(isTRUE(x$sep == ", ") == TRUE, cp <- which(dimnames(po)[[1]] == 
                "{} {}"), cp <- which(dimnames(po)[[1]] == "{}{}"))
            dimnames(po)[[1]][cp] <- dimnames(po)[[2]][cp] <- cp
        }
        else if (isTRUE(attr(x$gc, "class")[1] == "Galois") == 
            FALSE) {
            stop(paste(sQuote("x"), " is not a ", dQuote("Galois"), 
                " class object.", sep = ""))
        }
    }
    if (match.arg(type) == "pi.rels") {
        if (isTRUE("Pi.rels" %in% attr(x, "class")) == TRUE) {
            if (missing(sel) == FALSE && isTRUE(is.vector(sel) == 
                TRUE) == TRUE) {
                if (isTRUE(sel <= 0) == TRUE) {
                  stop(paste("Value of ", sQuote("sel"), " should be a positive integer.", 
                    sep = ""))
                }
                else {
                  NA
                }
            }
            else {
                if (missing(sel) == FALSE && is.null(sel) == 
                  TRUE) {
                  stop(paste(sQuote("sel"), " is NULL.", sep = ""))
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
            stop(paste(sQuote("x"), " is not a ", dQuote("Pi.rels"), 
                " class object.", sep = ""))
        }
    }
    ifelse(missing(lbs) == FALSE && isTRUE(length(lbs) == dim(po)[1]) == 
        TRUE, dimnames(po)[[2]] <- dimnames(po)[[1]] <- lbs, 
        NA)
    if (match.arg(type) == "galois") {
        class(po) <- c("Partial.Order", match.arg(type), x$sep)
    }
    else if (match.arg(type) == "pi.rels") {
        class(po) <- c("Partial.Order", "Pi.rels")
    }
    else {
        class(po) <- c("Partial.Order", match.arg(type))
    }
    po
}

