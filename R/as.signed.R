## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
as.signed <-
function (x, lbs) 
{
    if (isTRUE("Rel.Q" %in% attr(x, "class")) == TRUE) {
        x <- x$Q
    }
    else if (is.array(x) == FALSE) {
        stop("Data must be an array.")
    }
    else {
        NA
    }
    if (is.na(dim(x)[3]) == FALSE) {
        sm <- x[, , 1]
        message(paste("Take first dimension in ", sQuote("x"), 
            " only.", sep = ""))
    }
    else {
        sm <- x
    }
    if (missing(lbs) == FALSE && isTRUE(length(lbs) == dim(sm)[1]) == 
        TRUE) {
        rownames(sm) <- colnames(sm) <- lbs
    }
    else {
        ifelse(isTRUE(dimnames(x)[[1]]) == TRUE, rownames(sm) <- colnames(sm) <- seq_len(dim(sm)[1]), 
            rownames(sm) <- colnames(sm) <- dimnames(x)[[1]])
    }
    val <- levels(factor(sm))
    lst <- list(val = noquote(levels(stats::reorder(val, length(val):1))), 
        s = sm)
    class(lst) <- "Signed"
    return(lst)
}

