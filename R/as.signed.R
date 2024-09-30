as.signed <-
function (x, lbs) 
{
    if (isTRUE(attr(x, "class")[1] == "Rel.Q") == TRUE) {
        x <- x$Q
    }
    else if (is.array(x) == FALSE) {
        stop("Data must be an array")
    }
    else {
        NA
    }
    if (is.na(dim(x)[3]) == FALSE) {
        sm <- x[, , 1]
        warning("Take the 1st dim. in 'x' only.")
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
