fltr <-
function (x, PO, rclos = TRUE, ideal = FALSE) 
{
    if (is.null(dimnames(PO)[[1]]) == TRUE) 
        stop("Dimnames in 'PO' are NULL")
    if (isTRUE(is.character(x) == TRUE) == TRUE) {
        lbs <- dimnames(PO)[[1]]
        tmp <- jnt(unlist(strsplit(lbs, "} {", fixed = TRUE)), 
            sep = ", ")
        tmp <- sub("{", "", dhc(tmp, sep = ", "), fixed = TRUE)
        tmp <- sub("}", "", dhc(tmp, sep = ", "), fixed = TRUE)
        if (isTRUE(length(tmp) != length(unique(tmp))) == TRUE) 
            stop("'PO' must be in a reduced form.")
        ifelse(all(x %in% tmp) == FALSE, x <- x[which(x %in% 
            tmp)], NA)
        X <- vector()
        for (k in 1:length(x)) {
            for (i in 1:length(lbs)) {
                tmplb <- jnt(unlist(strsplit(lbs[i], "} {", fixed = TRUE)), 
                  sep = ", ")
                tmplb <- sub("{", "", dhc(tmplb, sep = ", "), 
                  fixed = TRUE)
                tmplb <- sub("}", "", dhc(tmplb, sep = ", "), 
                  fixed = TRUE)
                if (isTRUE(x[k] %in% tmplb) == TRUE) {
                  X <- append(X, i)
                  break
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        rm(k)
        if (isTRUE(length(X) == 0) == TRUE) 
            stop("'x' is not part of the given partial order.")
    }
    else {
        if (isTRUE(max(x) > nrow(PO) | min(x) <= 0L) == TRUE) 
            stop("Element in 'x' is either non-positive or a number greater than the size of the partial order.")
        X <- as.integer(x)
    }
    ifelse(isTRUE(ideal == TRUE) == TRUE, po <- t(PO), po <- PO)
    pfl <- vector()
    for (i in 1:nrow(po)) {
        for (k in 1:length(X)) {
            ifelse(isTRUE(po[X[k], i] == 1L) == TRUE && isTRUE(po[i, 
                X[k]] == 0L) == TRUE, pfl <- append(pfl, i), 
                NA)
        }
        rm(k)
    }
    rm(i)
    if (rclos) {
        pfl <- append(X, pfl)
    }
    if (isTRUE(length(pfl) > 0L) == TRUE) {
        pfl <- unique(pfl)
        pfll <- as.list(dimnames(PO)[[1]][pfl])
        attr(pfll, "names") <- pfl
    }
    else {
        pfll <- NULL
    }
    return(pfll)
}
