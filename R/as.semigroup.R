as.semigroup <-
function (x, gens = NA, lbs) 
{
    if (isTRUE("Semigroup" %in% attr(x, "class")) == TRUE) {
        S <- as.matrix(x$S)
    }
    else {
        if (is.array(x) == FALSE && is.data.frame(x) == FALSE) 
            stop("Data must be a square matrix or data frame")
        S <- as.matrix(x)
    }
    if (missing(lbs) == TRUE) {
        NA
    }
    else if (missing(lbs) == FALSE && isTRUE(all(lbs %in% S) == 
        FALSE) == TRUE) {
        z <- vector()
        for (i in seq_len(length(as.matrix(S)))) {
            z[i] <- lbs[which(dimnames(S)[[1]] == as.matrix(S)[i])]
        }
        rm(i)
        z <- matrix(z, nrow = nrow(S), ncol = ncol(S))
        dimnames(z)[[1]] <- dimnames(z)[[2]] <- as.list(lbs)
        S <- z
    }
    ifelse(is.numeric(S) == TRUE, lbs <- seq_len(nrow(S)), lbs <- rownames(S))
    lst <- list(gens = gens, ord = nrow(S), st = lbs, S = as.data.frame(S))
    ifelse(is.character(lst$st) == TRUE, class(lst) <- c("Semigroup", 
        "symbolic"), class(lst) <- c("Semigroup", "numerical"))
    return(lst)
}
