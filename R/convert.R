convert <-
function (x, lbs = NULL, SemigroupClass = FALSE) 
{
    if (isTRUE(attr(x, "class")[1] == "Semigroup") == FALSE) 
        stop("\"x\" should be an object of a \"Semigroup\" class.")
    S <- x$S
    if (isTRUE(attr(x, "class")[2] == "numerical") == TRUE) {
        if (is.null(lbs) == TRUE) {
            ifelse(is.null(x$st) == TRUE, lbs <- LETTERS[seq_len(x$ord)], 
                lbs <- x$st)
        }
        else {
            lbs <- as.vector(lbs)
        }
    }
    else if (isTRUE(attr(x, "class")[2] == "symbolic") == TRUE) {
        if (is.null(lbs) == FALSE) 
            warning("Semigroup is in 'symbolic' form and 'lbs' are therefore ignored.")
        ifelse(is.null(x$st) == TRUE, lbs <- LETTERS[seq_len(x$ord)], 
            lbs <- x$st)
    }
    s <- vector()
    for (i in seq_len(length(as.matrix(S)))) {
        if (isTRUE(attr(x, "class")[2] == "symbolic") == TRUE) {
            s[i] <- which(lbs == as.matrix(S)[i])
        }
        else if (isTRUE(attr(x, "class")[2] == "numerical") == 
            TRUE) {
            s[i] <- lbs[which(dimnames(S)[[1]] == as.matrix(S)[i])]
        }
    }
    s <- matrix(s, nrow = nrow(S), ncol = ncol(S))
    if (isTRUE(attr(x, "class")[2] == "numerical") == TRUE) {
        dimnames(s)[[1]] <- dimnames(s)[[2]] <- as.list(lbs)
    }
    else if (isTRUE(attr(x, "class")[2] == "symbolic") == TRUE) {
        dimnames(s)[[1]] <- dimnames(s)[[2]] <- as.list(seq_len(x$ord))
    }
    if (SemigroupClass) {
        lst <- list(ord = nrow(s), st = dimnames(s)[[1]], S = as.data.frame(s))
        ifelse(isTRUE(attr(x, "class")[2] == "numerical") == 
            TRUE, class(lst) <- c("Semigroup", "symbolic"), class(lst) <- c("Semigroup", 
            "numerical"))
        return(lst)
    }
    else {
        return(as.data.frame(s))
    }
}
