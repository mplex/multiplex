relabel <-
function (S, lbs = NULL) 
{
    if (isTRUE(attr(S, "class")[1] == "Semigroup") == FALSE) {
        stop("\"S\" should be an object of a \"Semigroup\" class.")
    }
    ifelse(isTRUE(is.null(lbs) == TRUE) == TRUE, lbs <- S$st, 
        NA)
    lb <- S$st
    S$st <- lbs
    x <- S$S
    ifelse(isTRUE(attr(S, "class")[2] == "symbolic") == TRUE, 
        rownames(x) <- colnames(x) <- lbs, NA)
    for (i in which(lbs != lb)) {
        x <- replace(x, S$S == as.character(lb[i]), as.character(lbs[i]))
    }
    rm(i)
    S$S <- as.data.frame(x)
    rm(x, lb)
    return(S)
}
