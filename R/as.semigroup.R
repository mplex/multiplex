as.semigroup <-
function (x, lbs) 
{
    if (is.array(x) == FALSE && is.data.frame(x) == FALSE) 
        stop("Data must be a square matrix or data frame")
    S <- as.data.frame(x)
    ifelse(suppressWarnings(NA %in% (as.numeric(attr(x, "names")))) == 
        FALSE, Lbs <- seq_len(nrow(S)), Lbs <- rownames(S))
    ifelse(missing(lbs) == TRUE, lbs <- Lbs, NA)
    lst <- list(ord = nrow(S), st = lbs, S = S)
    ifelse(suppressWarnings(NA %in% (as.numeric(attr(x, "names")))) == 
        TRUE, class(lst) <- c("Semigroup", "symbolic"), class(lst) <- c("Semigroup", 
        "numerical"))
    return(lst)
}
