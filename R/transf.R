transf <-
function (x, type = c("tolist", "toarray"), lb2lb = FALSE, labels = NULL, 
    ord, prsep) 
{
    ifelse(missing(prsep) == TRUE, prsep <- ", ", NA)
    if (match.arg(type) == "tolist") {
        if (isTRUE(is.character(x) == TRUE) == TRUE) 
            return(x)
        if (is.na(dim(x)[3]) == FALSE) 
            stop("Use the \"rel.sys\" function for 3D arrays.")
        if (isTRUE(is.matrix(x) == TRUE) == FALSE) 
            x <- as.matrix(x)
        if (isTRUE(lb2lb == TRUE) == TRUE) {
            if (isTRUE(is.null(labels) == TRUE) == TRUE) {
                if (isTRUE(is.null(dimnames(x)[[1]]) == TRUE | 
                  is.null(dimnames(x)[[2]]) == TRUE) == TRUE) 
                  stop("To use the \"lb2lb\" option you need to specify the labels.")
                labelsr <- dimnames(x)[[1]]
                labelsc <- dimnames(x)[[2]]
            }
            else {
                labelsr <- labelsc <- labels
            }
        }
        else {
            NA
        }
        if (isTRUE(sum(x) > 0L) == TRUE) {
            inc <- list()
            rws <- vector()
            cls <- vector()
            for (k in 1:max(x)) {
                X <- dichot(x, c = k)
                for (i in 1:length(which((X) == 1L))) {
                  cls[i] <- (ceiling(which((X) == 1L)/dim(x)[1]))[i]
                  ifelse((which((X) == 1L)%%dim(x)[1])[i] == 
                    0L, rws[i] <- (which((X) == 1L)%%dim(x)[1])[i] + 
                    dim(x)[1], rws[i] <- (which((X) == 1L)%%dim(x)[1])[i])
                  ifelse(isTRUE(lb2lb == TRUE) == TRUE, inc[[length(inc) + 
                    1L]] <- paste(labelsr[rws[i]], labelsc[cls[i]], 
                    sep = prsep), inc[[length(inc) + 1L]] <- paste(rws[i], 
                    cls[i], sep = prsep))
                }
                rm(i)
            }
            rm(k)
            return(sort(unlist(inc)))
        }
        else {
            return(paste(0, 0, sep = prsep))
        }
    }
    if (match.arg(type) == "toarray") {
        if (is.character(x) == FALSE) 
            return(x)
        if (missing(ord) == TRUE) {
            ord <- length(dhc(jnt(x)))
        }
        else {
            ord <- as.numeric(ord)
            if (isTRUE(nlevels(factor(dhc(x))) > ord) == TRUE) {
                ord <- nlevels(factor(dhc(x)))
                warning("Value of 'ord' given is less than the number of elements in the list.")
            }
            else {
                NA
            }
        }
        ifelse(is.null(labels) == TRUE, lbs <- levels(factor(dhc(x))), 
            lbs <- labels)
        mat <- matrix(0L, ncol = ord, nrow = ord, dimnames = list(lbs, 
            lbs))
        for (i in 1:length(x)) {
            mat[which(lbs == dhc(x[i])[1]), which(lbs == dhc(x[i])[2])] <- 1L
        }
        rm(i)
        return(mat)
    }
}
