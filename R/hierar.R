## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
hierar <-
function (W, x, type = c("person", "relation")) 
{
    if (isTRUE("Rel.Box" %in% attr(W, "class")) == FALSE) 
        stop(paste(sQuote(deparse(substitute(W))), " must be a ", 
            dQuote("Rel.Box"), " class object.", sep = ""))
    if (isTRUE(is.null(W$lbs) == TRUE) == FALSE) {
        if (isTRUE(is.numeric(x) == TRUE) == TRUE) {
            if (isTRUE(length(W$lbs) < x) == TRUE) 
                stop(paste(sQuote(deparse(substitute(x))), " exceeds the order of ", 
                  sQuote(deparse(substitute(W))), ".", sep = ""))
        }
        if (isTRUE(is.character(x) == TRUE) == TRUE) {
            if (isTRUE(x %in% W$lbs) == FALSE) 
                stop(paste(sQuote(deparse(substitute(x))), " is not part of ", 
                  sQuote(deparse(substitute(W))), ".", sep = ""))
        }
    }
    else if (isTRUE(is.null(W$lbs) == TRUE) == TRUE) {
        if (isTRUE(dim(W$W)[1] < x) == TRUE) 
            stop(paste(sQuote(deparse(substitute(x))), " exceeds the order of ", 
                sQuote(deparse(substitute(W))), ".", sep = ""))
    }
    ifelse(isTRUE(is.numeric(x) == TRUE), X <- x, X <- which(W$lbs == 
        x))
    rele <- unique(t(W$W[X, , ]))
    switch(match.arg(type), person = {
        Ph <- as.data.frame(array(0, dim = c(nrow(t(rele)), nrow(t(rele)))))
    }, relation = {
        Rh <- as.data.frame(array(0, dim = c(nrow(rele), nrow(rele))))
    })
    switch(match.arg(type), person = {
        for (j in seq_len(nrow(t(rele)))) {
            for (i in seq_len(nrow(t(rele)))) {
                if ((as.numeric(any(rele[, i] < rele[, j])) == 
                  1 && as.numeric(any(rele[, j] < rele[, i])) == 
                  0) | as.numeric(all(rele[, i] == rele[, j])) == 
                  1) Ph[i, j] <- 1
            }
        }
        rm(j)
        for (i in 1:nrow(t(rele))) {
            if (sum(rele[, i]) == 0) Ph[i, ] <- 0
        }
        rm(i)
        ph <- as.matrix(Ph)
        if (isTRUE(is.null(W$lbs) == FALSE)) dimnames(ph)[[1]] <- dimnames(ph)[[2]] <- W$lbs
        ph <- list(ph = ph, pers = x)
        class(ph) <- c("Person.Hierarchy")
        return(ph)
    }, relation = {
        for (j in seq_len(nrow(rele))) {
            for (i in seq_len(nrow(rele))) {
                if ((as.numeric(any(rele[i, ] < rele[j, ])) == 
                  1 && as.numeric(any(rele[j, ] < rele[i, ])) == 
                  0) | as.numeric(all(rele[i, ] == rele[j, ])) == 
                  1) Rh[i, j] <- 1
            }
        }
        rm(j)
        rh <- as.matrix(Rh)
        if (isTRUE(is.null(dimnames(W$W)[[3]]) == FALSE)) attributes(rh)$dimnames[[2]] <- attributes(rh)$dimnames[[1]] <- dimnames(rele)[[1]]
        rh <- list(rh = rh, pers = x)
        class(rh) <- c("Relation.Hierarchy")
        return(rh)
    })
}

