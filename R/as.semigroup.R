as.semigroup <-
function (x, gens = NA, lbs, numerical, edgeT) 
{
    ifelse(is.list(x) == TRUE && isTRUE(length(x) == 1L) == TRUE, 
        x <- x[[1]], NA)
    if (is.null(dimnames(x)) == TRUE && isTRUE("Semigroup" %in% 
        attr(x, "class")) == FALSE && missing(numerical) == TRUE) 
        stop("Dimnames in \"x\" must be provided.")
    ifelse(missing(numerical) == FALSE && isTRUE(numerical == 
        TRUE) == TRUE, numerical <- TRUE, numerical <- FALSE)
    if (is.null(dimnames(x)) == TRUE && isTRUE(numerical == TRUE) == 
        TRUE && isTRUE("Semigroup" %in% attr(x, "class")) == 
        FALSE) {
        dimnames(x)[[1]] <- seq_len(nrow(x))
        dimnames(x)[[2]] <- seq_len(ncol(x))
    }
    if (missing(edgeT) == FALSE && isTRUE(edgeT == TRUE) == TRUE) {
        ifelse(isTRUE(attr(x, "class") == "EdgeTable") == TRUE, 
            x <- edgS(x$ET), x <- edgS(x))
    }
    if ((missing(lbs) == TRUE && isTRUE(numerical == TRUE) == 
        FALSE)) {
        if (isTRUE("Semigroup" %in% attr(x, "class")) == TRUE) {
            ifelse(is.na(gens) == TRUE, NA, x$gens <- gens)
            return(x)
        }
        else {
            ifelse(is.null(dimnames(x)) == TRUE, s <- as.matrix(x), 
                s <- as.data.frame(x))
            if (all(rownames(x) %in% levels(unlist(x))) == TRUE || 
                all(as.character(as.matrix(s)) %in% unlist(dimnames(s))) == 
                  TRUE) {
                Sst <- rownames(x)
            }
            else {
                ifelse(is.numeric(x) == TRUE, Sst <- unique(c(unlist(x), 
                  unique(c(rownames(x), colnames(x))))), Sst <- c(unique(unlist(x)), 
                  unique(c(rownames(x), colnames(x)))))
            }
            ifelse(is.numeric(gens) == TRUE && any(gens %in% 
                unique(unlist(dimnames(x)))) == FALSE, gens <- unique(unlist(dimnames(x)))[gens], 
                NA)
            ifelse(all(x %in% unique(unlist(dimnames(x)))) == 
                TRUE, lst <- list(ord = nrow(x), st = unique(unlist(dimnames(x))), 
                gens = gens, S = s), lst <- list(ord = length(Sst), 
                st = Sst, gens = gens, S = s))
            ifelse(is.character(lst$st) == TRUE, class(lst) <- c("Semigroup", 
                "symbolic"), class(lst) <- c("Semigroup", "numerical"))
            return(lst)
        }
    }
    else if ((missing(lbs) == TRUE && isTRUE(numerical == TRUE) == 
        TRUE)) {
        if (isTRUE("Semigroup" %in% attr(x, "class")) == TRUE) {
            lbs <- seq_along(x$st)
        }
        else {
            ifelse(isTRUE(dim(x)[1] == dim(x)[2]) == TRUE, lbs <- seq_len(dim(x)[1]), 
                lbs <- NULL)
        }
    }
    else {
        ifelse(is.numeric(gens) == TRUE && any(gens %in% lbs) == 
            FALSE, gens <- lbs[gens], NA)
    }
    if (isTRUE("Semigroup" %in% attr(x, "class")) == TRUE) {
        s <- as.matrix(x$S)
        ord <- x$ord
        ifelse(isTRUE(numerical == TRUE) == TRUE, Sst <- lbs, 
            Sst <- x$st)
        ifelse(is.na(gens) == TRUE, NA, x$gens <- gens)
        ifelse(is.numeric(gens) == TRUE && any(gens %in% x$st) == 
            FALSE, gens <- x$st[gens], NA)
        gens <- x$gens
    }
    else {
        if (is.array(x) == FALSE && is.data.frame(x) == FALSE) 
            stop("Data must be a square matrix or data frame")
        s <- as.matrix(x, rownames.force = TRUE)
        if (all(as.character(s) %in% unlist(dimnames(s))) == 
            TRUE || any(is.na(s)) == TRUE) {
            Sst <- rownames(s)
        }
        else {
            ifelse(is.numeric(s) == TRUE, Sst <- unique(c(unlist(x), 
                unique(c(rownames(x), colnames(x))))), Sst <- unique(levels(unlist(x)), 
                unique(c(rownames(x), colnames(x)))))
        }
        ord <- length(Sst)
    }
    if (is.null(dimnames(s)) == TRUE && isTRUE(dim(s)[1] == dim(s)[2]) == 
        TRUE) {
        ifelse(isTRUE(length(Sst) == dim(s)[1]) == TRUE, dimnames(s)[[1]] <- dimnames(s)[[2]] <- seq_along(Sst), 
            NA)
    }
    if (is.null(lbs) == FALSE) {
        z <- vector()
        for (i in seq_along(as.matrix(s))) {
            if (isTRUE(numerical == TRUE) == TRUE && isTRUE("Semigroup" %in% 
                attr(x, "class")) == TRUE) {
                ifelse(is.na(as.matrix(s)[i]) == TRUE, NA, z[i] <- lbs[which(x$st == 
                  as.matrix(s)[i])])
            }
            else {
                ifelse(is.na(as.matrix(s)[i]) == TRUE, NA, z[i] <- lbs[which(Sst == 
                  as.matrix(s)[i])])
            }
        }
        rm(i)
        s <- matrix(z, nrow = nrow(s), ncol = ncol(s))
    }
    if ((is.null(dimnames(s)) == TRUE && isTRUE(dim(s)[1] == 
        dim(s)[2]) == TRUE) || isTRUE(numerical == TRUE) == TRUE) {
        rownames(s) <- colnames(s) <- lbs
    }
    else if (isTRUE(numerical == FALSE) == TRUE || isTRUE(dim(s)[1] == 
        dim(s)[2]) == FALSE) {
        if (isTRUE(all.equal(lbs, Sst) == TRUE) == TRUE && isTRUE(rownames(x) == 
            colnames(x)) == TRUE) {
            dimnames(s)[[1]] <- dimnames(s)[[2]] <- as.list(lbs)
        }
        else if (isTRUE(all.equal(lbs, Sst) == TRUE) == TRUE && 
            isTRUE(rownames(x) == colnames(x)) == FALSE) {
            rownames(s) <- lbs[(nlevels(factor(s)) + 1L):(nlevels(factor(s)) + 
                nrow(s))]
            colnames(s) <- lbs[((nlevels(factor(s)) + nrow(s) + 
                1L)):(nlevels(factor(s)) + nrow(s) + ncol(s))]
        }
        else {
            if (isTRUE(length(lbs) != length(Sst)) == TRUE) {
                ifelse(all(rownames(x) %in% lbs) == FALSE, rownames(s) <- Sst[(nlevels(factor(s)) + 
                  1L):(nlevels(factor(s)) + nrow(s))], NA)
                ifelse(all(colnames(x) %in% lbs) == FALSE, colnames(s) <- Sst[((nlevels(factor(s)) + 
                  nrow(s) + 1L)):(nlevels(factor(s)) + nrow(s) + 
                  ncol(s))], NA)
            }
            else {
                ifelse(all(rownames(x) %in% lbs) == FALSE, rownames(s) <- lbs[(nlevels(factor(s)) + 
                  1L):(nlevels(factor(s)) + nrow(s))], NA)
                ifelse(all(colnames(x) %in% lbs) == FALSE, colnames(s) <- lbs[((nlevels(factor(s)) + 
                  nrow(s) + 1L)):(nlevels(factor(s)) + nrow(s) + 
                  ncol(s))], NA)
            }
        }
    }
    else {
        NA
    }
    Sdf <- as.data.frame(s)
    if (is.null(dimnames(s)) == TRUE && isTRUE(dim(s)[1] == dim(s)[2]) == 
        TRUE) {
        dimnames(Sdf)[[1]] <- dimnames(Sdf)[[2]] <- lbs
    }
    else {
        NA
    }
    ifelse(isTRUE(numerical == TRUE) == TRUE, lst <- list(ord = ord, 
        st = as.numeric(unique(unlist(dimnames(Sdf)))), gens = gens, 
        S = Sdf), lst <- list(ord = ord, st = unique(unlist(dimnames(Sdf))), 
        gens = gens, S = Sdf))
    ifelse(is.character(lst$st) == TRUE, class(lst) <- c("Semigroup", 
        "symbolic"), class(lst) <- c("Semigroup", "numerical"))
    return(lst)
}
