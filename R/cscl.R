## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
cscl <-
function (x, cl, scale = c("nominal", "ordinal", "intord", "biord", 
    "dichot"), sep) 
{
    if (is.array(x) == FALSE) 
        stop(paste("Input in ", sQuote(deparse(substitute(x))), 
            " should be a table or an array.", sep = ""))
    if (any(cl %in% unique(x)) == FALSE && is.null(cl) == FALSE) {
        warning(paste("Any value in ", sQuote(deparse(substitute(x))), 
            " correspond to concept-levels in ", sQuote(deparse(substitute(cl))), 
            " and return ", sQuote("x"), ".", sep = ""), call. = FALSE)
        return(x)
    }
    else {
        if (is.null(cl) == TRUE) 
            return(x)
        if (is.vector(cl) == FALSE) 
            stop(paste("Concept-levels in ", sQuote(deparse(substitute(cl))), 
                " should be a list or vector.", sep = ""))
        flgd <- FALSE
        if (is.list(cl) == TRUE) {
            scl <- diag(length(unlist(cl)))
            colnames(scl) <- rownames(scl) <- unlist(cl)
        }
        else {
            if (match.arg(scale) == "dichot") {
                if (isTRUE(length(cl) != 2) == TRUE) 
                  stop("Dichotomic scale requires a vector of conceptual levels with length 2.")
                scl <- diag(2)
                flgd <- TRUE
            }
            else if (match.arg(scale) == "biord") {
                stop("Biordinal scale requires a list of conceptual levels with length 2.")
            }
            else {
                scl <- diag(length(cl))
            }
            colnames(scl) <- rownames(scl) <- cl
        }
        if (isTRUE(flgd == FALSE) == TRUE) {
            switch(match.arg(scale), nominal = {
                NA
            }, ordinal = {
                scl[upper.tri(scl)] <- 1L
            }, intord = {
                scl[seq_len(round(length(cl)/2L)), seq_len(round(length(cl)/2L))][upper.tri(scl[seq_len(round(length(cl)/2L)), 
                  seq_len(round(length(cl)/2L))])] <- 1L
                scl[(round(length(cl)/2L) + 1L):nrow(scl), (round(length(cl)/2L) + 
                  1L):ncol(scl)][lower.tri(scl[(round(length(cl)/2L) + 
                  1L):nrow(scl), (round(length(cl)/2L) + 1L):ncol(scl)])] <- 1L
            }, biord = {
                scl[seq_len(length(cl[[1]])), seq_len(length(cl[[1]]))][upper.tri(scl[seq_len(length(cl[[1]])), 
                  seq_len(length(cl[[1]]))])] <- 1L
                scl[(length(cl[[1]]) + 1):nrow(scl), (length(cl[[1]]) + 
                  1):ncol(scl)][lower.tri(scl[(length(cl[[1]]) + 
                  1):nrow(scl), (length(cl[[1]]) + 1):ncol(scl)])] <- 1L
                ifelse(isTRUE(length(cl) == 2) == TRUE, NA, warning(paste("Biordinal scale ", 
                  dQuote("biord"), " in ", sQuote("scale"), " is for two-dimensional.", 
                  sep = ""), call. = FALSE))
            })
        }
        else {
            NA
        }
        scl <- as.data.frame(scl)
    }
    ifelse(missing(sep) == TRUE, sep <- ".", NA)
    lvl <- attr(scl, "names")
    dx <- data.frame(matrix(NA, ncol = ncol(x) * ncol(scl), nrow = 0))
    for (i in seq_len(nrow(x))) {
        tmp <- vector()
        for (j in seq_len(ncol(x))) {
            tmp <- c(tmp, as.vector(unlist(scl[which(x[i, j] == 
                lvl), ])))
        }
        rm(j)
        dx <- rbind(dx, tmp)
    }
    rm(i)
    rownames(dx) <- rownames(x)
    colnames(dx) <- paste0(rep(colnames(x), each = length(lvl)), 
        sep, lvl)
    attr(dx, "class") <- c(class(dx), "conceptual.scaling", jnt(cl, 
        uniq = TRUE), sep)
    dx
}

