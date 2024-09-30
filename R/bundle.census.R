bundle.census <-
function (x, loops = FALSE) 
{
    if (isTRUE(is.array(x)) == FALSE) 
        stop("'x' sholud be an array.")
    if (isTRUE(dim(x)[1] == dim(x)[2]) == FALSE) 
        stop("'x' must be a square array.")
    sep <- ", "
    lb2lb <- TRUE
    ifelse(isTRUE(is.null(dimnames(x)[1]) == TRUE | is.null(dimnames(x)[1][[1]]) == 
        TRUE) == TRUE, LBS <- seq_len(nrow(x)), LBS <- dimnames(x)[[1]])
    lbs <- seq(LBS)
    xd <- dichot(x, c = 1L, diag = TRUE)
    ifelse(isTRUE(dim(xd)[3] == 1) == TRUE, xd <- xd[, , 1], 
        NA)
    if (is.na(dim(xd)[3]) == FALSE) {
        TRD <- TRUE
        r <- dim(x)[3]
        if (isTRUE(is.null(dimnames(x)[[3]]) == TRUE) | isTRUE(any(duplicated(dimnames(x)[[3]]))) == 
            TRUE) {
            dimnames(x)[[3]] <- seq_len(dim(x)[3])
        }
        mlt <- list()
        for (i in seq_len(dim(x)[3])) {
            mlt[[i]] <- trnf(xd[, , i], tolist = TRUE, lbs = lbs, 
                lb2lb = lb2lb, sep = sep)
        }
        rm(i)
        m <- unlist(mlt)
    }
    else {
        TRD <- FALSE
        r <- 1L
        m <- trnf(xd, tolist = TRUE, lbs = lbs, lb2lb = lb2lb, 
            sep = sep)
    }
    bd <- bnd(x, xd, lbs, TRD, r, m, mlt, sep)
    Et <- vector()
    if (isTRUE(TRD == TRUE) == TRUE) {
        for (i in seq_len(length(bd$Eout))) {
            for (j in seq_len(length(bd$Eout[[i]]))) {
                if (isTRUE(length(bd$Eout[[i]]) != 0L) == TRUE) {
                  Et <- append(Et, paste(lbs[i], bd$Eout[[i]][j], 
                    sep = sep))
                }
            }
            rm(j)
        }
        rm(i)
        ENT <- list()
        for (k in seq_len(r)) {
            tmp <- vector()
            for (i in which(Et %in% trnf(xd[, , k], tolist = TRUE, 
                lbs = lbs, lb2lb = lb2lb, sep = sep))) {
                tmp <- append(tmp, Et[i])
            }
            rm(i)
            ENT[[k]] <- as.character(tmp)
        }
        rm(k)
    }
    else {
        ENT <- as.character(Et)
    }
    if (loops) {
        if (isTRUE(is.na(dim(xd)[3])) == FALSE) {
            vec <- vector()
            for (i in seq_len(dim(xd)[3])) {
                vec <- append(vec, sum(diag(xd[, , i])))
            }
            rm(i)
            lop <- sum(vec)
        }
        else {
            lop <- sum(diag(xd))
        }
    }
    tbnd <- (length(unlist(bd$full))/2L + length(unlist(bd$asym))/2L + 
        length(unlist(bd$recp))/2L + length(unlist(bd$xchg))/2L + 
        length(unlist(bd$Eout))/1L + (length(unlist(bd$tripfl))/2L) - 
        (length(which(table(unlist(ENT)) > 2L))))
    ifelse(isTRUE(loops == FALSE) == TRUE, {
        bc <- cbind(abs(choose(nrow(xd), 2) - (choose(nrow(xd), 
            2)) - tbnd), (choose(nrow(xd), 2)) - tbnd, length(unlist(bd$asym))/2L, 
            length(unlist(bd$recp))/2L, length(unlist(bd$Eout))/1L, 
            length(unlist(bd$xchg))/2L, (length(unlist(bd$tripfl))/2L) - 
                (length(which(table(unlist(ENT)) > 2L))), length(unlist(bd$full))/2L)
        colnames(bc) <- c("BUNDLES", "NULL", "ASYMM", "RECIP", 
            "T.ENTR", "T.EXCH", "MIXED", "FULL")
        rownames(bc) <- "TOTAL"
    }, {
        bc <- cbind(abs(choose(nrow(xd), 2) - (choose(nrow(xd), 
            2)) - tbnd), (choose(nrow(xd), 2)) - tbnd, length(unlist(bd$asym))/2L, 
            length(unlist(bd$recp))/2L, length(unlist(bd$Eout))/1L, 
            length(unlist(bd$xchg))/2L, (length(unlist(bd$tripfl))/2L) - 
                (length(which(table(unlist(ENT)) > 2L))), length(unlist(bd$full))/2L, 
            lop)
        colnames(bc) <- c("BUNDLES", "NULL", "ASYMM", "RECIP", 
            "T.ENTR", "T.EXCH", "MIXED", "FULL", "LOOP")
        rownames(bc) <- "TOTAL"
    })
    return(bc)
}
