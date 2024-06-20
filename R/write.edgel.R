write.edgel <-
function (x, file = NULL, sep = "\t", header = TRUE) 
{
    if (isTRUE(is.na(dim(x)[3]) == TRUE) == FALSE) {
        if (isTRUE(is.null(dimnames(x)[[3]])) == TRUE) 
            dimnames(x)[[3]] <- seq_len(dim(x)[3])
    }
    if (isTRUE(is.null(dimnames(x)[[1]])) == TRUE) 
        dimnames(x)[[1]] <- dimnames(x)[[2]] <- seq_len(dim(x)[1])
    if (isTRUE(dim(x)[3] == 1) == TRUE) 
        x <- x[, , 1]
    if (isTRUE(is.na(dim(x)[3]) == TRUE) == TRUE) {
        if (header) {
            cat(paste("Sender", "Receiver", "Ties", sep = sep), 
                file = file, sep = "\n", append = TRUE)
        }
        tmp <- transf(x, type = "tolist", lb2lb = TRUE, lbs = dimnames(x)[[1]])
        for (i in seq_len(length(tmp))) {
            cat(paste(strsplit(tmp[i], ", ")[[1]][1], strsplit(tmp[i], 
                ", ")[[1]][2], "1", sep = sep), file = file, 
                sep = "\n", append = TRUE)
        }
        rm(i)
    }
    else if (isTRUE(is.na(dim(x)[3]) == TRUE) == FALSE) {
        ts <- rep(0L, dim(x)[3])
        DF <- data.frame(matrix(ncol = (dim(x)[3] + 2L), nrow = 0L))
        for (k in seq_len(dim(x)[3])) {
            tmp <- trnf(x[, , k], tolist = TRUE, lb2lb = TRUE, 
                lbs = dimnames(x)[[1]])
            for (i in seq_len(length(tmp))) {
                ts[k] <- 1L
                DF[(nrow(DF) + 1L), ] <- c(strsplit(tmp[i], ", ")[[1]][1], 
                  strsplit(tmp[i], ", ")[[1]][2], ts)
                ts <- rep(0L, dim(x)[3])
            }
            rm(i)
        }
        rm(k)
        isol <- dimnames(x)[[1]][which(!(dimnames(x)[[1]] %in% 
            dimnames(rm.isol(x))[[1]]))]
        if (isTRUE(length(isol) > 0L) == TRUE) {
            for (i in seq_len(length(isol))) {
                DF[(nrow(DF) + 1L), ] <- c(isol[i], isol[i], 
                  ts)
            }
            rm(i)
        }
        colnames(DF) <- c("Sender", "Receiver", dimnames(x)[[3]])
        if (header) {
            utils::write.table(DF, file = file, quote = FALSE, 
                sep = sep, row.names = FALSE, col.names = TRUE)
        }
        else if (!header) {
            utils::write.table(DF, file = file, quote = FALSE, 
                sep = sep, row.names = FALSE, col.names = FALSE)
        }
    }
}
