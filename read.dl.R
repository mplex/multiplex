read.dl <-
function (file) 
{
    arx <- scan(file, what = "character", nlines = -1, quiet = TRUE)
    if (isTRUE(arx[1] == "DL") == FALSE) 
        stop("Input file must have a DL format.")
    tip <- dhc(arx[2:3], sep = "=")
    if (isTRUE(tip[1] == "N") == TRUE) {
        if (any(tip == "NM") == TRUE) {
            arr <- array(NA, dim = c(as.numeric(rep(tip[2], 2)), 
                as.numeric(tip[4])))
            if (isTRUE(length(which(arx == "ROW")) > 0) == TRUE) {
                dimnames(arr)[[1]] <- arx[(which(arx == "ROW")[1] + 
                  2L):(which(arx == "ROW")[1] + 1L + dim(arr)[1])]
            }
            else {
                NA
            }
            if (isTRUE(length(which(arx == "COLUMN")) > 0) == 
                TRUE) {
                dimnames(arr)[[2]] <- arx[(which(arx == "COLUMN")[1] + 
                  2L):(which(arx == "COLUMN")[1] + 1L + dim(arr)[2])]
            }
            else {
                NA
            }
            if (isTRUE(length(which(arx == "ROW")) > 0) == TRUE && 
                isTRUE(length(which(arx == "COLUMN")) > 0) == 
                  TRUE) {
                dimnames(arr)[[3]] <- arx[(which(arx == "LABELS:")[3] + 
                  1L):(which(arx == "LABELS:")[3] + dim(arr)[3])]
            }
            else {
                dimnames(arr)[[3]] <- arx[(which(arx == "LABELS:")[1] + 
                  1L):(which(arx == "LABELS:")[1] + dim(arr)[3])]
            }
            lnch <- which(arx == "DATA:")
            lgtv <- dim(arr)[1]^2L
            arrt <- arr
            for (k in 1:dim(arr)[3]) {
                arrt[, , k] <- as.numeric(arx[(lnch + 1):(lgtv + 
                  lnch)])
                arr[, , k] <- t(arrt[, , k])
                lnch <- (lgtv + lnch)
            }
            rm(k, arrt, lnch, lgtv)
        }
        else if (any(tip == "NM") == FALSE) {
            if (any(tip == "EDGELIST1") == TRUE) {
                stop("\"EDGELIST\" option in DL is not yet supported for reading.")
            }
            else {
                arrt <- array(as.numeric(arx[(which(arx == "DATA:") + 
                  1L):length(arx)]), dim = c(as.numeric(rep(tip[2], 
                  2))))
                arr <- t(arrt)
                dimnames(arr)[[1]] <- dimnames(arr)[[2]] <- arx[(which(arx == 
                  "LABELS:")[1] + 1L):(which(arx == "LABELS:")[1] + 
                  as.numeric(tip[2]))]
            }
        }
        else {
            NA
        }
    }
    else if (isTRUE(tip[1] == "NR") == TRUE) {
        tip <- dhc(tip, ",")
        arrt <- data.frame(matrix(as.numeric(arx[(which(arx == 
            "DATA:") + 1L):length(arx)]), ncol = as.numeric(tip[which(tip == 
            "NR") + 1]), nrow = as.numeric(tip[which(tip == "NC") + 
            1])))
        arr <- t(arrt)
        if (isTRUE(length(which(arx == "COLUMN")) > 0) == TRUE) {
            colnames(arr) <- arx[(which(arx == "COLUMN") + 2L):((which(arx == 
                "COLUMN") + 1L + ncol(arr)))]
        }
        else {
            NA
        }
        if (isTRUE(length(which(arx == "ROW")) > 0) == TRUE) {
            rownames(arr) <- arx[(which(arx == "ROW") + 2L):((which(arx == 
                "ROW") + 1L + nrow(arr)))]
        }
        else {
            NA
        }
    }
    else {
        stop("Format not supported.")
    }
    arr
}
