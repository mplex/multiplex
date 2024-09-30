ti <-
function (net) 
{
    ifelse(isTRUE(is.data.frame(net) == TRUE) == TRUE, net <- as.matrix(net), 
        NA)
    ifelse(isTRUE(dim(net)[3] == 1) == TRUE, net <- net[, , 1], 
        NA)
    ifelse(isTRUE(is.na(dim(net)[3]) == TRUE) == TRUE, z <- 1L, 
        z <- dim(net)[3])
    if (isTRUE(z == 1) == TRUE) {
        x <- net
        cp <- unique(cpath(x, sep = ", ", lb2lb = FALSE))
        if (is.list(cp) == TRUE && isTRUE(sum(unlist(cp)) == 
            0) == TRUE) {
            NA
        }
        else {
            for (i in seq_len(length(cp))) {
                pck <- as.numeric(dhc(cp[i], sep = ", "))
                if (isTRUE(length(pck) > 2) == TRUE && isTRUE(x[pck, 
                  pck][1, 3] != 0) == TRUE) {
                  tri <- net[pck, pck]
                  if (isTRUE(sum(tri[1, 2] + tri[2, 3]) < tri[1, 
                    3]) == TRUE) {
                    x[pck[c(1, 3)][1], pck[c(1, 3)][2]] <- 0L
                  }
                  else {
                    NA
                  }
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        Qq <- x
    }
    else {
        Qq <- net
        for (k in seq_len(z)) {
            x <- net[, , k]
            cp <- unique(cpath(x, sep = ", ", lb2lb = FALSE))
            for (i in seq_len(length(cp))) {
                pck <- as.numeric(dhc(cp[i], sep = ", "))
                if (isTRUE(length(pck) > 2) == TRUE && isTRUE(x[pck, 
                  pck][1, 3] != 0) == TRUE) {
                  tri <- x[pck, pck]
                  if (isTRUE(sum(tri[1, 2] + tri[2, 3]) < tri[1, 
                    3]) == TRUE) {
                    x[pck[c(1, 3)][1], pck[c(1, 3)][2]] <- 0L
                  }
                  else {
                    NA
                  }
                }
                else {
                  NA
                }
            }
            rm(i)
            Qq[, , k] <- x
        }
        rm(k)
    }
    Qq
}
