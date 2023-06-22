green.rel <-
function (S) 
{
    ifelse(isTRUE(tolower(class(S)[1]) != "semigroup") == TRUE, 
        S <- as.semigroup(S), NA)
    x <- S$S
    st <- colnames(x)
    rrel <- vector("list", length = nrow(x))
    names(rrel) <- dimnames(x)[[1]]
    for (i in seq_len(nrow(x))) {
        rrel[[i]] <- sort(unique(as.vector(unlist(x[i, ]))))
    }
    rm(i)
    urrel <- unique(rrel)
    clur <- vector("list", length = length(urrel))
    names(clur) <- urrel
    for (i in seq_len(length(urrel))) {
        clur[[i]] <- which(rrel %in% list(urrel[[i]]))
    }
    rm(i)
    lrel <- vector("list", length = ncol(x))
    names(lrel) <- dimnames(x)[[2]]
    for (i in seq_len(ncol(x))) {
        lrel[[i]] <- sort(unique(as.vector(unlist(x[, i]))))
    }
    rm(i)
    ulrel <- unique(lrel)
    clul <- vector("list", length = length(ulrel))
    names(clul) <- ulrel
    for (i in seq_len(length(ulrel))) {
        clul[[i]] <- which(lrel %in% list(ulrel[[i]]))
    }
    rm(i)
    xe <- x[unlist(clur, use.names = FALSE), unlist(clul, use.names = FALSE)]
    lstr <- vector("list", length = length(clur))
    for (i in seq_len(length(lstr))) {
        lstr[[i]] <- xe[which(rownames(xe) %in% st[clur[[i]]]), 
            ]
    }
    rm(i)
    lstl <- vector("list", length = length(clul))
    for (i in seq_len(length(lstl))) {
        lstl[[i]] <- xe[, which(colnames(xe) %in% st[clul[[i]]])]
    }
    rm(i)
    lstrl <- vector("list", length = length(clur) * length(clul))
    k <- 1
    for (i in seq_len(length(lstr))) {
        for (j in seq_len(length(lstl))) {
            if (is.null(colnames(lstl[[j]])) == TRUE) {
                tmp <- lstr[[i]][which(colnames(lstr[[i]]) %in% 
                  st[clul[[j]]])]
                lstrl[[k]] <- tmp
            }
            else {
                tmp <- lstr[[i]][which(colnames(lstr[[i]]) %in% 
                  colnames(lstl[[j]]))]
                lstrl[[k]] <- tmp
            }
            k <- k + 1
        }
        rm(j)
    }
    rm(i)
    clurs <- clur
    for (i in seq_len(length(clur))) {
        clurs[[i]] <- c(st[clur[[i]]], "|")
    }
    rm(i)
    cluls <- clul
    for (i in seq_len(length(clul))) {
        cluls[[i]] <- c(st[clul[[i]]], "|")
    }
    rm(i)
    clr <- lapply(clur, length)
    cll <- lapply(clul, length)
    taqs <- which(seq_len(length(lstrl))%%length(lstl) == 1)
    tpqs <- which(seq_len(length(lstrl))%%length(lstl) == 0)
    xep <- data.frame(matrix(nrow = 0, ncol = (dim(xe)[2] + length(cll) - 
        1)))
    coln <- unlist(cluls, use.names = FALSE)
    colnames(xep) <- gsub("[|]", " ", coln)[seq_len(length(coln) - 
        1)]
    if (isTRUE(length(taqs) == 0) == TRUE) {
        xep <- xe
    }
    else {
        for (i in seq_len(length(taqs))) {
            temp <- lapply(lstrl[taqs[i]:tpqs[i]], function(x) {
                cbind(x, "|")
            })
            if (isTRUE(nrow(temp[[1]]) == 1) == TRUE) {
                utemp <- unlist(temp, use.names = FALSE)
                utemp[which(names(lapply(unlist(temp, recursive = FALSE), 
                  c)) == "\"|\"")] <- "|"
            }
            else {
                utemp <- Reduce(cbind, temp)
            }
            if (isTRUE(nrow(xep) == 0) == TRUE) {
                if (is.data.frame(utemp) == FALSE) {
                  xep[1:2, ] <- rbind(rep(" ", ncol(xep)), noquote(utemp[-length(utemp)]))
                  rownames(xep)[1:2] <- c("", coln[1])
                }
                else {
                  xep[seq_len(as.numeric(clr[1]) + 1), ] <- rbind(rep(" ", 
                    ncol(xep)), as.matrix(unname(utemp[-length(utemp)])))
                  rownames(xep)[seq_len(as.numeric(clr[1]) + 
                    1)] <- c("", rownames(xe)[seq_len(as.numeric(clr[1]))])
                }
            }
            else {
                if (is.vector(utemp) == TRUE) {
                  xep <- rbind(xep, utemp[-length(utemp)])
                  rownames(xep)[nrow(xep)] <- rownames(temp[[1]])
                }
                else {
                  if (is.data.frame(unname(utemp[-length(utemp)])) == 
                    FALSE) {
                    xep <- noquote(rbind(as.matrix(xep), as.vector(unname(utemp[-length(utemp)]))))
                    rownames(xep)[nrow(xep)] <- clurs[[i]][-length(clurs[[i]])]
                  }
                  else {
                    xep <- noquote(rbind(as.matrix(xep), as.matrix(unname(utemp[-length(utemp)]))))
                  }
                }
            }
            ifelse(isTRUE(i == length(taqs)) == TRUE, NA, xep <- rbind(as.matrix(xep), 
                rep("-", ncol(xep))))
        }
        rm(i)
    }
    clrs <- vector(length = S$ord)
    for (i in seq_len(length(clurs))) clrs[which(st %in% clurs[[i]])] <- i
    clls <- vector(length = S$ord)
    for (i in seq_len(length(cluls))) clls[which(st %in% cluls[[i]])] <- i
    lst <- list(S = x, ord = S$ord, st = st, clu = list(R = clrs, 
        L = clls), R = noquote(unlist(clurs, use.names = FALSE)[-length(unlist(clurs))]), 
        L = noquote(unlist(cluls, use.names = FALSE)[-length(unlist(cluls))]), 
        D = noquote(xep))
    class(lst) <- c("Semigroup", "Green.Rels")
    return(lst)
}
