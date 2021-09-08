transl <-
function (lt, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    llt <- levels(factor(lt))
    if (isTRUE(length(llt) == 1) == TRUE) {
        ifelse(isTRUE(length(jnt(llt)) == 1) == TRUE, return(jnt(llt)), 
            return(llt))
    }
    else {
        Ls <- as.list(llt)
        j <- 1L
        names(Ls)[1] <- j
        tmp0 <- strsplit(Ls[[1]], sep)[[1]]
        for (i in 2:length(Ls)) {
            tmp2 <- strsplit(Ls[[i]], sep)[[1]]
            ifelse((any(tmp2 %in% tmp0)) | (any(tmp0 %in% tmp2)), 
                attr(Ls[[i]], "names") <- as.integer(attr(Ls[[j]], 
                  "names")), NA)
        }
        rm(i)
        uno <- unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
            "names") == j)])))
        for (i in which(is.na(names(Ls)))) {
            tmp2 <- strsplit(Ls[[i]], sep)[[1]]
            ifelse((any(uno %in% tmp2)) | (any(tmp2 %in% uno)), 
                names(Ls)[i] <- j, NA)
        }
        rm(i)
        if (isTRUE(length(uno) > 1) == TRUE) {
            while (isTRUE(unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
                "names") == j)]))) == uno) == TRUE) {
                for (i in which(is.na(names(Ls)))) {
                  tmp2 <- strsplit(Ls[[i]], sep)[[1]]
                  ifelse((any(uno %in% tmp2)) | (any(tmp2 %in% 
                    uno)), attr(Ls[[i]], "names") <- j, NA)
                }
                rm(i)
            }
        }
        while (length(which(is.na(names(Ls)))) != 0) {
            j <- (j + 1L)
            names(Ls)[which(is.na(names(Ls)))] <- j
            if (length(which(is.na(names(Ls)))) != 0) {
                tmp0 <- unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
                  "names") == j)])))
                for (i in which(is.na(names(Ls)))) {
                  tmp2 <- strsplit(Ls[[i]], sep)[[1]]
                  ifelse((any(tmp2 %in% tmp0)) | (any(tmp0 %in% 
                    tmp2)), attr(Ls[[i]], "names") <- j, NA)
                }
                rm(i)
                tmp0 <- unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
                  "names") == j)])))
                for (i in which(is.na(names(Ls)))) {
                  tmp2 <- strsplit(Ls[[i]], sep)[[1]]
                  ifelse((any(tmp0 %in% tmp2)) | (any(tmp2 %in% 
                    tmp0)), attr(Ls[[i]], "names") <- j, NA)
                }
                rm(i)
            }
        }
        clu <- vector()
        for (i in seq_len(length(Ls))) clu[i] <- as.numeric(names(Ls)[i])
        tls <- vector()
        for (i in seq_len(nlevels(factor(clu)))) tls <- append(tls, 
            jnt(llt[clu == i]))
        return(levels(factor(tls)))
    }
}
