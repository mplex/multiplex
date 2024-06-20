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
        tmp0 <- strsplit(Ls[[1]], sep)[[1]]
        attr(Ls[[1]], "names") <- j
        for (i in 2:length(Ls)) {
            tmp2 <- strsplit(Ls[[i]], sep)[[1]]
            ifelse((any(tmp2 %in% tmp0)) | (any(tmp0 %in% tmp2)), 
                attr(Ls[[i]], "names") <- as.integer(attr(Ls[[j]], 
                  "names")), NA)
        }
        rm(i)
        uno <- unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
            "names") == j)])))
        for (i in which(attr(unlist(Ls), "names") == "")) {
            tmp2 <- strsplit(Ls[[i]], sep)[[1]]
            ifelse((any(uno %in% tmp2)) | (any(tmp2 %in% uno)), 
                attr(Ls[[i]], "names") <- j, NA)
            uno <- unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
                "names") == j)])))
        }
        rm(i)
        if (isTRUE(length(uno) > 1) == TRUE) {
            while (isTRUE(unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
                "names") == j)]))) == uno) == TRUE) {
                for (i in which(attr(unlist(Ls), "names") == 
                  "")) {
                  tmp2 <- strsplit(Ls[[i]], sep)[[1]]
                  ifelse((any(uno %in% tmp2)) | (any(tmp2 %in% 
                    uno)), attr(Ls[[i]], "names") <- j, NA)
                }
                rm(i)
            }
        }
        kual <- which(attr(unlist(Ls), "names") == "")
        while (length(kual) != 0) {
            j <- (j + 1L)
            attr(Ls[[kual[1]]], "names") <- j
            kual <- which(attr(unlist(Ls), "names") == "")
            if (length(kual) != 0) {
                tmp0 <- unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
                  "names") == j)])))
                for (i in kual) {
                  tmp2 <- strsplit(Ls[[i]], sep)[[1]]
                  ifelse((any(tmp2 %in% tmp0)) | (any(tmp0 %in% 
                    tmp2)), attr(Ls[[i]], "names") <- j, NA)
                }
                rm(i)
                tmp0 <- unlist(dhc(jnt(unlist(Ls)[which(attr(unlist(Ls), 
                  "names") == j)])))
                for (i in kual) {
                  tmp2 <- strsplit(Ls[[i]], sep)[[1]]
                  ifelse((any(tmp0 %in% tmp2)) | (any(tmp2 %in% 
                    tmp0)), attr(Ls[[i]], "names") <- j, NA)
                }
                rm(i)
            }
            kual <- which(attr(unlist(Ls), "names") == "")
        }
        clu <- vector()
        for (i in seq_len(length(Ls))) clu[i] <- as.numeric(attr(Ls[[i]], 
            "names")[1])
        tls <- vector()
        for (i in seq_len(nlevels(factor(clu)))) tls <- append(tls, 
            jnt(llt[clu == i]))
        return(levels(factor(tls)))
    }
}
