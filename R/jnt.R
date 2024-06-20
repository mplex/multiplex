jnt <-
function (xj, unique = FALSE, sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (isTRUE(length(xj) != 0) == TRUE) {
        if (isTRUE(is.list(xj)) == TRUE) {
            Xj <- list()
            jt <- list()
            length(Xj) <- length(jt) <- length(xj)
            for (i in seq_len(length(xj))) {
                if (isTRUE(length(xj[[i]]) != 0) == TRUE) {
                  tmpj <- as.list(xj[[i]])
                  for (j in seq_len(length(xj[[i]]))) {
                    ifelse(isTRUE("." %in% strsplit(sep, "")[[1]]) == 
                      TRUE, Xj[[i]] <- append(Xj[[i]], tmpj[[j]]), 
                      Xj[[i]] <- append(Xj[[i]], strsplit(tmpj[[j]], 
                        sep)[[1]]))
                  }
                  rm(j, tmpj)
                  Xj[[i]] <- unique(Xj[[i]])
                  if (length(Xj[[i]]) == 1) 
                    jt[[i]] <- Xj[[i]]
                  if (length(Xj[[i]]) > 1) 
                    jt[[i]] <- paste(Xj[[i]][1], Xj[[i]][2], 
                      sep = sep)
                  if (length(Xj[[i]]) > 2) {
                    for (j in 3:length(Xj[[i]])) jt[[i]] <- paste(jt[[i]], 
                      Xj[[i]][j], sep = sep)
                    rm(j)
                  }
                }
                else {
                  NA
                }
            }
            rm(i)
            ifelse(is.null(attr(xj, "names")) == FALSE, attr(jt, 
                "names") <- attr(xj, "names"), NA)
        }
        else if (isTRUE(is.vector(xj)) == TRUE) {
            vec <- vector()
            for (i in seq_len(length(xj))) {
                ifelse(is.numeric(xj) == TRUE, vec <- append(vec, 
                  strsplit(as.character(xj)[i], sep)[[1]]), vec <- append(vec, 
                  strsplit(xj[i], sep)[[1]]))
            }
            rm(i)
            if (isTRUE(unique == TRUE) == TRUE) {
                vec <- unique(vec)
            }
            else {
                vec <- levels(factor(vec))
            }
            if (length(vec) == 1) 
                jt <- vec
            if (length(vec) > 1) 
                jt <- paste(vec[1], vec[2], sep = sep)
            if (length(vec) > 2) {
                for (i in 3:length(vec)) {
                  jt <- paste(jt, vec[i], sep = sep)
                }
                rm(i)
            }
        }
        else {
            stop("Input data must be a list or a vector.")
        }
        return(jt)
    }
    else {
        xj
    }
}
