## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
jnt <-
function (z, uniq, sep) 
{
    ifelse(missing(uniq) == FALSE && isTRUE(uniq == TRUE) == 
        TRUE, uniq <- TRUE, uniq <- FALSE)
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (isTRUE(length(z) != 0) == TRUE) {
        if (is.list(z) == TRUE) {
            tj <- list()
            jt <- list()
            length(tj) <- length(jt) <- length(z)
            for (i in seq_len(length(z))) {
                if (isTRUE(length(z[[i]]) != 0) == TRUE) {
                  tmpj <- as.list(z[[i]])
                  for (j in seq_len(length(z[[i]]))) {
                    ifelse(isTRUE("." %in% strsplit(sep, "")[[1]]) == 
                      TRUE, tj[[i]] <- append(tj[[i]], tmpj[[j]]), 
                      tj[[i]] <- append(tj[[i]], strsplit(tmpj[[j]], 
                        sep)[[1]]))
                  }
                  rm(j, tmpj)
                  tj[[i]] <- unique(tj[[i]])
                  if (length(tj[[i]]) == 1) 
                    jt[[i]] <- tj[[i]]
                  if (length(tj[[i]]) > 1) 
                    jt[[i]] <- paste(tj[[i]][1], tj[[i]][2], 
                      sep = sep)
                  if (length(tj[[i]]) > 2) {
                    for (j in 3:length(tj[[i]])) jt[[i]] <- paste(jt[[i]], 
                      tj[[i]][j], sep = sep)
                    rm(j)
                  }
                }
                else {
                  NA
                }
            }
            rm(i)
            ifelse(is.null(attr(z, "names")) == FALSE, attr(jt, 
                "names") <- attr(z, "names"), NA)
        }
        else if (is.vector(z) == TRUE) {
            vec <- vector()
            for (i in seq_len(length(z))) {
                ifelse(is.numeric(z) == TRUE, vec <- append(vec, 
                  strsplit(as.character(z)[i], sep)[[1]]), vec <- append(vec, 
                  strsplit(z[i], sep)[[1]]))
            }
            rm(i)
            if (isTRUE(uniq == TRUE) == TRUE) {
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
        z
    }
}

