## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
neighb <-
function (x, rs, type = c("und", "inn", "out"), k = 1, inclx = FALSE, 
    expand) 
{
    if (isTRUE("Rel.System" %in% attr(rs, "class")) == FALSE) {
        if (is.array(rs) == FALSE) {
            stop(paste(sQuote(deparse(substitute(x))), " must be at least a stacked array of square matrices.", 
                sep = ""))
        }
        warning(paste(sQuote(deparse(substitute(rs))), " is transformed into a relational system with a ", 
            dQuote("Rel.System"), " class.", sep = ""), call. = FALSE)
        rs <- rel.sys(rs, bonds = "entire")
    }
    if (isTRUE(k < 0L) == TRUE) 
        stop(paste(sQuote("k"), " must not be negative.", sep = ""))
    if (isTRUE(k == 0L) == TRUE) 
        ifelse((inclx), return(x), return(character(0)))
    ifelse(missing(expand) == FALSE && isTRUE(expand == TRUE) == 
        TRUE, expand <- TRUE, expand <- FALSE)
    if (isTRUE(all(x %in% unique(unlist(dhc(as.character(rs$nodes)))))) == 
        TRUE) {
        if (isTRUE(length(rs$Ties) > 0L) == TRUE) {
            rst <- as.list(unlist(rs$Ties))
            srs <- list()
            for (i in seq_len(length(rst))) {
                tmp <- vector()
                if (length(rst[[i]]) > 0L) {
                  for (n in seq_len(length(x))) {
                    for (j in seq_len(length(rst[[i]]))) {
                      if (x[n] %in% c(c(strsplit(rst[[i]][j], 
                        rs$sep)[[1]][1], strsplit(rst[[i]][j], 
                        rs$sep)[[1]][2]))) {
                        tmp <- append(tmp, rst[[i]][j])
                      }
                    }
                    rm(j)
                  }
                  rm(n)
                }
                srs[[i]] <- as.vector(unlist(tmp))
            }
            rm(i)
            attr(srs, "names") <- attr(rst, "names")
            nrs <- vector()
            for (i in seq_len(length(srs))) {
                if (isTRUE(length(srs[[i]]) > 0L) == TRUE) {
                  for (j in seq_len(length(srs[[i]]))) {
                    switch(match.arg(type), und = nrs <- append(nrs, 
                      strsplit(srs[[i]][j], rs$sep)[[1]][1]), 
                      inn = nrs <- append(nrs, strsplit(srs[[i]][j], 
                        rs$sep)[[1]][1]), out = nrs <- append(nrs, 
                        (strsplit(srs[[i]][j], rs$sep)[[1]][2])))
                    switch(match.arg(type), und = nrs <- append(nrs, 
                      strsplit(srs[[i]][j], rs$sep)[[1]][2]), 
                      inn = NA, out = NA)
                  }
                  rm(j)
                }
            }
            rm(i)
            nb <- unique(nrs)
            if (isTRUE(k > 1L) == TRUE) {
                if (isTRUE(expand == FALSE) == TRUE) {
                  for (i in 2:k) {
                    nb <- append(nb, ngbs(nb, rs, type = type))
                  }
                  rm(i)
                }
                else if (isTRUE(expand == TRUE) == TRUE) {
                  nb2 <- nb
                  nbk <- list()
                  if (!(inclx)) {
                    nbk[[1]] <- nb2[which(!(nb2 %in% x))]
                    ink <- 2L
                  }
                  else {
                    nbk[[1]] <- x
                    nbk[[2]] <- nb2[which(!(nb2 %in% x))]
                    ink <- 3
                    k <- k + 1L
                  }
                  for (i in ink:k) {
                    nb2 <- append(nb2, ngbs(nb2, rs, type = type))
                    nbk[[i]] <- nb2[which(!(nb2 %in% c(nb, unlist(nbk))))]
                  }
                  rm(i)
                  ifelse(!(inclx), attr(nbk, "names") <- paste0("k=", 
                    seq_len(k)), attr(nbk, "names") <- paste0("k=", 
                    seq_along((ink - 2L):k) - 1L))
                }
            }
            else {
                NA
            }
        }
        else if (isTRUE(length(rs$Ties) > 0L) == FALSE) {
            nb <- x
        }
        if (isTRUE(expand == FALSE) == TRUE) {
            ifelse(!(inclx), return(nb[which(!(nb %in% x))]), 
                return(nb))
        }
        else if (isTRUE(expand == TRUE) == TRUE) {
            return(nbk)
        }
    }
    else {
        warning(paste(sQuote(deparse(substitute(x))), " is not part of ", 
            sQuote(deparse(substitute(rs))), ".", sep = ""), 
            call. = FALSE)
        x
    }
}

