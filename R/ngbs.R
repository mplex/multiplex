## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
ngbs <-
function (x, rs, type = c("und", "inn", "out"), num = FALSE) 
{
    if (isTRUE("Rel.System" %in% attr(rs, "class")) == FALSE) 
        stop(paste(sQuote(deparse(substitute(rs))), " must be a ", 
            dQuote("Rel.System"), " class object.", sep = ""))
    ifelse(isTRUE(is.numeric(x)) == TRUE, x <- rs$nodes[x], NA)
    if (isTRUE(all(x %in% unique(unlist(dhc(as.character(rs$nodes)))))) == 
        TRUE) {
        rst <- as.list(unlist(rs$Ties))
        srs <- list()
        for (i in seq_len(length(rst))) {
            tmp <- vector()
            if (length(rst[[i]]) > 0) {
                for (n in seq_len(length(x))) {
                  for (j in seq_len(length(rst[[i]]))) {
                    if (x[n] %in% c(c(strsplit(rst[[i]][j], rs$sep)[[1]][1], 
                      strsplit(rst[[i]][j], rs$sep)[[1]][2]))) {
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
            if (isTRUE(length(srs[[i]]) > 0) == TRUE) {
                for (j in seq_len(length(srs[[i]]))) {
                  switch(match.arg(type), und = nrs <- append(nrs, 
                    strsplit(srs[[i]][j], rs$sep)[[1]][1]), inn = nrs <- append(nrs, 
                    strsplit(srs[[i]][j], rs$sep)[[1]][1]), out = nrs <- append(nrs, 
                    (strsplit(srs[[i]][j], rs$sep)[[1]][2])))
                  switch(match.arg(type), und = nrs <- append(nrs, 
                    strsplit(srs[[i]][j], rs$sep)[[1]][2]), inn = NA, 
                    out = NA)
                }
                rm(j)
            }
        }
        rm(i)
        nb <- levels(factor(nrs))
        if (num) {
            return(which(rs$nodes %in% nb[which(!(nb %in% x))]))
        }
        else {
            return(nb[which(!(nb %in% x))])
        }
    }
    else {
        warning(paste(sQuote(deparse(substitute(x))), " is not part of ", 
            sQuote(deparse(substitute(rs))), ".", sep = ""), 
            call. = FALSE)
        x
    }
}

