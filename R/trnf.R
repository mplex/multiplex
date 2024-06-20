trnf <-
function (x, lbs = NULL, lb2lb = FALSE, tolist = FALSE, ord, 
    sep) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (isTRUE(tolist == TRUE) == TRUE) {
        ifelse(is.na(dim(x)[3]) == FALSE, return(rel.sys(x, type = "tolist", 
            sep = sep, loops = TRUE)$Ties), NA)
        if (isTRUE(sum(x) > 0L) == FALSE) 
            return(paste(0, 0, sep = sep))
        if (isTRUE(is.matrix(x) == TRUE) == FALSE) 
            x <- as.matrix(x)
        if (is.null(lbs) == FALSE | isTRUE(lb2lb == TRUE) == 
            FALSE) {
            lbsr <- lbsc <- lbs
        }
        else {
            lbsr <- dimnames(x)[[1]]
            lbsc <- dimnames(x)[[2]]
        }
        rws <- vector()
        cls <- vector()
        inc <- list()
        for (l in seq_len(max(x))) {
            xd <- dichot(x, c = l)
            for (i in seq_len(length(which((xd) == 1L)))) {
                cls[i] <- (ceiling(which((xd) == 1L)/dim(x)[1]))[i]
                ifelse((which((xd) == 1L)%%dim(x)[1])[i] == 0L, 
                  rws[i] <- (which((xd) == 1L)%%dim(x)[1])[i] + 
                    dim(x)[1], rws[i] <- (which((xd) == 1L)%%dim(x)[1])[i])
                ifelse(isTRUE(lb2lb == TRUE) == TRUE, inc[[length(inc) + 
                  1L]] <- paste(lbsr[rws[i]], lbsc[cls[i]], sep = sep), 
                  inc[[length(inc) + 1L]] <- paste(rws[i], cls[i], 
                    sep = sep))
            }
            rm(i)
        }
        rm(l)
        return(sort(unlist(inc)))
    }
    else {
        ifelse(is.array(x) == TRUE, return(x), NA)
        ifelse(is.null(lbs) == TRUE, lbs <- levels(factor(dhc(x, 
            sep = sep))), lbs <- lbs)
        if (missing(ord) == TRUE && is.array(x) == FALSE) {
            ord <- length(dhc(jnt(x, sep = sep), sep = sep))
        }
        else {
            ord <- as.numeric(ord)
            if (isTRUE(nlevels(factor(dhc(unlist(x), sep = sep))) > 
                ord) == TRUE) {
                ord <- nlevels(factor(dhc(unlist(x), sep = sep)))
                warning("Argument \"ord\" is ignored since is less than the number of factor levels in the pairwise list.")
            }
            else {
                NA
            }
        }
        lbs <- lbs[seq_len(ord)]
        mat <- matrix(0L, ncol = ord, nrow = ord, dimnames = list(lbs, 
            lbs))
        for (i in seq_len(length(x))) {
            mat[which(lbs == dhc(x[i], sep = sep)[1]), which(lbs == 
                dhc(x[i], sep = sep)[2])] <- mat[which(lbs == 
                dhc(x[i], sep = sep)[1]), which(lbs == dhc(x[i], 
                sep = sep)[2])] + 1L
        }
        rm(i)
        return(mat)
    }
}
