diagram.levels <-
function (x, perm = FALSE) 
{
    po <- x & (1L - t(x))
    diag(po) <- 0L
    for (i in seq_len(ncol(po))) {
        tmp <- outer(po[, i], po[i, ], pmin.int)
        po <- pmin(po, (1L - tmp))
    }
    if (isTRUE(sum(po) > 0L) == TRUE) {
        if (requireNamespace("Rgraphviz", quietly = TRUE)) {
            suppressWarnings(grDevices::pictex())
            X <- Rgraphviz::plot(methods::as(po, "graphNEL"))
            alt <- vector()
            nam <- vector()
            for (i in 1:length(X@AgNode)) {
                alt[length(alt) + 1L] <- X@AgNode[[i]]@center@y
                nam[length(nam) + 1L] <- X@AgNode[[i]]@name
            }
            rm(i)
            cls <- (rbind(nam, rep(0, length(nam))))
            for (i in 1:nlevels(factor(alt))) cls[2, ][which(alt == 
                levels(factor(alt))[i])] <- i
            ord <- vector()
            for (i in 1:length(X@AgEdge)) ord <- append(ord, 
                c(X@AgEdge[[i]]@head, X@AgEdge[[i]]@tail))
            cls[2, ][which(!(cls[1, ] %in% (unique(ord))))] <- as.numeric(max(levels(factor(cls[2, 
                ])))) + 1L
            attr(cls, "dimnames") <- NULL
            colnames(cls) <- cls[2, ]
            cls <- as.data.frame(cls)
            grDevices::dev.off()
            unlink("Rplots.tex")
        }
        else {
            stop("Package \"Rgraphviz\" is required.")
        }
    }
    else {
        cls <- as.data.frame(rbind(as.vector(dimnames(x)[[1]]), 
            rep(1L, ncol(x))))
        attr(cls, "dimnames") <- NULL
        colnames(cls) <- rep(1L, ncol(x))
    }
    if (perm) {
        clu <- as.numeric(as.vector(unlist(cls[2, ])))
        return(list(cls = cls[1, ], clu = clu, perm = perm(x, 
            clu = clu)))
    }
    else {
        dgl <- cls[1, ]
        ulv <- unique(attr(as.list(dgl), "names"))
        lulv <- vector("list", length = length(ulv))
        for (k in ulv) {
            lulv[[which(ulv %in% k)]] <- unlist(as.list(dgl)[which(attr(as.list(dgl), 
                "names") == k)], use.names = FALSE)
        }
        rm(k)
        attr(lulv, "names") <- ulv
        return(lulv)
    }
}
