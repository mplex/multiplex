bnd <-
function (x, xd, lbs, TRD, r, m, mlt, sep) 
{
    DF <- data.frame(matrix(ncol = 2L, nrow = length(m)))
    DF[, 1] <- dhc(m, sep = sep)[which(1:(length(m) * 2L)%%2L == 
        1L)]
    DF[, 2] <- dhc(m, sep = sep)[which(1:(length(m) * 2L)%%2L == 
        0L)]
    DF <- DF[which(DF[, 1] != DF[, 2]), ]
    out <- list()
    inn <- list()
    All <- list()
    for (i in seq(lbs)) {
        out[[i]] <- as.numeric(DF[which(DF[, 1] == as.numeric(lbs[i])), 
            2])
        inn[[i]] <- as.numeric(DF[which(DF[, 2] == as.numeric(lbs[i])), 
            1])
        All[[i]] <- c(out[[i]], inn[[i]])
    }
    rm(i)
    finn <- list()
    fout <- list()
    for (i in seq(lbs)) {
        finn[[i]] <- which(tabulate(inn[[i]]) == dim(x)[3])
        fout[[i]] <- which(tabulate(out[[i]]) == dim(x)[3])
    }
    rm(i)
    full <- list()
    for (i in seq(lbs)) {
        full[[i]] <- intersect(fout[[i]], finn[[i]])
    }
    rm(i)
    rm(finn, fout)
    asym <- list()
    for (i in seq(lbs)) {
        asym[[i]] <- which(tabulate(All[[i]]) == 1L)
    }
    rm(i)
    sout <- list()
    sinn <- list()
    for (i in seq(lbs)) {
        sout[[i]] <- which(tabulate(out[[i]]) == 1L)[which(!(which(tabulate(out[[i]]) == 
            1L) %in% asym[[i]]))]
        sinn[[i]] <- which(tabulate(inn[[i]]) == 1L)[which(!(which(tabulate(inn[[i]]) == 
            1L) %in% asym[[i]]))]
    }
    rm(i)
    dobl <- list()
    dout <- list()
    dinn <- list()
    for (i in seq(lbs)) {
        dobl[[i]] <- which(tabulate(All[[i]]) == 2L)
        dout[[i]] <- which(tabulate(out[[i]]) == 2L)
        dinn[[i]] <- which(tabulate(inn[[i]]) == 2L)
    }
    rm(i)
    if ((is.na(dim(x)[3]) == FALSE | isTRUE(dim(x)[3] == 1) == 
        TRUE)) {
        Eout <- list()
        length(Eout) <- length(lbs)
        Einn <- list()
        length(Einn) <- length(lbs)
        TEnt <- list()
        length(TEnt) <- length(lbs)
        for (i in seq(length(dobl))) {
            tmpout <- vector()
            tmpinn <- vector()
            for (j in seq(length(dobl[[i]]))) {
                if (isTRUE(dobl[[i]][j] %in% dout[[i]]) == TRUE) 
                  tmpout[length(tmpout) + 1L] <- dobl[[i]][j]
                if (isTRUE(dobl[[i]][j] %in% dinn[[i]]) == TRUE) 
                  tmpinn[length(tmpinn) + 1L] <- dobl[[i]][j]
            }
            rm(j)
            Eout[[i]] <- as.integer(tmpout)
            Einn[[i]] <- tmpinn
            TEnt[[i]] <- c(tmpout, tmpinn)
        }
        rm(i)
    }
    else {
        TEnt <- Eout <- Einn <- character(0)
    }
    trip <- list()
    trin <- list()
    trou <- list()
    for (i in seq(lbs)) {
        trip[[i]] <- which(tabulate(All[[i]]) > 2L)
        trin[[i]] <- which(tabulate(inn[[i]]) > 2L)
        trou[[i]] <- which(tabulate(out[[i]]) > 2L)
    }
    rm(i)
    tripfl <- list()
    trinfl <- list()
    troufl <- list()
    for (i in seq(lbs)) {
        tripfl[[i]] <- trip[[i]][which(!(trip[[i]] %in% full[[i]]))]
        trinfl[[i]] <- trin[[i]][which(!(trin[[i]] %in% full[[i]]))]
        troufl[[i]] <- trou[[i]][which(!(trou[[i]] %in% full[[i]]))]
    }
    rm(i)
    sngl <- list()
    for (i in seq(lbs)) {
        sngl[[i]] <- sout[[i]][which(!(sout[[i]] %in% tripfl[[i]]))]
    }
    rm(i)
    recp <- list()
    for (i in seq(lbs)) {
        tmprcp <- vector()
        for (j in seq(length(sngl[[i]]))) {
            chk <- paste(sngl[[i]][j], i, sep = sep)
            if (isTRUE(TRD == TRUE) == TRUE) {
                for (k in 1:dim(x)[3]) {
                  ifelse(isTRUE(all(c(chk, swp(chk, sep = sep)) %in% 
                    mlt[[k]])) == TRUE, tmprcp <- append(tmprcp, 
                    sngl[[i]][j]), NA)
                }
                rm(k)
            }
            else {
                ifelse(isTRUE(all(c(chk, swp(chk, sep = sep)) %in% 
                  m)) == TRUE, tmprcp <- append(tmprcp, sngl[[i]][j]), 
                  NA)
            }
        }
        rm(j)
        recp[[i]] <- as.integer(tmprcp)
    }
    rm(i)
    if (isTRUE(TRD == TRUE) == TRUE) {
        xchg <- list()
        for (i in seq(lbs)) {
            xchg[[i]] <- sngl[[i]][which(!(sngl[[i]] %in% recp[[i]]))]
        }
        rm(i)
    }
    else {
        xchg <- character(0)
    }
    if (isTRUE(TRD == TRUE) == TRUE) {
        Eout3p <- list()
        Einn3p <- list()
        TEnt3p <- list()
        for (i in seq(length(tripfl))) {
            tmpout <- vector()
            tmpinn <- vector()
            for (j in seq(length(tripfl[[i]]))) {
                if (isTRUE(tripfl[[i]][j] %in% trou[[i]]) == 
                  TRUE) 
                  tmpout[length(tmpout) + 1L] <- tripfl[[i]][j]
                if (isTRUE(tripfl[[i]][j] %in% trin[[i]]) == 
                  TRUE) 
                  tmpinn[length(tmpinn) + 1L] <- tripfl[[i]][j]
            }
            rm(j)
            Eout3p[[i]] <- tmpout
            Einn3p[[i]] <- tmpinn
            TEnt3p[[i]] <- as.integer(c(tmpout, tmpinn))
        }
        TEinn <- list()
        TEout <- list()
        for (i in seq(length(TEnt3p))) {
            tmpinn <- vector()
            tmpout <- vector()
            for (j in seq(length(TEnt3p[[i]]))) {
                if (isTRUE(length(Eout3p[[i]]) > 0) == TRUE) {
                  if (isTRUE(!(Eout3p[[i]][j] %in% inn[[i]])) == 
                    TRUE) 
                    tmpout <- append(tmpout, Eout3p[[i]][j])
                }
                if (isTRUE(length(Einn3p[[i]]) > 0) == TRUE) {
                  if (isTRUE(!(Einn3p[[i]][j] %in% out[[i]])) == 
                    TRUE) 
                    tmpinn <- append(tmpinn, Einn3p[[i]][j])
                }
            }
            rm(j)
            TEinn[[i]] <- tmpinn
            TEout[[i]] <- tmpout
        }
        rm(i)
        if (isTRUE(all.equal(TEnt3p, tripfl) == TRUE) == TRUE && 
            isTRUE(length(unlist(tripfl)) > 0) == TRUE) {
            mixe <- as.list(integer(length = length(lbs)))
        }
        else {
            mixe <- list()
            for (i in seq(lbs)) {
                mixe[[i]] <- tripfl[[i]][which(!(tripfl[[i]] %in% 
                  c(TEinn[[i]], TEout[[i]])))]
            }
            rm(i)
        }
        Eoutnp <- list()
        for (i in seq(lbs)) {
            Eoutnp[[i]] <- unique(c(Eout[[i]], Eout3p[[i]]))
            Eout[[i]] <- Eoutnp[[i]][which(!(Eoutnp[[i]] %in% 
                mixe[[i]]))]
        }
        rm(i)
    }
    else {
        mixe <- TEinn <- TEout <- character(0)
    }
    return(list(asym = asym, recp = recp, Eout = Eout, xchg = xchg, 
        mixe = mixe, full = full, out = out, tripfl = tripfl))
}
