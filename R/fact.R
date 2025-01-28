fact <-
function (S, P, uniq = TRUE, fac, atoms, mca, atmc, patm, k) 
{
    if (isTRUE("Decomp" %in% attr(S, "class")) == TRUE) {
        if (missing(fac) == TRUE || missing(fac) == FALSE && 
            (is.numeric(fac) == FALSE || isTRUE(fac > length(S$IM)) == 
                TRUE)) {
            warning("First factor is taken since \"fac\" must be an integer no larger than the number of factors in the input.")
            s <- S$IM[[1]]
            po <- S$PO[[1]]
        }
        else if (missing(fac) == FALSE) {
            s <- S$IM[[fac]]
            po <- S$PO[[fac]]
        }
        S <- s
        P <- po
        rm(s, po)
    }
    else {
        NA
    }
    if (isTRUE("symbolic" %in% attr(S, "class")) == TRUE || isTRUE("Semigroup" %in% 
        attr(S, "class")) == FALSE) {
        ifelse(is.list(S) == TRUE && isTRUE(length(S) == 1L) == 
            TRUE, S <- S[[1]], NA)
        S <- as.semigroup(S, numerical = TRUE)
        ifelse(is.list(P) == TRUE && isTRUE(length(P) == 1L) == 
            TRUE, P <- P[[1]], NA)
        rownames(P) <- colnames(P) <- S$st
    }
    else {
        NA
    }
    NS <- S$ord
    TAB <- as.matrix(S$S)
    if (isTRUE(length(unique(as.vector(TAB))) == 1) == TRUE) {
        return(list(po = P[1:NS, 1:NS], iin = transf(1 - P, type = "tolist", 
            lb2lb = TRUE), Note = "1-element semigroup found."))
    }
    ifelse(missing(atoms) == FALSE && isTRUE(atoms == FALSE) == 
        TRUE, atoms <- FALSE, atoms <- TRUE)
    ifelse(missing(mca) == FALSE && isTRUE(mca == FALSE) == TRUE, 
        mca <- FALSE, mca <- TRUE)
    ifelse(missing(atmc) == FALSE && isTRUE(atmc == TRUE) == 
        TRUE, atmc <- TRUE, atmc <- FALSE)
    ifelse(missing(patm) == FALSE && isTRUE(patm == TRUE) == 
        TRUE, patm <- TRUE, patm <- FALSE)
    gi <- transf(1 - P, type = "tolist", sep = ", ")
    iin <- list()
    length(iin) <- length(gi)
    attr(iin, "names") <- gi
    ncomp <- vector()
    znote <- vector()
    for (z in seq_along(gi)) {
        Q <- P[1:NS, 1:NS]
        rownames(Q) <- colnames(Q) <- seq_len(NS)
        K <- as.numeric(dhc(gi[z], sep = ", ")[1])
        l <- as.numeric(dhc(gi[z], sep = ", ")[2])
        NCOMP <- 0
        COMP <- data.frame(matrix(nrow = 0L, ncol = 2L))
        ifelse(all(Q == dichot(Q %*% Q)) == TRUE, NA, Q <- dichot(Q %*% 
            Q))
        for (m in seq_len(NS)) {
            if (isTRUE(Q[TAB[K, m], TAB[l, m]] == 1) == TRUE) {
                NA
            }
            else {
                Q[TAB[K, m], TAB[l, m]] <- 1
                NCOMP <- NCOMP + 1
                COMP[NCOMP, 1] <- TAB[K, m]
                COMP[NCOMP, 2] <- TAB[l, m]
            }
            if (isTRUE(Q[TAB[m, K], TAB[m, l]] == 1) == TRUE) {
                NA
            }
            else {
                Q[TAB[m, K], TAB[m, l]] <- 1
                NCOMP <- NCOMP + 1
                COMP[NCOMP, 1] <- TAB[m, K]
                COMP[NCOMP, 2] <- TAB[m, l]
            }
        }
        rm(m)
        if (isTRUE(NCOMP != 0) == TRUE) {
            NMIN <- 1
            NMAX <- NCOMP
            for (m in NMIN:NMAX) {
                for (n in seq_len(NS)) {
                  if (isTRUE(Q[TAB[COMP[m, 1], n], TAB[COMP[m, 
                    2], n]] == 1) == TRUE) {
                    NA
                  }
                  else {
                    Q[TAB[COMP[m, 1], n], TAB[COMP[m, 2], n]] <- 1
                    NCOMP <- NCOMP + 1
                    COMP[NCOMP, 1] <- TAB[COMP[m, 1], n]
                    COMP[NCOMP, 2] <- TAB[COMP[m, 2], n]
                  }
                  if (isTRUE(Q[TAB[n, COMP[m, 1]], TAB[n, COMP[m, 
                    2]]] == 1) == TRUE) {
                    NA
                  }
                  else {
                    Q[TAB[n, COMP[m, 1]], TAB[n, COMP[m, 2]]] <- 1
                    NCOMP <- NCOMP + 1
                    COMP[NCOMP, 1] <- TAB[n, COMP[m, 1]]
                    COMP[NCOMP, 2] <- TAB[n, COMP[m, 2]]
                  }
                  if (isTRUE(NCOMP == NMAX) == TRUE) {
                    if (all(Q == dichot(Q %*% Q)) == TRUE) {
                      NA
                    }
                    else {
                      ifelse(all(Q == dichot(Q %*% Q)) == TRUE, 
                        NA, Q <- (dichot(Q %*% Q)))
                    }
                  }
                  else {
                    NMIN <- NMIN + 1
                    NMAX <- NCOMP
                  }
                }
                rm(n)
            }
            rm(m)
        }
        else {
            znote <- append(znote, z)
        }
        if (isTRUE(nrow(COMP) != 0) == TRUE) {
            vec <- vector()
            for (i in seq_len(nrow(COMP))) {
                vec <- append(vec, jnt(as.character(COMP[i, ]), 
                  unique = TRUE))
            }
            rm(i)
            PQ <- replace(P + Q, P + Q == 2, 0)
            diag(PQ) <- 0
            iin[[z]] <- sort(unique(c(vec, transf(PQ, type = "tolist", 
                sep = ", "))))
        }
        else {
        }
        ncomp <- append(ncomp, length(iin[[z]]))
    }
    rm(z)
    Iin <- iin[lengths(iin) != 0]
    Ncomp <- ncomp[ncomp != 0]
    iin <- unique(Iin)
    attr(iin, "names") <- attr(Iin, "names")[which(duplicated(Iin) == 
        FALSE)]
    ncomp <- Ncomp[which(duplicated(Iin) == FALSE)]
    if (isTRUE(atoms == TRUE) == TRUE) {
        ifelse(missing(k) == TRUE, k <- 3, NA)
        patms <- list()
        for (i in seq_len(k)) {
            patms[[length(patms) + 1]] <- iin[which(ncomp == 
                which(tabulate(ncomp) != 0)[i])]
        }
        rm(i)
        vecn <- vector()
        for (i in seq_along(patms)) {
            vecn <- append(vecn, attr(patms[[i]], "names"))
        }
        rm(i)
        patoms <- which(attr(iin, "names") %in% vecn)
        pii <- array(dim = c(nrow(P), ncol(P), length(patoms)))
        for (i in seq_along(iin[patoms])) {
            pii[, , i] <- transf(iin[patoms][[i]], type = "toarray", 
                ord = nrow(P), lbs = seq_len(nrow(P))) + P
        }
        rm(i)
        dimnames(pii)[[1]] <- dimnames(pii)[[2]] <- seq_len(nrow(P))
        dimnames(pii)[[3]] <- patoms
        popatoms <- strng(pii)
        atm <- iin[patoms[which(apply(popatoms, 2, sum) == 1L)]]
    }
    if (isTRUE(mca == TRUE) == TRUE) {
        vatm <- patoms[which(apply(popatoms, 2, sum) == 1)]
        mcl <- list()
        iimc <- list()
        length(iimc) <- length(mcl) <- length(vatm)
        for (K in seq_along(vatm)) {
            vec <- vector()
            for (i in seq_along(iin)) {
                ifelse(all(iin[[vatm[K]]] %in% iin[[i]]) == FALSE, 
                  vec <- append(vec, i), NA)
            }
            rm(i)
            mcl[[K]] <- vec
            iimc[[K]] <- iin[mcl[[K]]]
        }
        rm(K)
        attr(mcl, "names") <- attr(atm, "names")
        pomcl <- list()
        length(pomcl) <- length(iimc)
        for (K in seq_along(iimc)) {
            potmp <- matrix(0L, nrow = length(iimc[[K]]), ncol = length(iimc[[K]]), 
                dimnames = list(mcl[[K]], mcl[[K]]))
            for (j in seq_along(iimc[[K]])) {
                for (i in seq_along(iimc[[K]])) {
                  ifelse(isTRUE(all(iimc[[K]][[i]] %in% iimc[[K]][[j]])) == 
                    TRUE, potmp[i, j] <- 1L, NA)
                }
                rm(i)
            }
            rm(j)
            pomcl[[K]] <- potmp
        }
        rm(K)
        pmcl <- list()
        length(pmcl) <- length(pomcl)
        for (K in seq_along(pomcl)) {
            pmcl[[K]] <- which(apply(pomcl[[K]], 1, sum) == 1)
            mcl[[K]][which(apply(pomcl[[K]], 1, sum) == 1)]
        }
        rm(K)
        niin <- length(iin)
        mmcs <- list()
        length(mmcs) <- length(pmcl)
        for (K in seq_along(pmcl)) {
            if (isTRUE(length(pmcl[[K]]) == 1) == TRUE) {
                mmcs[[K]] <- transf(iin[[as.numeric(attr(pmcl[[K]], 
                  "names"))]], type = "toarray", ord = NS, lbs = 1:NS) + 
                  P[1:NS, 1:NS]
            }
            else {
                mmcs[[K]] <- transf(unique(unlist(iin[as.numeric(attr(pmcl[[K]], 
                  "names"))])), type = "toarray", ord = NS, lbs = 1:NS) + 
                  P[1:NS, 1:NS]
                niin <- niin + 1L
                mcl[[K]] <- append(mcl[[K]], (niin))
            }
        }
        rm(K)
    }
    if (isTRUE(uniq == TRUE) == TRUE) {
        Lst <- list(po = P[1:NS, 1:NS], iin = iin, niin = ncomp, 
            patm = patoms, atm = atm, atmc = mcl, mca = mmcs, 
            note = znote)
    }
    else {
        Lst <- list(po = P[1:NS, 1:NS], iin = Iin, niin = Ncomp, 
            patm = patoms, atm = atm, atmc = mcl, mca = mmcs, 
            note = znote)
    }
    nvc <- c(1L, 2L, 3L)
    attr(Lst, "names")[length(Lst)] <- "Note: no substitution property in induced inclusions"
    vc <- max(nvc) + 1L
    if (isTRUE(patm == TRUE) == TRUE) {
        nvc <- append(nvc, vc)
    }
    else {
        NA
    }
    vc <- vc + 1L
    if (isTRUE(atoms == TRUE) == TRUE) {
        nvc <- append(nvc, vc)
    }
    else {
        NA
    }
    vc <- vc + 1L
    if (isTRUE(atmc == TRUE) == TRUE) {
        nvc <- append(nvc, vc)
    }
    else {
        NA
    }
    vc <- vc + 1L
    if (isTRUE(mca == TRUE) == TRUE) {
        nvc <- append(nvc, vc)
    }
    else {
        NA
    }
    if (isTRUE(length(znote) != 0) == TRUE) {
        nvc <- append(nvc, length(Lst))
    }
    else {
        NA
    }
    lst <- Lst[nvc]
    rm(vc, nvc, Lst)
    class(lst) <- "Ind.incl"
    return(lst)
}
