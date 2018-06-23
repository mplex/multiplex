fact <-
function (S, P, uniq = TRUE, atoms, mc, atmc, patm, K = 3) 
{
    if (isTRUE("symbolic" %in% attr(S, "class")) == TRUE || isTRUE("Semigroup" %in% 
        attr(S, "class")) == FALSE) {
        S <- as.semigroup(S, numerical = TRUE)
        rownames(P) <- colnames(P) <- S$st
    }
    else {
        NA
    }
    NS <- S$ord
    TAB <- as.matrix(S$S)
    ifelse(missing(atoms) == FALSE && isTRUE(atoms == FALSE) == 
        TRUE, atoms <- FALSE, atoms <- TRUE)
    ifelse(missing(mc) == FALSE && isTRUE(mc == FALSE) == TRUE, 
        mc <- FALSE, mc <- TRUE)
    ifelse(missing(atmc) == FALSE && isTRUE(atmc == TRUE) == 
        TRUE, atmc <- TRUE, atmc <- FALSE)
    ifelse(missing(patm) == FALSE && isTRUE(patm == TRUE) == 
        TRUE, patm <- TRUE, patm <- FALSE)
    gi <- transf(1 - P, type = "tolist", sep = ", ")
    iin <- list()
    length(iin) <- length(gi)
    attr(iin, "names") <- gi
    ncomp <- vector()
    for (z in seq_along(gi)) {
        Q <- P[1:NS, 1:NS]
        rownames(Q) <- colnames(Q) <- seq_len(NS)
        k <- as.numeric(dhc(gi[z], sep = ", ")[1])
        l <- as.numeric(dhc(gi[z], sep = ", ")[2])
        NCOMP <- 0
        COMP <- data.frame(matrix(nrow = 0L, ncol = 2L))
        ifelse(all(Q == dichot(Q %*% Q)) == TRUE, NA, Q <- dichot(Q %*% 
            Q))
        for (m in seq_len(NS)) {
            if (isTRUE(Q[TAB[k, m], TAB[l, m]] == 1) == TRUE) {
                NA
            }
            else {
                Q[TAB[k, m], TAB[l, m]] <- 1
                NCOMP <- NCOMP + 1
                COMP[NCOMP, 1] <- TAB[k, m]
                COMP[NCOMP, 2] <- TAB[l, m]
            }
            if (isTRUE(Q[TAB[m, k], TAB[m, l]] == 1) == TRUE) {
                NA
            }
            else {
                Q[TAB[m, k], TAB[m, l]] <- 1
                NCOMP <- NCOMP + 1
                COMP[NCOMP, 1] <- TAB[m, k]
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
            print("Q HAS THE SUBSTITUTION PROPERTY SO RETURN.")
        }
        vec <- vector()
        for (i in 1:nrow(COMP)) {
            vec <- append(vec, jnt(as.character(COMP[i, ]), unique = T))
        }
        rm(i)
        PQ <- replace(P + Q, P + Q == 2, 0)
        diag(PQ) <- 0
        iin[[z]] <- sort(unique(c(vec, transf(PQ, type = "tolist", 
            sep = ", "))))
        ncomp <- append(ncomp, length(iin[[z]]))
    }
    rm(z)
    Iin <- iin
    Ncomp <- ncomp
    iin <- unique(Iin)
    attr(iin, "names") <- attr(Iin, "names")[which(duplicated(Iin) == 
        FALSE)]
    ncomp <- Ncomp[which(duplicated(Iin) == FALSE)]
    if (isTRUE(atoms == TRUE) == TRUE) {
        patms <- list()
        for (k in seq_len(K)) {
            patms[[length(patms) + 1]] <- iin[which(ncomp == 
                which(tabulate(ncomp) != 0)[k])]
        }
        rm(k)
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
        atm <- iin[patoms[which(apply(popatoms, 2, sum) == 1)]]
    }
    if (isTRUE(mc == TRUE) == TRUE) {
        vatm <- patoms[which(apply(popatoms, 2, sum) == 1)]
        mcl <- list()
        iimc <- list()
        length(iimc) <- length(mcl) <- length(vatm)
        for (k in seq_along(vatm)) {
            vec <- vector()
            for (i in seq_along(iin)) {
                ifelse(all(iin[[vatm[k]]] %in% iin[[i]]) == FALSE, 
                  vec <- append(vec, i), NA)
            }
            rm(i)
            mcl[[k]] <- vec
            iimc[[k]] <- iin[mcl[[k]]]
        }
        rm(k)
        attr(mcl, "names") <- attr(atm, "names")
        pomcl <- list()
        length(pomcl) <- length(iimc)
        for (k in seq_along(iimc)) {
            potmp <- matrix(0L, nrow = length(iimc[[k]]), ncol = length(iimc[[k]]), 
                dimnames = list(mcl[[k]], mcl[[k]]))
            for (j in seq_along(iimc[[k]])) {
                for (i in seq_along(iimc[[k]])) {
                  ifelse(isTRUE(all(iimc[[k]][[i]] %in% iimc[[k]][[j]])) == 
                    TRUE, potmp[i, j] <- 1L, NA)
                }
                rm(i)
            }
            rm(j)
            pomcl[[k]] <- potmp
        }
        rm(k)
        pmcl <- list()
        length(pmcl) <- length(pomcl)
        for (k in seq_along(pomcl)) {
            pmcl[[k]] <- which(apply(pomcl[[k]], 1, sum) == 1)
            mcl[[k]][which(apply(pomcl[[k]], 1, sum) == 1)]
        }
        rm(k)
        niin <- length(iin)
        mmcs <- list()
        length(mmcs) <- length(pmcl)
        for (k in seq_along(pmcl)) {
            if (isTRUE(length(pmcl[[k]]) == 1) == TRUE) {
                mmcs[[k]] <- transf(iin[[as.numeric(attr(pmcl[[k]], 
                  "names"))]], type = "toarray", ord = NS, lbs = 1:NS) + 
                  P[1:NS, 1:NS]
            }
            else {
                mmcs[[k]] <- transf(unique(unlist(iin[as.numeric(attr(pmcl[[k]], 
                  "names"))])), type = "toarray", ord = NS, lbs = 1:NS) + 
                  P[1:NS, 1:NS]
                niin <- niin + 1
                mcl[[k]] <- append(mcl[[k]], (niin))
            }
        }
        rm(k)
    }
    if (isTRUE(uniq == TRUE) == TRUE) {
        Lst <- list(iin = iin, niin = ncomp, patm = patoms, atm = atm, 
            atmc = mcl, mc = mmcs)
    }
    else {
        Lst <- list(iin = Iin, niin = Ncomp, patm = patoms, atm = atm, 
            atmc = mcl, mc = mmcs)
    }
    nvc <- c(1L, 2L)
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
    if (isTRUE(mc == TRUE) == TRUE) {
        nvc <- append(nvc, vc)
    }
    else {
        NA
    }
    lst <- Lst[nvc]
    rm(vc, nvc, Lst)
    class(lst) <- "Ind.incl"
    return(lst)
}
