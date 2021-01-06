galois <-
function (x, labeling = c("full", "reduced"), sep, valued, scl, 
    sep2) 
{
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    if (missing(valued) == FALSE && isTRUE(valued == TRUE) == 
        TRUE) {
        ifelse(missing(sep2) == TRUE, sep2 <- "_", NA)
        x <- cscl(x, scl = scl, sep = sep2)
    }
    else {
        NA
    }
    if (is.data.frame(x) == FALSE) {
        if (is.vector(x) == TRUE) {
            x <- t(as.data.frame(x))
        }
        else if (is.array(x) == TRUE) {
            x <- as.data.frame(x)
        }
        else {
            stop("Data in \"x\" must be an array or a vector")
        }
    }
    if ("f" %in% colnames(x) == TRUE) {
        cnx <- colnames(x)
        X <- x
        colnames(X)[which(colnames(X) == "f")] <- ""
        wdx <- which(duplicated(X))
        flgf <- TRUE
    }
    else {
        wdx <- which(duplicated(x))
        flgf <- FALSE
    }
    eq0 <- list()
    if (isTRUE(length(wdx) > 1L) == TRUE) {
        for (i in wdx) {
            tmp <- vector()
            for (j in seq_len(nrow(x))) {
                if (isTRUE(all(x[i, ] == x[j, ]) == TRUE) == 
                  TRUE) {
                  tmp <- append(tmp, rownames(x)[j])
                }
                else {
                  NA
                }
            }
            rm(j)
            eq0[[length(eq0) + 1L]] <- paste(rownames(x)[i], 
                jnt(tmp, sep = sep), sep = sep)
        }
        rm(i)
    }
    else if (isTRUE(length(wdx) == 1L) == TRUE) {
        tmp <- vector()
        for (j in seq_len(nrow(x))) {
            ifelse(isTRUE(all(x[wdx, ] == x[j, ]) == TRUE) == 
                TRUE, tmp <- append(tmp, rownames(x)[j]), NA)
        }
        rm(j)
        eq0[[length(eq0) + 1L]] <- paste(rownames(x)[wdx], jnt(tmp, 
            sep = sep), sep = sep)
    }
    else {
        NA
    }
    ifelse(isTRUE(flgf == TRUE) == TRUE, colnames(x) <- cnx, 
        NA)
    if (isTRUE(length(eq0) > 0L) == TRUE) {
        for (i in seq_len(length(eq0))) {
            eq0[[i]] <- jnt(dhc(eq0[[i]], sep = sep), sep = sep)
        }
        rm(i)
        eq0 <- unique(eq0)
        x <- unique(x)
    }
    else {
        NA
    }
    eq1 <- list()
    for (i in which(duplicated(t(x)))) {
        tmp <- vector()
        for (j in seq_len(ncol(x))) {
            if (isTRUE(all(x[, i] == x[, j]) == TRUE) == TRUE) {
                tmp <- append(tmp, colnames(x)[j])
            }
            else {
                NA
            }
        }
        rm(j)
        eq1[[length(eq1) + 1L]] <- paste(colnames(x)[i], jnt(tmp, 
            sep = sep), sep = sep)
    }
    rm(i)
    if (isTRUE(length(eq1) > 0L) == TRUE) {
        for (i in seq_len(length(eq1))) {
            eq1[[i]] <- jnt(dhc(eq1[[i]], sep = sep), sep = sep)
        }
        rm(i)
        eq1 <- unique(eq1)
        x <- t(unique(t(x)))
        for (k in seq_len(length(eq1))) {
            colnames(x)[which(colnames(x) %in% dhc(eq1[[k]], 
                sep = sep))] <- eq1[[k]]
        }
        rm(k)
    }
    else {
        NA
    }
    conj <- list()
    length(conj) <- ncol(x)
    for (i in seq_len(ncol(x))) {
        conj[[i]] <- jnt(rownames(x)[which(x[, i] != 0L)], sep = sep)
    }
    rm(i)
    attr(conj, "names") <- colnames(x)
    conj1 <- list()
    length(conj1) <- nrow(x)
    for (i in seq_len(nrow(x))) {
        conj1[[i]] <- jnt(colnames(x)[which(x[i, ] != 0L)], sep = sep)
    }
    rm(i)
    attr(conj1, "names") <- rownames(x)
    conj2 <- list()
    conj2n <- vector()
    if (isTRUE(length(conj) > 1L) == TRUE) {
        for (k in 2:length(conj)) {
            for (i in k:length(conj)) {
                if (isTRUE(length(intersect(dhc(conj[[k - 1]], 
                  sep = sep), dhc(conj[[i]], sep = sep))) > 1L) == 
                  TRUE) {
                  conj2[[length(conj2) + 1L]] <- jnt(intersect(dhc(conj[[k - 
                    1]], sep = sep), dhc(conj[[i]], sep = sep)), 
                    sep = sep)
                }
                else if (isTRUE(length(intersect(dhc(conj[[k - 
                  1L]], sep = sep), dhc(conj[[i]], sep = sep))) > 
                  1L) == FALSE) {
                  conj2[[length(conj2) + 1L]] <- intersect(dhc(conj[[k - 
                    1L]], sep = sep), dhc(conj[[i]], sep = sep))
                }
                conj2n[length(conj2n) + 1L] <- paste(attr(conj, 
                  "names")[k - 1L], attr(conj, "names")[i], sep = sep)
            }
            rm(i)
        }
        rm(k)
        attr(conj2, "names") <- conj2n
    }
    else {
        conj2 <- conj
    }
    if (isTRUE(length(which(conj2 %in% conj == FALSE)) != 0L) == 
        TRUE) {
        if (isTRUE(jnt(rownames(x), sep = sep) %in% c(conj, conj2)) == 
            TRUE) {
            conj3 <- list(conj, unique(conj2[which(conj2 %in% 
                conj == FALSE)]))
        }
        else if (isTRUE(jnt(rownames(x), sep = sep) %in% c(conj, 
            conj2)) == FALSE) {
            conj3 <- list(conj, unique(conj2[which(conj2 %in% 
                conj == FALSE)]), jnt(rownames(x), sep = sep))
        }
    }
    else if (isTRUE(length(which(conj2 %in% conj == FALSE)) != 
        0L) == FALSE) {
        conj3 <- list(conj, jnt(rownames(x), sep = sep))
    }
    for (i in seq_len(length(conj))) {
        if (isTRUE(length(which(conj2 %in% conj[[i]] == TRUE)) != 
            0L) == TRUE) {
            attr(conj3[[1]], "names")[i] <- jnt(attr(conj2, "names")[which(conj2 %in% 
                conj[[i]])], sep = sep)
        }
        else {
            NA
        }
    }
    rm(i)
    for (i in seq_len(length(conj3[[2]]))) {
        if (isTRUE(length(attr(conj2, "names")[which(conj3[[2]][[i]] == 
            conj2)]) != 0L) == TRUE) {
            if (isTRUE(length(dhc(conj3[[2]][[i]], sep = sep)) == 
                1L) == TRUE) {
                attr(conj3[[2]], "names")[i] <- conj1[[which(conj3[[2]][[i]] == 
                  attr(conj1, "names"))]]
            }
            else if (isTRUE(length(dhc(conj3[[2]][[i]], sep = sep)) == 
                1L) == FALSE) {
                attr(conj3[[2]], "names")[i] <- jnt(attr(conj2, 
                  "names")[which(conj3[[2]][[i]] == conj2)], 
                  sep = sep)
            }
        }
        else {
            attr(conj3[[2]], "names")[i] <- jnt(attr(conj2, "names")[which(conj2 %in% 
                levels(factor(unlist(conj2))))], sep = sep)
        }
    }
    rm(i)
    if (isTRUE(jnt(rownames(x), sep = sep) %in% c(conj, conj2)) == 
        FALSE) {
        ifelse(isTRUE(length(conj3) > 2L) == TRUE, con <- c(conj3[[1]], 
            conj3[[2]], conj3[[3]]), con <- c(conj3[[1]], conj3[[2]]))
    }
    else {
        ifelse(isTRUE(conj3[[2]] %in% conj3[[1]]) == TRUE, con <- conj3[[1]], 
            con <- c(conj3[[1]], conj3[[2]]))
    }
    attr(con, "names")[which(attr(con, "names") == "")] <- NA
    der <- unique(con)
    attr(der, "names") <- unique(stats::na.omit(attr(con, "names")))
    po <- matrix(0L, nrow = length(der), ncol = length(der))
    for (j in seq_len(length(der))) {
        for (i in seq_len(length(der))) {
            ifelse(isTRUE(all(dhc(der[[i]], sep = sep) %in% dhc(der[[j]], 
                sep = sep))) == TRUE, po[i, j] <- 1L, NA)
        }
    }
    rm(i, j)
    ints <- attr(der, "names")
    exts <- der
    rownames(po) <- colnames(po) <- ints
    for (i in (length(conj3[[1]]) + 1L):length(der)) {
        if (isTRUE(length(der) > length(conj3[[1]])) == TRUE) {
            ifelse(isTRUE(length(flt(i, po, rclos = FALSE)) > 
                0L) == TRUE, ints[i] <- jnt(dhc(ints[flt(i, po, 
                rclos = FALSE)], sep = sep), sep = sep), NA)
        }
        else {
            NA
        }
    }
    rm(i)
    attr(der, "names") <- attr(exts, "names") <- ints
    ints[which(is.na(ints))] <- ""
    if (isTRUE(length(der) > 2L) == TRUE) {
        for (k in 2:length(der)) {
            for (i in k:length(der)) {
                if (isTRUE((k - 1L) == i) == FALSE) {
                  if (isTRUE(any(isTRUE(all(po[, i] - po[, (k - 
                    1L)] != -1)) == TRUE || isTRUE(all(po[, (k - 
                    1L)] - po[, i] != -1)) == TRUE)) == TRUE) {
                    if (isTRUE(all(po[, i] - po[, (k - 1L)] != 
                      -1)) == TRUE) {
                      if (isTRUE(ints[(k - 1L)] == "") == FALSE | 
                        isTRUE(which(!(dhc(ints[(k - 1L)], sep = sep) %in% 
                          dhc(ints[i], sep = sep))) > 0) == TRUE) {
                        ifelse(all(dhc(ints[(k - 1L)], sep = sep) %in% 
                          dhc(ints[i], sep = sep)) == TRUE, NA, 
                          ints[(k - 1L)] <- jnt(dhc(ints[(k - 
                            1L)], sep = sep)[which(!(dhc(ints[(k - 
                            1L)], sep = sep) %in% dhc(ints[i], 
                            sep = sep)))], sep = sep))
                      }
                      else {
                        NA
                      }
                      if (isTRUE(length(exts[[i]]) == 0L) == 
                        FALSE) {
                        exts[[i]] <- jnt(dhc(exts[[i]], sep = sep)[which(!(dhc(exts[[i]], 
                          sep = sep) %in% dhc(exts[[(k - 1L)]], 
                          sep = sep)))], sep = sep)
                      }
                      else {
                        NA
                      }
                    }
                    else if (isTRUE(all(po[, i] - po[, (k - 1L)] != 
                      -1)) == FALSE) {
                      if (isTRUE(all.equal(dhc(ints[i], sep = sep), 
                        dhc(ints[(k - 1L)], sep = sep))) == TRUE) {
                        ints[i] <- ""
                      }
                      else if (isTRUE(all.equal(dhc(ints[i], 
                        sep = sep), dhc(ints[(k - 1L)], sep = sep))) == 
                        FALSE) {
                        ifelse(isTRUE(length(jnt(dhc(ints[i], 
                          sep = sep)[which(!(dhc(ints[i], sep = sep) %in% 
                          dhc(ints[(k - 1L)], sep = sep)))], 
                          sep = sep)) == 0L) == TRUE, NA, ints[i] <- jnt(dhc(ints[i], 
                          sep = sep)[which(!(dhc(ints[i], sep = sep) %in% 
                          dhc(ints[(k - 1L)], sep = sep)))], 
                          sep = sep))
                      }
                      if (isTRUE(all.equal(dhc(exts[[(k - 1L)]], 
                        sep = sep), dhc(exts[[i]], sep = sep))) == 
                        TRUE) {
                        exts[[(k - 1L)]] <- ""
                      }
                      else if (isTRUE(all.equal(dhc(exts[[(k - 
                        1L)]], sep = sep), dhc(exts[[i]], sep = sep))) == 
                        FALSE) {
                        exts[[(k - 1L)]] <- jnt(dhc(exts[[(k - 
                          1L)]], sep = sep)[which(!(dhc(exts[[(k - 
                          1L)]], sep = sep) %in% dhc(exts[[i]], 
                          sep = sep)))], sep = sep)
                      }
                    }
                  }
                  else {
                    NA
                  }
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        rm(k)
    }
    else if (isTRUE(length(der) == 2L) == TRUE) {
        if (isTRUE(any(isTRUE(all(po[, 2] - po[, 1] != -1)) == 
            TRUE | isTRUE(all(po[, 1] - po[, 2] != -1)) == TRUE)) == 
            TRUE) {
            if (isTRUE(all(po[, 1] - po[, 2] != -1)) == TRUE) {
                if (isTRUE(is.na(ints[2])) == FALSE) {
                  ints[2] <- jnt(dhc(ints[1], sep = sep)[which(!(dhc(ints[1], 
                    sep = sep) %in% dhc(ints[2], sep = sep)))], 
                    sep = sep)
                }
                else {
                  NA
                }
                if (isTRUE(length(exts[[2]]) == 0L) == FALSE) {
                  exts[[2]] <- jnt(dhc(exts[[2]], sep = sep)[which(!(dhc(exts[[2]], 
                    sep = sep) %in% dhc(exts[[1]], sep = sep)))], 
                    sep = sep)
                }
                else {
                  NA
                }
            }
            else if (isTRUE(any(po[, 2] - po[, 1] != -1)) == 
                TRUE) {
                if (isTRUE(all.equal(dhc(ints[2], sep = sep), 
                  dhc(ints[1], sep = sep))) == TRUE) {
                  ints[2] <- ""
                }
                else if (isTRUE(all.equal(dhc(ints[2], sep = sep), 
                  dhc(ints[1], sep = sep))) == FALSE) {
                  ifelse(isTRUE(length(jnt(dhc(ints[2], sep = sep)[which(!(dhc(ints[2], 
                    sep = sep) %in% dhc(ints[1], sep = sep)))], 
                    sep = sep)) == 0L) == TRUE, NA, ints[2] <- jnt(dhc(ints[2], 
                    sep = sep)[which(!(dhc(ints[2], sep = sep) %in% 
                    dhc(ints[1], sep = sep)))], sep = sep))
                }
                if (isTRUE(all.equal(dhc(exts[[1]], sep = sep), 
                  dhc(exts[[2]], sep = sep))) == TRUE) {
                  exts[[2]] <- ""
                }
                else if (isTRUE(all.equal(dhc(exts[[1]], sep = sep), 
                  dhc(exts[[2]], sep = sep))) == FALSE) {
                  exts[[2]] <- jnt(dhc(exts, sep = sep)[[2]][which(!(dhc(exts[[2]], 
                    sep = sep) %in% dhc(exts[[1]], sep = sep)))], 
                    sep = sep)
                }
            }
        }
        else {
            NA
        }
    }
    else {
        NA
    }
    dupl <- levels(factor(unlist(dhc(exts, sep = sep))[which(duplicated(unlist(dhc(exts, 
        sep = sep))) == TRUE)]))
    dder <- der
    if (isTRUE(length(dupl) > 0L) == TRUE) {
        for (i in seq_len(length(dupl))) dder[[(length(dder) + 
            1L)]] <- dupl[i]
        for (i in seq_len(length(dupl))) {
            for (j in seq_len(nrow(x))) {
                if (isTRUE(any(isTRUE(all(x[j, ] - x[which(rownames(x) == 
                  dupl[i]), ] != -1)) == TRUE & isTRUE(all(x[which(rownames(x) == 
                  dupl[i]), ] - x[j, ] != -1)) == FALSE)) == 
                  TRUE) {
                  dder[[(length(der) + i)]] <- jnt(append(dder[[(length(der) + 
                    i)]], rownames(x)[j]), sep = sep)
                }
                else {
                  NA
                }
            }
            rm(j)
        }
        rm(i)
        dpo <- matrix(0L, nrow = length(dder), ncol = length(dder))
        for (j in seq_len(length(dder))) {
            for (i in seq_len(length(dder))) {
                ifelse(isTRUE(all(dhc(dder[[i]], sep = sep) %in% 
                  dhc(dder[[j]], sep = sep))) == TRUE, dpo[i, 
                  j] <- 1L, NA)
            }
        }
        rm(i, j)
        attr(dder, "names")[which(is.na(attr(dder, "names")) == 
            TRUE)] <- ""
        ints <- attr(dder, "names")
        exts <- dder
        rownames(dpo) <- colnames(dpo) <- ints
        PO <- dpo
        if (isTRUE(length(dder) > length(der)) == TRUE) {
            for (i in (length(der) + 1L):length(dder)) {
                ifelse(isTRUE(length(flt(i, dpo, rclos = FALSE)) > 
                  0L) == TRUE, ints[i] <- jnt(dhc(ints[flt(i, 
                  dpo, rclos = FALSE)], sep = sep), sep = sep), 
                  NA)
            }
            rm(i)
        }
        else {
            NA
        }
        attr(dder, "names") <- ints
        for (k in 2:length(dder)) {
            for (i in k:length(dder)) {
                if (isTRUE((k - 1L) == i) == FALSE) {
                  if (isTRUE(any(isTRUE(all(dpo[, i] - dpo[, 
                    (k - 1L)] != -1)) == TRUE | isTRUE(all(dpo[, 
                    (k - 1L)] - dpo[, i] != -1)) == TRUE)) == 
                    TRUE) {
                    if (isTRUE(all(dpo[, i] - dpo[, (k - 1L)] != 
                      -1)) == TRUE) {
                      if (isTRUE(ints[(k - 1L)] == "") == FALSE) {
                        ifelse(all(dhc(ints[(k - 1L)], sep = sep) %in% 
                          dhc(ints[i], sep = sep)) == TRUE, NA, 
                          ints[(k - 1L)] <- jnt(dhc(ints[(k - 
                            1L)], sep = sep)[which(!(dhc(ints[(k - 
                            1L)], sep = sep) %in% dhc(ints[i], 
                            sep = sep)))], sep = sep))
                      }
                      else {
                        NA
                      }
                      if (isTRUE(length(exts[[i]]) == 0L) == 
                        FALSE) {
                        exts[[i]] <- jnt(dhc(exts[[i]], sep = sep)[which(!(dhc(exts[[i]], 
                          sep = sep) %in% dhc(exts[[(k - 1L)]], 
                          sep = sep)))], sep = sep)
                      }
                      else {
                        NA
                      }
                    }
                    else if (isTRUE(all(dpo[, i] - dpo[, (k - 
                      1L)] != -1)) == FALSE) {
                      if (isTRUE(all.equal(dhc(ints[i], sep = sep), 
                        dhc(ints[(k - 1L)], sep = sep))) == TRUE) {
                        ints[i] <- ""
                      }
                      else if (isTRUE(all.equal(dhc(ints[i], 
                        sep = sep), dhc(ints[(k - 1L)], sep = sep))) == 
                        FALSE) {
                        ifelse(isTRUE(length(jnt(dhc(ints[i], 
                          sep = sep)[which(!(dhc(ints[i], sep = sep) %in% 
                          dhc(ints[(k - 1L)], sep = sep)))], 
                          sep = sep)) == 0L) == TRUE, NA, ints[i] <- jnt(dhc(ints[i], 
                          sep = sep)[which(!(dhc(ints[i], sep = sep) %in% 
                          dhc(ints[(k - 1L)], sep = sep)))], 
                          sep = sep))
                      }
                      if (isTRUE(all.equal(dhc(exts[[(k - 1L)]], 
                        sep = sep), dhc(exts[[i]], sep = sep))) == 
                        TRUE) {
                        exts[[(k - 1L)]] <- ""
                      }
                      else if (isTRUE(all.equal(dhc(exts[[(k - 
                        1L)]], sep = sep), dhc(exts[[i]], sep = sep))) == 
                        FALSE) {
                        exts[[(k - 1L)]] <- jnt(dhc(exts[[(k - 
                          1L)]], sep = sep)[which(!(dhc(exts[[(k - 
                          1L)]], sep = sep) %in% dhc(exts[[i]], 
                          sep = sep)))], sep = sep)
                      }
                    }
                  }
                  else {
                    NA
                  }
                }
                else {
                  NA
                }
            }
            rm(i)
        }
        rm(k)
    }
    else if (isTRUE(length(dupl) > 0L) == FALSE) {
        PO <- po
    }
    if (isTRUE(length(dder) > 1L) == TRUE) {
        ifelse(isTRUE(length(jnt(dhc(ints[length(dder)], sep = sep)[which(!(dhc(ints[length(dder)], 
            sep = sep) %in% dhc(ints[1:(length(dder) - 1L)], 
            sep = sep)))], sep = sep)) == 0) == TRUE, ints[length(dder)] <- "", 
            NA)
    }
    if (isTRUE(length(eq0) > 0L) == TRUE) {
        for (k in seq_len(length(eq0))) {
            cmb <- which(exts %in% dhc(eq0, sep = sep)[[k]])
            if (isTRUE(length(cmb) == 0) == TRUE) {
                cmb <- which(unlist(dhc(exts, sep = sep)) %in% 
                  dhc(eq0, sep = sep)[[k]])
                ifelse(isTRUE(length(cmb) > 0) == TRUE, exts[[cmb]] <- jnt(unique(c(dhc(exts[[cmb]], 
                  sep = sep), dhc(eq0[[k]], sep = sep))), sep = sep), 
                  NA)
            }
            else {
                ifelse(isTRUE(length(cmb) > 0) == TRUE, exts[[cmb]] <- eq0[[k]], 
                  NA)
            }
            for (i in seq_len(length(dder))) {
                ifelse(isTRUE(any(dhc(dder, sep = sep)[[i]] %in% 
                  dhc(eq0, sep = sep)[[k]]) == TRUE) == TRUE, 
                  dder[[i]] <- jnt(c(dhc(dder, sep = sep)[[i]], 
                    dhc(eq0, sep = sep)[[k]]), sep = sep), NA)
            }
            rm(i)
        }
        rm(k, cmb)
    }
    mi <- NULL
    for (i in seq_len(dim(PO)[1])) {
        flt <- flt(i, PO, rclos = TRUE)
        ifelse(isTRUE(length(flt) == dim(PO)[1]) == TRUE, mi <- i, 
            NA)
    }
    rm(i)
    if (isTRUE(jnt(colnames(x), sep = sep) %in% attr(dder, "names")) == 
        FALSE) {
        if (isTRUE(length(mi) == 0L) == TRUE) {
            dder[[length(dder) + 1L]] <- exts[[length(exts) + 
                1L]] <- ints[length(ints) + 1L] <- ""
            attr(dder, "names")[length(dder)] <- jnt(colnames(x), 
                sep = sep)
        }
        else {
            attr(dder, "names")[mi] <- jnt(colnames(x), sep = sep)
        }
    }
    else {
        NA
    }
    attr(dder, "names")[which(is.na(attr(dder, "names")) == TRUE)] <- ""
    class(dder) <- c("Galois", "full")
    derr <- exts
    attr(derr, "names") <- ints
    attr(derr, "names")[which(is.na(attr(derr, "names")) == TRUE)] <- ""
    for (i in which(derr == "")) {
        derr[[i]] <- character(0)
    }
    rm(i)
    lst <- (red = redl(dder, derr))
    class(lst) <- c("Galois", "reduced")
    switch(match.arg(labeling), full = Lst <- list(sep = sep, 
        gc = dder), reduced = Lst <- list(sep = sep, gc = lst))
    Lst
}
