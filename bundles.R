bundles <-
function (x, loops = FALSE, smpl = FALSE, lb2lb = TRUE, collapse = FALSE, 
    sep) 
{
    if (is.array(x) == FALSE) 
        stop("'x' must be an array.")
    if (isTRUE(dim(x)[1] == dim(x)[2]) == FALSE) 
        stop("'x' must be a square array.")
    ifelse(missing(sep) == TRUE, sep <- ", ", NA)
    ifelse(isTRUE(is.null(dimnames(x)[1]) == TRUE | is.null(dimnames(x)[1][[1]]) == 
        TRUE) == TRUE, LBS <- seq_len(nrow(x)), LBS <- dimnames(x)[[1]])
    lbs <- seq(LBS)
    xd <- dichot(x, c = 1L, diag = TRUE)
    ifelse(isTRUE(dim(xd)[3] == 1) == TRUE, xd <- xd[, , 1], 
        NA)
    if (is.na(dim(xd)[3]) == FALSE) {
        TRD <- TRUE
        r <- dim(x)[3]
        if (isTRUE(is.null(dimnames(x)[[3]]) == TRUE) | isTRUE(any(duplicated(dimnames(x)[[3]]))) == 
            TRUE) {
            dimnames(x)[[3]] <- seq_len(dim(x)[3])
        }
        mlt <- list()
        for (i in seq_len(dim(x)[3])) {
            mlt[[i]] <- trnf(xd[, , i], tolist = TRUE, lbs = lbs, 
                sep = sep, lb2lb = lb2lb)
        }
        rm(i)
        m <- unlist(mlt)
    }
    else {
        TRD <- FALSE
        r <- 1
        m <- trnf(xd, tolist = TRUE, lbs = lbs, sep = sep, lb2lb = lb2lb)
    }
    if (smpl) {
        if (isTRUE(TRD == TRUE) == TRUE) {
            tmp <- list()
            tt <- vector()
            if (isTRUE(is.null(dimnames(x)[[3]])) == FALSE) {
                for (i in seq_len(length(dimnames(x)[[3]]))) tmp[i] <- dimnames(x)[[3]][i]
                rm(i)
                for (i in seq_len(length(tmp))) tt[i] <- (strsplit(tmp[[i]], 
                  "")[[1]][1])
                rm(i)
            }
            else {
                for (i in seq_len(dim(x)[3])) tt[i] <- tmp[i] <- i
                rm(i)
            }
            for (k in seq_len(r)) {
                allr <- paste("all", tt[k], sep = "_")
                assign(allr, trnf(xd[, , k], tolist = TRUE, lbs = lbs, 
                  sep = sep, lb2lb = lb2lb))
                tmp <- trnf(xd[, , k], tolist = TRUE, lbs = lbs, 
                  sep = sep, lb2lb = lb2lb)
                tDF <- data.frame(matrix(ncol = 2L, nrow = 0L))
                for (i in seq_len(length(tmp))) {
                  tDF[i, 1] <- strsplit(tmp[i], sep)[[1]][1]
                  tDF[i, 2] <- strsplit(tmp[i], sep)[[1]][2]
                }
                rm(i)
                rm(tmp)
                oud <- list()
                ind <- list()
                ald <- list()
                for (i in seq_len(length(lbs))) {
                  oud[[i]] <- as.numeric(tDF[which(tDF[, 1] == 
                    as.numeric(lbs[i])), 2])
                  ind[[i]] <- as.numeric(tDF[which(tDF[, 2] == 
                    as.numeric(lbs[i])), 1])
                  ald[[i]] <- c(oud[[i]], ind[[i]])
                }
                rm(i)
                assign(allr, ald)
                rm(oud, ind, ald, allr)
                rm(tDF)
            }
            rm(k)
        }
        else if (isTRUE(TRD == FALSE) == TRUE) {
            tt <- "R"
            tmp <- x
        }
    }
    else if (!(smpl)) {
        ifelse(isTRUE(TRD == TRUE) == TRUE, tt <- dimnames(x)[[3]], 
            NA)
    }
    bd <- bnd(x, xd, lbs, TRD, r, m, mlt, sep)
    As <- vector()
    for (i in seq_len(length(bd$asym))) {
        for (j in seq_len(length(bd$asym[[i]]))) {
            if (isTRUE(length(bd$asym[[i]]) != 0L) == TRUE) {
                if (isTRUE(is.na(dim(x)[3])) == FALSE) {
                  As <- append(As, paste(lbs[i], bd$asym[[i]][j], 
                    sep = sep))
                }
                else {
                  ifelse(isTRUE(bd$asym[[i]][j] %in% bd$out[[i]]) == 
                    TRUE, As <- append(As, paste(lbs[i], bd$asym[[i]][j], 
                    sep = sep)), NA)
                }
            }
        }
        rm(j)
    }
    rm(i)
    if (isTRUE(TRD == TRUE) == TRUE) {
        AS <- list()
        for (k in seq_len(r)) {
            tmp <- vector()
            for (i in which(As %in% trnf(xd[, , k], tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmp <- append(tmp, As[i])
            }
            rm(i)
            AS[[k]] <- as.character(tmp)
        }
        rm(k)
    }
    else {
        AS <- as.character(As)
    }
    Rp <- vector()
    for (i in seq_len(length(bd$recp))) {
        for (j in seq_len(length(bd$recp[[i]]))) {
            if (isTRUE(length(bd$recp[[i]]) != 0L) == TRUE) {
                Rp <- append(Rp, paste(lbs[i], bd$recp[[i]][j], 
                  sep = sep))
            }
        }
        rm(j)
    }
    rm(i)
    if (isTRUE(TRD == TRUE) == TRUE) {
        RP <- list()
        for (k in seq_len(r)) {
            tmp <- vector()
            for (i in which(Rp %in% trnf(xd[, , k], tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Rp[i])
            }
            rm(i)
            RP[[k]] <- as.character(tmp)
        }
        rm(k)
    }
    else {
        RP <- as.character(Rp)
    }
    Xc <- vector()
    if (isTRUE(TRD == TRUE) == TRUE) {
        for (i in seq_len(length(bd$xchg))) {
            for (j in seq_len(length(bd$xchg[[i]]))) {
                if (isTRUE(length(bd$xchg[[i]]) != 0L) == TRUE) {
                  if (isTRUE(lbs[i] < bd$xchg[[i]][j]) == TRUE) 
                    Xc <- append(Xc, paste(lbs[i], bd$xchg[[i]][j], 
                      sep = sep))
                }
            }
            rm(j)
        }
        rm(i)
        XC <- list()
        XCt <- list()
        for (k in seq_len(r)) {
            tmp <- vector()
            tmpt <- vector()
            for (i in which(Xc %in% trnf(xd[, , k], tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Xc[i])
            }
            rm(i)
            for (i in which(Xc %in% trnf(t(xd[, , k]), tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmpt <- append(tmpt, paste(strsplit(Xc[i], sep)[[1]][2], 
                  strsplit(Xc[i], sep)[[1]][1], sep = sep))
            }
            rm(i)
            XC[[k]] <- tmp
            XCt[[k]] <- tmpt
        }
        rm(k)
        XCH <- list()
        for (i in seq_len(length(XC))) {
            XCH[[i]] <- as.character(c(XC[[i]], XCt[[i]]))
        }
        rm(i)
    }
    else {
        XCH <- as.character(Xc)
    }
    Et <- vector()
    if (isTRUE(TRD == TRUE) == TRUE) {
        for (i in seq_len(length(bd$Eout))) {
            for (j in seq_len(length(bd$Eout[[i]]))) {
                if (isTRUE(length(bd$Eout[[i]]) != 0L) == TRUE) {
                  Et <- append(Et, paste(lbs[i], bd$Eout[[i]][j], 
                    sep = sep))
                }
            }
            rm(j)
        }
        rm(i)
        ENT <- list()
        for (k in seq_len(r)) {
            tmp <- vector()
            for (i in which(Et %in% trnf(xd[, , k], tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Et[i])
            }
            rm(i)
            ENT[[k]] <- as.character(tmp)
        }
        rm(k)
    }
    else {
        ENT <- as.character(Et)
    }
    Mx <- vector()
    if (isTRUE(TRD == TRUE) == TRUE) {
        for (i in seq_len(length(bd$mixe))) {
            for (j in seq_len(length(bd$mixe[[i]]))) {
                if (isTRUE(length(bd$mixe[[i]]) != 0L) == TRUE) {
                  if (isTRUE(lbs[i] < bd$mixe[[i]][j]) == TRUE) 
                    Mx <- append(Mx, paste(lbs[i], bd$mixe[[i]][j], 
                      sep = sep))
                }
            }
            rm(j)
        }
        rm(i)
        MX <- list()
        MXt <- list()
        for (k in seq_len(r)) {
            tmp <- vector()
            tmpt <- vector()
            for (i in which(Mx %in% trnf(xd[, , k], tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Mx[i])
            }
            rm(i)
            for (i in which(Mx %in% trnf(t(xd[, , k]), tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmpt <- append(tmpt, paste(strsplit(Mx[i], sep)[[1]][2], 
                  strsplit(Mx[i], sep)[[1]][1], sep = sep))
            }
            rm(i)
            MX[[k]] <- tmp
            MXt[[k]] <- tmpt
        }
        rm(k)
        MIX <- list()
        for (i in seq_len(length(MX))) {
            MIX[[i]] <- as.character(c(MX[[i]], MXt[[i]]))
        }
        rm(i)
    }
    else {
        MIX <- as.character(Mx)
    }
    Fl <- vector()
    if (isTRUE(TRD == TRUE) == TRUE) {
        for (i in seq_len(length(bd$full))) {
            for (j in seq_len(length(bd$full[[i]]))) {
                if (isTRUE(length(bd$full[[i]]) != 0L) == TRUE) {
                  if (isTRUE(lbs[i] < bd$full[[i]][j]) == TRUE) 
                    Fl <- append(Fl, paste(lbs[i], bd$full[[i]][j], 
                      sep = sep))
                }
            }
            rm(j)
        }
        rm(i)
        FL <- list()
        FLt <- list()
        for (k in seq_len(r)) {
            tmp <- vector()
            tmpt <- vector()
            for (i in which(Fl %in% trnf(xd[, , k], tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmp <- append(tmp, Fl[i])
            }
            rm(i)
            for (i in which(Fl %in% trnf(t(xd[, , k]), tolist = TRUE, 
                lbs = lbs, sep = sep, lb2lb = lb2lb))) {
                tmpt <- append(tmpt, paste(strsplit(Fl[i], sep)[[1]][2], 
                  strsplit(Fl[i], sep)[[1]][1], sep = sep))
            }
            rm(i)
            FL[[k]] <- tmp
            FLt[[k]] <- tmpt
        }
        rm(k)
        FUL <- list()
        for (i in seq_len(length(FL))) {
            FUL[[i]] <- as.character(c(FL[[i]], FLt[[i]]))
        }
        rm(i)
    }
    else {
        FUL <- as.character(Fl)
    }
    if (lb2lb) {
        if (isTRUE(is.null(dimnames(x)[[1]]) == TRUE) == FALSE) {
            if (length(AS) > 0L) {
                for (k in seq_len(length(AS))) {
                  for (i in seq_len(length(AS[[k]]))) {
                    if (length(AS[[k]]) > 0L) {
                      AS[[k]][i] <- paste(dimnames(x)[[1]][as.numeric(strsplit(AS[[k]][i], 
                        sep)[[1]][1])], dimnames(x)[[1]][as.numeric(strsplit(AS[[k]][i], 
                        sep)[[1]][2])], sep = sep)
                    }
                  }
                  rm(i)
                }
                rm(k)
            }
            if (length(RP) > 0L) {
                for (k in seq_len(length(RP))) {
                  for (i in seq_len(length(RP[[k]]))) {
                    if (length(RP[[k]]) > 0L) {
                      RP[[k]][i] <- paste(dimnames(x)[[1]][as.numeric(strsplit(RP[[k]][i], 
                        sep)[[1]][1])], dimnames(x)[[1]][as.numeric(strsplit(RP[[k]][i], 
                        sep)[[1]][2])], sep = sep)
                    }
                  }
                  rm(i)
                }
                rm(k)
            }
            if (isTRUE(is.na(dim(x)[3])) == FALSE) {
                if (length(XCH) > 0L) {
                  for (k in seq_len(length(XCH))) {
                    for (i in seq_len(length(XCH[[k]]))) {
                      if (length(XCH[[k]]) > 0L) {
                        XCH[[k]][i] <- paste(dimnames(x)[[1]][as.numeric(strsplit(XCH[[k]][i], 
                          sep)[[1]][1])], dimnames(x)[[1]][as.numeric(strsplit(XCH[[k]][i], 
                          sep)[[1]][2])], sep = sep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                if (length(ENT) > 0L) {
                  for (k in seq_len(length(ENT))) {
                    for (i in seq_len(length(ENT[[k]]))) {
                      if (length(ENT[[k]]) > 0L) {
                        ENT[[k]][i] <- paste(dimnames(x)[[1]][as.numeric(strsplit(ENT[[k]][i], 
                          sep)[[1]][1])], dimnames(x)[[1]][as.numeric(strsplit(ENT[[k]][i], 
                          sep)[[1]][2])], sep = sep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                if (length(MIX) > 0L) {
                  for (k in seq_len(length(MIX))) {
                    for (i in seq_len(length(MIX[[k]]))) {
                      if (length(MIX[[k]]) > 0L) {
                        MIX[[k]][i] <- paste(dimnames(x)[[1]][as.numeric(strsplit(MIX[[k]][i], 
                          sep)[[1]][1])], dimnames(x)[[1]][as.numeric(strsplit(MIX[[k]][i], 
                          sep)[[1]][2])], sep = sep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
                if (length(FUL) > 0L) {
                  for (k in seq_len(length(FUL))) {
                    for (i in seq_len(length(FUL[[k]]))) {
                      if (length(FUL[[k]]) > 0L) {
                        FUL[[k]][i] <- paste(dimnames(x)[[1]][as.numeric(strsplit(FUL[[k]][i], 
                          sep)[[1]][1])], dimnames(x)[[1]][as.numeric(strsplit(FUL[[k]][i], 
                          sep)[[1]][2])], sep = sep)
                      }
                    }
                    rm(i)
                  }
                  rm(k)
                }
            }
        }
    }
    if (loops) {
        if (isTRUE(TRD == TRUE) == TRUE) {
            LOP <- list()
            length(LOP) <- dim(x)[3]
            for (i in seq_len(dim(x)[3])) {
                lp <- which(diag(xd[, , i]) != 0L)
                if (isTRUE(length(lp) > 0L) == TRUE) {
                  for (j in seq_len(length(lp))) {
                    if (lb2lb) {
                      if (isTRUE(is.null(dimnames(x)[[1]])) == 
                        FALSE) {
                        LOP[[i]] <- append(LOP[[i]], paste(dimnames(x)[[1]][lp][j], 
                          dimnames(x)[[1]][lp][j], sep = sep))
                      }
                      else if (isTRUE(is.null(dimnames(x)[[1]])) == 
                        TRUE) {
                        LOP[[i]] <- append(LOP[[i]], paste(lp[j], 
                          lp[j], sep = sep))
                      }
                    }
                    else {
                      LOP[[i]] <- append(LOP[[i]], paste(lp[j], 
                        lp[j], sep = sep))
                    }
                  }
                  rm(j)
                }
                else {
                  LOP[[i]] <- character(0)
                }
            }
            rm(i)
            attr(LOP, "names") <- dimnames(x)[[3]]
        }
        else {
            LOP <- vector()
            lp <- which(diag(xd) != 0L)
            for (j in seq_len(length(lp))) {
                LOP <- append(LOP, paste(LBS[lp][j], LBS[lp][j], 
                  sep = sep))
            }
            rm(j)
        }
        if (isTRUE(smpl == FALSE) == TRUE) {
            ifelse(isTRUE(TRD == TRUE) == TRUE, attr(LOP, "names") <- dimnames(x)[[3]], 
                NA)
        }
        else {
            ifelse(isTRUE(TRD == TRUE) == TRUE, attr(LOP, "names") <- tt, 
                NA)
        }
    }
    if (isTRUE(TRD == TRUE) == TRUE) {
        ifelse(isTRUE(smpl == FALSE) == TRUE, attr(FUL, "names") <- attr(MIX, 
            "names") <- attr(ENT, "names") <- attr(XCH, "names") <- attr(RP, 
            "names") <- attr(AS, "names") <- dimnames(x)[[3]], 
            attr(FUL, "names") <- attr(MIX, "names") <- attr(ENT, 
                "names") <- attr(XCH, "names") <- attr(RP, "names") <- attr(AS, 
                "names") <- tt)
    }
    if (collapse) {
        ifelse(isTRUE(loops == FALSE) == TRUE, lst <- list(asym = as.vector(unlist(AS)), 
            recp = as.vector(unlist(RP)), tent = as.vector(unlist(ENT)), 
            txch = as.vector(unlist(XCH)), mixd = as.vector(unlist(MIX)), 
            full = as.vector(unlist(FUL))), lst <- list(asym = as.vector(unlist(AS)), 
            recp = as.vector(unlist(RP)), tent = as.vector(unlist(ENT)), 
            txch = as.vector(unlist(XCH)), mixd = as.vector(unlist(MIX)), 
            full = as.vector(unlist(FUL)), loop = as.vector(unlist(LOP))))
    }
    else {
        ifelse(isTRUE(loops == FALSE) == TRUE, lst <- list(asym = AS, 
            recp = RP, tent = ENT, txch = XCH, mixd = MIX, full = FUL), 
            lst <- list(asym = AS, recp = RP, tent = ENT, txch = XCH, 
                mixd = MIX, full = FUL, loop = LOP))
    }
    class(lst) <- c("Rel.Bundles", paste0("sep: ", sep))
    return(lst)
}
