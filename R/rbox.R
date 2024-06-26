rbox <- 
function (w, transp = FALSE, smpl = FALSE, k = 3, tlbs) 
{
    if (is.array(w) == FALSE) 
        stop("Data must be a stacked array of square matrices.")
    if (is.na(dim(w)[3]) == TRUE) 
        stop("Data must have at least 2 dimensions.")
    if (k > 9L) 
        stop("Only polynomial until length 9 is supported.")
    x <- w
    ifelse(is.null(dimnames(w)[[3]]) == FALSE, lbs <- dimnames(w)[[3]], 
        lbs <- seq_len(dim(w)[3]))
    if (smpl == TRUE) {
        olbs <- dimnames(w)[[3]]
        if (is.null(dimnames(w)[[3]]) == FALSE) {
            nlb <- list()
            for (i in seq_len(length(lbs))) nlb[i] <- lbs[i]
            for (i in seq_len(length(nlb))) lbs[i] <- (strsplit(nlb[[i]], 
                "")[[1]][1])
            dimnames(w)[[3]] <- lbs
        }
        nlbs <- dimnames(w)[[3]]
    }
    if (transp == TRUE) {
        if (missing(tlbs) == TRUE) {
            LBS <- chartr("a-zA-Z", "A-Za-z", lbs)
        }
        else {
            if (isTRUE(length(tlbs) == dim(w)[3]) == TRUE) {
                LBS <- tlbs
            }
            else {
                warning("\"tlbs\" has not equal length to dim(w)[3], use toggle case for labels instead...")
                LBS <- chartr("a-zA-Z", "A-Za-z", lbs)
            }
        }
        Lbs <- as.vector(stats::na.exclude(LBS))
        tw <- array(dim = c(dim(w)[1], dim(w)[2], length(Lbs)))
        for (i in seq_len(length(Lbs))) {
            if (isTRUE(is.na(LBS)[i]) == FALSE) {
                tw[, , i] <- t(w[, , i])
            }
            else {
                NA
            }
        }
        rm(i)
        dimnames(tw)[3] <- list(Lbs)
        tmp <- zbnd(w, tw)
        lbs <- c(dimnames(w)[[3]], dimnames(tw)[[3]])
        w <- tmp
        dimnames(w)[[3]] <- lbs
        rm(tmp, tw)
    }
    w1 <- w
    if (isTRUE(k > 1L) == TRUE) {
        tmp2 <- data.frame(matrix(ncol = (dim(w)[1] * dim(w)[2]), 
            nrow = (dim(w)[3]^2)))
        lb2 <- vector()
        h <- 1L
        for (i in seq_len(dim(w)[3])) {
            for (j in seq_len(dim(w)[3])) {
                tmp2[h, ] <- replace(as.numeric(as.vector(w[, 
                  , i] %*% w[, , j])), as.numeric(as.vector(w[, 
                  , i] %*% w[, , j])) >= 1L, 1L)
                lb2[h] <- paste(dimnames(w)[[3]][i], dimnames(w)[[3]][j], 
                  sep = "")
                h <- h + 1L
            }
        }
        w2 <- array(dim = c(dim(w)[1], dim(w)[2], nrow(tmp2)))
        for (i in seq_len(nrow(tmp2))) {
            w2[, , i][seq_len((dim(w)[1] * dim(w)[2]))] <- as.numeric(tmp2[i, 
                ])
        }
        if (isTRUE(k > 2L) == TRUE) {
            tmp3 <- data.frame(matrix(ncol = (dim(w)[1] * dim(w)[2]), 
                nrow = (dim(w)[3]^3)))
            lb3 <- vector()
            h <- 1L
            for (i in seq_len(dim(w)[3])) {
                for (j in seq_len(dim(w)[3])) {
                  for (l in seq_len(dim(w)[3])) {
                    tmp3[h, ] <- replace(as.numeric(as.vector(w[, 
                      , i] %*% w[, , j] %*% w[, , l])), as.numeric(as.vector(w[, 
                      , i] %*% w[, , j] %*% w[, , l])) >= 1L, 
                      1L)
                    lb3[h] <- paste(dimnames(w)[[3]][i], dimnames(w)[[3]][j], 
                      dimnames(w)[[3]][l], sep = "")
                    h <- h + 1L
                  }
                }
            }
            w3 <- array(dim = c(dim(w)[1], dim(w)[2], nrow(tmp3)))
            for (i in seq_len(nrow(tmp3))) {
                w3[, , i][1:(dim(w)[1] * dim(w)[2])] <- as.numeric(tmp3[i, 
                  ])
            }
            if (isTRUE(k > 3L) == TRUE) {
                tmp4 <- data.frame(matrix(ncol = (dim(w)[1] * 
                  dim(w)[2]), nrow = (dim(w)[3]^4)))
                lb4 <- vector()
                h <- 1L
                for (i in seq_len(dim(w)[3])) {
                  for (j in seq_len(dim(w)[3])) {
                    for (l in seq_len(dim(w)[3])) {
                      for (m in seq_len(dim(w)[3])) {
                        tmp4[h, ] <- replace(as.numeric(as.vector(w[, 
                          , i] %*% w[, , j] %*% w[, , l] %*% 
                          w[, , m])), as.numeric(as.vector(w[, 
                          , i] %*% w[, , j] %*% w[, , l] %*% 
                          w[, , m])) >= 1L, 1L)
                        lb4[h] <- paste(dimnames(w)[[3]][i], 
                          dimnames(w)[[3]][j], dimnames(w)[[3]][l], 
                          dimnames(w)[[3]][m], sep = "")
                        h <- h + 1L
                      }
                    }
                  }
                }
                w4 <- array(dim = c(dim(w)[1], dim(w)[2], nrow(tmp4)))
                for (i in seq_len(nrow(tmp4))) {
                  w4[, , i][1:(dim(w)[1] * dim(w)[2])] <- as.numeric(tmp4[i, 
                    ])
                }
                if (isTRUE(k > 4L) == TRUE) {
                  tmp5 <- data.frame(matrix(ncol = (dim(w)[1] * 
                    dim(w)[2]), nrow = (dim(w)[3]^5)))
                  lb5 <- vector()
                  h <- 1L
                  for (i in seq_len(dim(w)[3])) {
                    for (j in seq_len(dim(w)[3])) {
                      for (l in seq_len(dim(w)[3])) {
                        for (m in seq_len(dim(w)[3])) {
                          for (n in seq_len(dim(w)[3])) {
                            tmp5[h, ] <- replace(as.numeric(as.vector(w[, 
                              , i] %*% w[, , j] %*% w[, , l] %*% 
                              w[, , m] %*% w[, , n])), as.numeric(as.vector(w[, 
                              , i] %*% w[, , j] %*% w[, , l] %*% 
                              w[, , m] %*% w[, , n])) >= 1L, 
                              1L)
                            lb5[h] <- paste(dimnames(w)[[3]][i], 
                              dimnames(w)[[3]][j], dimnames(w)[[3]][l], 
                              dimnames(w)[[3]][m], dimnames(w)[[3]][n], 
                              sep = "")
                            h <- h + 1L
                          }
                        }
                      }
                    }
                  }
                  w5 <- array(dim = c(dim(w)[1], dim(w)[2], nrow(tmp5)))
                  for (i in seq_len(nrow(tmp5))) {
                    w5[, , i][seq_len((dim(w)[1] * dim(w)[2]))] <- as.numeric(tmp5[i, 
                      ])
                  }
                  if (isTRUE(k > 5L) == TRUE) {
                    tmp6 <- data.frame(matrix(ncol = (dim(w)[1] * 
                      dim(w)[2]), nrow = (dim(w)[3]^6)))
                    lb6 <- vector()
                    h <- 1L
                    for (i in seq_len(dim(w)[3])) {
                      for (j in seq_len(dim(w)[3])) {
                        for (l in seq_len(dim(w)[3])) {
                          for (m in seq_len(dim(w)[3])) {
                            for (n in seq_len(dim(w)[3])) {
                              for (o in seq_len(dim(w)[3])) {
                                tmp6[h, ] <- replace(as.numeric(as.vector(w[, 
                                  , i] %*% w[, , j] %*% w[, , 
                                  l] %*% w[, , m] %*% w[, , n] %*% 
                                  w[, , o])), as.numeric(as.vector(w[, 
                                  , i] %*% w[, , j] %*% w[, , 
                                  l] %*% w[, , m] %*% w[, , n] %*% 
                                  w[, , o])) >= 1L, 1L)
                                lb6[h] <- paste(dimnames(w)[[3]][i], 
                                  dimnames(w)[[3]][j], dimnames(w)[[3]][l], 
                                  dimnames(w)[[3]][m], dimnames(w)[[3]][n], 
                                  dimnames(w)[[3]][o], sep = "")
                                h <- h + 1L
                              }
                            }
                          }
                        }
                      }
                    }
                    w6 <- array(dim = c(dim(w)[1], dim(w)[2], 
                      nrow(tmp6)))
                    for (i in seq_len(nrow(tmp6))) {
                      w6[, , i][seq_len((dim(w)[1] * dim(w)[2]))] <- as.numeric(tmp6[i, 
                        ])
                    }
                    if (isTRUE(k > 6L) == TRUE) {
                      tmp7 <- data.frame(matrix(ncol = (dim(w)[1] * 
                        dim(w)[2]), nrow = (dim(w)[3]^7)))
                      lb7 <- vector()
                      h <- 1L
                      for (i in seq_len(dim(w)[3])) {
                        for (j in seq_len(dim(w)[3])) {
                          for (l in seq_len(dim(w)[3])) {
                            for (m in seq_len(dim(w)[3])) {
                              for (n in seq_len(dim(w)[3])) {
                                for (o in seq_len(dim(w)[3])) {
                                  for (q in seq_len(dim(w)[3])) {
                                    tmp7[h, ] <- replace(as.numeric(as.vector(w[, 
                                      , i] %*% w[, , j] %*% w[, 
                                      , l] %*% w[, , m] %*% w[, 
                                      , n] %*% w[, , o] %*% w[, 
                                      , q])), as.numeric(as.vector(w[, 
                                      , i] %*% w[, , j] %*% w[, 
                                      , l] %*% w[, , m] %*% w[, 
                                      , n] %*% w[, , o] %*% w[, 
                                      , q])) >= 1L, 1L)
                                    lb7[h] <- paste(dimnames(w)[[3]][i], 
                                      dimnames(w)[[3]][j], dimnames(w)[[3]][l], 
                                      dimnames(w)[[3]][m], dimnames(w)[[3]][n], 
                                      dimnames(w)[[3]][o], dimnames(w)[[3]][q], 
                                      sep = "")
                                    h <- h + 1L
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      w7 <- array(dim = c(dim(w)[1], dim(w)[2], 
                        nrow(tmp7)))
                      for (i in seq_len(nrow(tmp7))) {
                        w7[, , i][seq_len((dim(w)[1] * dim(w)[2]))] <- as.numeric(tmp7[i, 
                          ])
                      }
                      if (isTRUE(k > 7L) == TRUE) {
                        tmp8 <- data.frame(matrix(ncol = (dim(w)[1] * 
                          dim(w)[2]), nrow = (dim(w)[3]^8)))
                        lb8 <- vector()
                        h <- 1L
                        for (i in seq_len(dim(w)[3])) {
                          for (j in seq_len(dim(w)[3])) {
                            for (l in seq_len(dim(w)[3])) {
                              for (m in seq_len(dim(w)[3])) {
                                for (n in seq_len(dim(w)[3])) {
                                  for (o in seq_len(dim(w)[3])) {
                                    for (q in seq_len(dim(w)[3])) {
                                      for (r in seq_len(dim(w)[3])) {
                                        tmp8[h, ] <- replace(as.numeric(as.vector(w[, 
                                          , i] %*% w[, , j] %*% 
                                          w[, , l] %*% w[, , 
                                          m] %*% w[, , n] %*% 
                                          w[, , o] %*% w[, , 
                                          q] %*% w[, , r])), 
                                          as.numeric(as.vector(w[, 
                                            , i] %*% w[, , j] %*% 
                                            w[, , l] %*% w[, 
                                            , m] %*% w[, , n] %*% 
                                            w[, , o] %*% w[, 
                                            , q] %*% w[, , r])) >= 
                                            1L, 1L)
                                        lb8[h] <- paste(dimnames(w)[[3]][i], 
                                          dimnames(w)[[3]][j], 
                                          dimnames(w)[[3]][l], 
                                          dimnames(w)[[3]][m], 
                                          dimnames(w)[[3]][n], 
                                          dimnames(w)[[3]][o], 
                                          dimnames(w)[[3]][q], 
                                          dimnames(w)[[3]][r], 
                                          sep = "")
                                        h <- h + 1L
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                        w8 <- array(dim = c(dim(w)[1], dim(w)[2], 
                          nrow(tmp8)))
                        for (i in seq_len(nrow(tmp8))) {
                          w8[, , i][seq_len((dim(w)[1] * dim(w)[2]))] <- as.numeric(tmp8[i, 
                            ])
                        }
                        if (isTRUE(k > 8L) == TRUE) {
                          tmp9 <- data.frame(matrix(ncol = (dim(w)[1] * 
                            dim(w)[2]), nrow = (dim(w)[3]^9)))
                          lb9 <- vector()
                          h <- 1L
                          for (i in seq_len(dim(w)[3])) {
                            for (j in seq_len(dim(w)[3])) {
                              for (l in seq_len(dim(w)[3])) {
                                for (m in seq_len(dim(w)[3])) {
                                  for (n in seq_len(dim(w)[3])) {
                                    for (o in seq_len(dim(w)[3])) {
                                      for (q in seq_len(dim(w)[3])) {
                                        for (r in seq_len(dim(w)[3])) {
                                          for (s in seq_len(dim(w)[3])) {
                                            tmp9[h, ] <- replace(as.numeric(as.vector(w[, 
                                              , i] %*% w[, , 
                                              j] %*% w[, , l] %*% 
                                              w[, , m] %*% w[, 
                                              , n] %*% w[, , 
                                              o] %*% w[, , q] %*% 
                                              w[, , r] %*% w[, 
                                              , s])), as.numeric(as.vector(w[, 
                                              , i] %*% w[, , 
                                              j] %*% w[, , l] %*% 
                                              w[, , m] %*% w[, 
                                              , n] %*% w[, , 
                                              o] %*% w[, , q] %*% 
                                              w[, , r] %*% w[, 
                                              , s])) >= 1L, 1L)
                                            lb9[h] <- paste(dimnames(w)[[3]][i], 
                                              dimnames(w)[[3]][j], 
                                              dimnames(w)[[3]][l], 
                                              dimnames(w)[[3]][m], 
                                              dimnames(w)[[3]][n], 
                                              dimnames(w)[[3]][o], 
                                              dimnames(w)[[3]][q], 
                                              dimnames(w)[[3]][r], 
                                              dimnames(w)[[3]][s], 
                                              sep = "")
                                            h <- h + 1L
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          w9 <- array(dim = c(dim(w)[1], dim(w)[2], 
                            nrow(tmp9)))
                          for (i in seq_len(nrow(tmp9))) {
                            w9[, , i][seq_len((dim(w)[1] * dim(w)[2]))] <- as.numeric(tmp9[i, 
                              ])
                          }
                        }
                      }
                    }
                  }
                }
            }
        }
    }
    {
        if (k == 9L) {
            W <- zbnd(w1, zbnd(w2, zbnd(w3, zbnd(w4, zbnd(w5, 
                zbnd(w6, zbnd(w7, zbnd(w8, w9))))))))
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2, lb3, lb4, lb5, 
                  lb6, lb7, lb8, lb9)
        }
        else if (k == 8L) {
            W <- zbnd(w1, zbnd(w2, zbnd(w3, zbnd(w4, zbnd(w5, 
                zbnd(w6, zbnd(w7, w8)))))))
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2, lb3, lb4, lb5, 
                  lb6, lb7, lb8)
        }
        else if (k == 7L) {
            W <- zbnd(w1, zbnd(w2, zbnd(w3, zbnd(w4, zbnd(w5, 
                zbnd(w6, w7))))))
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2, lb3, lb4, lb5, 
                  lb6, lb7)
        }
        else if (k == 6L) {
            W <- zbnd(w1, zbnd(w2, zbnd(w3, zbnd(w4, zbnd(w5, 
                w6)))))
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2, lb3, lb4, lb5, 
                  lb6)
        }
        else if (k == 5L) {
            W <- zbnd(w1, zbnd(w2, zbnd(w3, zbnd(w4, w5))))
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2, lb3, lb4, lb5)
        }
        else if (k == 4L) {
            W <- zbnd(w1, zbnd(w2, zbnd(w3, w4)))
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2, lb3, lb4)
        }
        else if (k == 3L) {
            W <- zbnd(w1, zbnd(w2, w3))
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2, lb3)
        }
        else if (k == 2L) {
            W <- zbnd(w1, w2)
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- c(lbs, lb2)
        }
        else {
            W <- w1
            if (is.null(dimnames(w)[[3]]) == FALSE) 
                dimnames(W)[[3]] <- lbs
        }
    }
    if (transp == TRUE) {
        if (smpl == TRUE) {
            lst <- list(w = w, W = W, lbs = dimnames(x)[[1]], 
                Note = c("Relation labels have been simplified and the transpose relations are included."), 
                Orels = olbs, Srels = nlbs, Trels = Lbs, k = k, 
                z = dim(W)[3])
        }
        else {
            lst <- list(w = w, W = W, lbs = dimnames(x)[[1]], 
                Note = c("Transpose relations are included"), 
                Trels = Lbs, k = k, z = dim(W)[3])
        }
    }
    else if (transp == FALSE) {
        if (smpl == TRUE) {
            lst <- list(w = w, W = W, lbs = dimnames(x)[[1]], 
                Note = c("Relation labels have been simplified"), 
                Orels = olbs, Srels = nlbs, k = k, z = dim(W)[3])
        }
        else if (smpl == FALSE) {
            lst <- list(w = w, W = W, lbs = dimnames(x)[[1]], 
                k = k, z = dim(W)[3])
        }
    }
    class(lst) <- "Rel.Box"
    return(lst)
}
