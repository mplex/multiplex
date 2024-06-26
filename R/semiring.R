semiring <- 
function (x, type = c("balance", "cluster"), symclos = TRUE, 
    transclos = TRUE, k = 2, lbs) 
{
    if (isTRUE(attr(x, "class") == "Signed") == FALSE) 
        stop("\"x\" should be an object of a \"Signed\" class.")
    q <- x$s
    q[which(q == 1)] <- "p"
    q[which(q == 0)] <- "o"
    q[which(q == -1)] <- "n"
    if (isTRUE(all(levels(factor(as.matrix(q))) %in% c("a", "n", 
        "o", "p", "q")) == TRUE) == TRUE) {
        if (isTRUE(symclos == TRUE) == TRUE) {
            for (i in 1:nrow(q)) {
                for (j in 1:ncol(q)) {
                  if (isTRUE(q[i, j] != q[j, i]) == TRUE) {
                    if (isTRUE(q[i, j] == "o") == TRUE) {
                      q[i, j] <- q[j, i]
                    }
                    else if (isTRUE(q[j, i] == "o") == TRUE) {
                      q[j, i] <- q[i, j]
                    }
                  }
                }
                rm(j)
            }
            rm(i)
            for (i in 1:nrow(q)) {
                for (j in 1:ncol(q)) {
                  if (isTRUE(q[i, j] != q[j, i]) == TRUE) {
                    if (isTRUE(q[i, j] == "p") == TRUE) {
                      q[j, i] <- q[i, j]
                    }
                    else if (isTRUE(q[j, i] == "p") == TRUE) {
                      q[i, j] <- q[j, i]
                    }
                    else if (isTRUE(q[i, j] == "q") == TRUE || 
                      isTRUE(q[j, i] == "q") == TRUE) {
                      q[i, j] <- q[j, i] <- "p"
                    }
                    else {
                      q[i, j] <- "a"
                    }
                  }
                }
                rm(j)
            }
            rm(i)
        }
        ifelse(isTRUE(attr(q, "class") != "data.frame") == TRUE, 
            Q <- as.data.frame(unclass(q)), Q <- as.data.frame(q))
        if (k == 1) {
            lst <- list(val = x$val, s = x$s, Q = noquote(as.matrix(q)), 
                k = k)
            class(lst) <- c("Rel.Q", match.arg(type))
            return(lst)
        }
        else {
            for (z in 2:k) {
                chn <- list()
                for (h in 1:nrow(Q)) {
                  tmp2 <- list()
                  for (i in 1:nrow(Q)) {
                    tmp <- vector()
                    for (j in 1:ncol(Q)) {
                      tmp <- append(tmp, paste(Q[h, j], Q[j, 
                        i], sep = ", "))
                    }
                    rm(j)
                    tmp2[[i]] <- tmp
                  }
                  rm(i)
                  chn[[h]] <- tmp2
                }
                rm(h)
                rm(tmp, tmp2)
                mx <- list()
                for (i in 1:nrow(Q)) {
                  tmp2 <- list()
                  for (j in 1:nrow(Q)) {
                    tmp <- vector()
                    for (l in 1:ncol(Q)) {
                      ch <- strsplit(chn[[i]][[j]][l], ", ")[[1]]
                      switch(match.arg(type), balance = {
                        if ("o" %in% ch) {
                          tmp <- append(tmp, "o")
                        } else if ("a" %in% ch && is.na(match("o", 
                          ch)) == TRUE) {
                          tmp <- append(tmp, "a")
                        } else if (all(c("p", "n") %in% ch)) {
                          tmp <- append(tmp, "n")
                        } else tmp <- append(tmp, "p")
                      }, cluster = {
                        if ("o" %in% ch) {
                          tmp <- append(tmp, "o")
                        } else if ("q" %in% ch | all("n" == ch)) {
                          tmp <- append(tmp, "q")
                        } else if (all("p" == ch)) {
                          tmp <- append(tmp, "p")
                        } else if (all("a" == ch) | all(c("a", 
                          "p") %in% ch)) {
                          tmp <- append(tmp, "a")
                        } else tmp <- append(tmp, "n")
                      })
                      rm(ch)
                    }
                    rm(l)
                    tmp2[[j]] <- tmp
                  }
                  rm(j)
                  mx[[i]] <- tmp2
                }
                rm(i)
                rm(tmp, tmp2)
                switch(match.arg(type), balance = {
                  NA
                }, cluster = {
                  mxn <- mx
                  for (i in 1:ncol(Q)) {
                    for (j in 1:ncol(Q)) {
                      if (any("n" == strsplit(chn[[i]][[j]][i], 
                        ", ")[[1]])) {
                        mxn[[i]][[j]][i] <- "n"
                      } else if (any("p" == strsplit(chn[[i]][[j]][i], 
                        ", ")[[1]])) {
                        mxn[[i]][[j]][i] <- "p"
                      }
                    }
                    rm(j)
                  }
                  rm(i)
                  mx <- mxn
                  rm(mxn)
                })
                x2 <- data.frame(matrix(nrow = nrow(Q), ncol = ncol(Q)))
                for (i in 1:nrow(Q)) {
                  for (j in 1:nrow(Q)) {
                    ch <- mx[[i]][[j]]
                    switch(match.arg(type), balance = {
                      if (isTRUE(all("o" == ch)) == TRUE) {
                        x2[i, j] <- "o"
                      } else {
                        ch <- replace(ch, ch == "o", NA)
                        cch <- stats::na.omit(ch)
                        if ("a" %in% cch) {
                          x2[i, j] <- "a"
                        } else if (all("p" == cch)) {
                          x2[i, j] <- "p"
                        } else if (all("n" == cch)) {
                          x2[i, j] <- "n"
                        } else x2[i, j] <- "a"
                        rm(cch)
                      }
                    }, cluster = {
                      if (isTRUE(all("o" == ch)) == TRUE) {
                        x2[i, j] <- "o"
                      } else {
                        ch <- replace(ch, ch == "o", NA)
                        cch <- stats::na.omit(ch)
                        if (isTRUE(length(cch) == 0) == TRUE) {
                          x2[i, j] <- "o"
                        } else if (all("p" == cch)) {
                          x2[i, j] <- "p"
                        } else if (all("n" == cch)) {
                          x2[i, j] <- "n"
                        } else if (all("q" == cch)) {
                          x2[i, j] <- "q"
                        } else if (all("a" == cch)) {
                          x2[i, j] <- "a"
                        } else {
                          cch <- replace(cch, cch == "q", NA)
                          ccch <- stats::na.omit(cch)
                          if ("a" %in% ccch) {
                            x2[i, j] <- "a"
                          } else if (all("p" == ccch)) {
                            x2[i, j] <- "p"
                          } else if (all("n" == ccch)) {
                            x2[i, j] <- "n"
                          } else if (all("q" == ccch)) {
                            x2[i, j] <- "q"
                          } else x2[i, j] <- "a"
                          rm(ccch)
                        }
                        rm(cch)
                      }
                    })
                  }
                  rm(j)
                  rm(ch)
                }
                rm(i)
                switch(match.arg(type), balance = {
                  if (isTRUE(transclos == TRUE) == TRUE) {
                    y2 <- as.matrix(x2)
                    y2 <- replace(y2, y2 == "p", 1L)
                    y2 <- replace(y2, y2 != 1L, 0)
                    for (i in seq_len(ncol(y2))) {
                      y2 <- pmax(y2, outer(y2[, i], y2[i, ], 
                        pmin.int))
                    }
                    rm(i)
                    y2 <- replace(y2, y2 == 1L, "p")
                    y2 <- replace(y2, y2 != "p", "o")
                    nn <- which(x2 == "n", arr.ind = TRUE)
                    for (i in 1:nrow(nn)) y2[nn[, 1][i], nn[, 
                      2][i]] <- "n"
                    rm(i, nn)
                    aa <- which(x2 == "a", arr.ind = TRUE)
                    for (i in 1:nrow(aa)) y2[aa[, 1][i], aa[, 
                      2][i]] <- "a"
                    rm(i, aa)
                    qq <- which(x2 == "q", arr.ind = TRUE)
                    for (i in 1:nrow(qq)) y2[qq[, 1][i], qq[, 
                      2][i]] <- "q"
                    rm(i, qq)
                    x2 <- data.frame(matrix(nrow = nrow(x2), 
                      ncol = ncol(x2)))
                    for (i in 1:nrow(y2)) x2[i, ] <- y2[i, ]
                    rm(i)
                  } else {
                    NA
                  }
                  Q <- x2
                }, cluster = {
                  chmx <- list()
                  for (h in 1:nrow(Q)) {
                    tmp2 <- list()
                    for (i in 1:nrow(Q)) {
                      tmp <- vector()
                      for (j in 1:ncol(Q)) {
                        tmp <- append(tmp, c(Q[h, j], Q[j, i]))
                      }
                      rm(j)
                      tmp2[[i]] <- tmp
                    }
                    rm(i)
                    chmx[[h]] <- tmp2
                  }
                  rm(h)
                  rm(tmp, tmp2)
                  if (isTRUE(transclos == TRUE) == TRUE) {
                    y2 <- as.matrix(x2)
                    y2 <- replace(y2, y2 == "p", 1L)
                    y2 <- replace(y2, y2 != 1L, 0)
                    for (i in seq_len(ncol(y2))) {
                      y2 <- pmax(y2, outer(y2[, i], y2[i, ], 
                        pmin.int))
                    }
                    rm(i)
                    y2 <- replace(y2, y2 == 1L, "p")
                    y2 <- replace(y2, y2 != "p", "o")
                    nn <- which(x2 == "n", arr.ind = TRUE)
                    for (i in 1:nrow(nn)) y2[nn[, 1][i], nn[, 
                      2][i]] <- "n"
                    rm(i, nn)
                    aa <- which(x2 == "a", arr.ind = TRUE)
                    for (i in 1:nrow(aa)) y2[aa[, 1][i], aa[, 
                      2][i]] <- "a"
                    rm(i, aa)
                    qq <- which(x2 == "q", arr.ind = TRUE)
                    for (i in 1:nrow(qq)) y2[qq[, 1][i], qq[, 
                      2][i]] <- "q"
                    rm(i, qq)
                    x2 <- data.frame(matrix(nrow = nrow(x2), 
                      ncol = ncol(x2)))
                    for (i in 1:nrow(y2)) x2[i, ] <- y2[i, ]
                    rm(i)
                  } else {
                    NA
                  }
                  x3 <- x2
                  for (o in seq_along(which(x2 == "o"))) {
                    if (isTRUE(sum(as.numeric(chmx[[which(x2 == 
                      "o", arr.ind = TRUE)[, 2][1]]][[which(x2 == 
                      "o", arr.ind = TRUE)[, 1][1]]] == "n")) > 
                      0) == TRUE) {
                      x3[which(x2 == "o", arr.ind = TRUE)[, 1][o], 
                        which(x2 == "o", arr.ind = TRUE)[, 2][o]] <- "n"
                    }
                  }
                  rm(o)
                  Q <- x3
                  rm(chmx, x2, x3)
                })
            }
            rm(z)
            Q <- noquote(as.matrix(Q))
            if (isTRUE(is.null(dimnames(q)) == FALSE) == TRUE) 
                rownames(Q) <- colnames(Q) <- dimnames(q)[[1]]
            if (missing(lbs) == FALSE && isTRUE(length(lbs) == 
                dim(Q)[1]) == TRUE) 
                rownames(Q) <- colnames(Q) <- lbs
            lst <- list(val = x$val, s = x$s, Q = Q, k = k)
            class(lst) <- c("Rel.Q", match.arg(type))
            return(lst)
        }
    }
    else {
        stop("Valence not permitted included in the data; cf. the manual to see the possible entries in 'x'.")
    }
}
