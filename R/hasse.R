hasse <-
function (x, attrs = NULL, main = NULL, incmp, cex.main, bg, 
    mar, shape, col, col0, fcol, ecol, lty, lbs, ffamily, fstyle, 
    fsize, col.main, sep, ...) 
{
    if (requireNamespace("Rgraphviz", quietly = TRUE)) {
        ifelse(missing(shape) == TRUE || shape %in% c("rectangle", 
            "rect", "circle", "box", "ellipse") == FALSE, shape <- "rectangle", 
            NA)
        ifelse(missing(col) == TRUE, fillcolor <- "transparent", 
            fillcolor <- col)
        ifelse(missing(col0) == TRUE, color <- "transparent", 
            color <- col0)
        ifelse(missing(fcol) == TRUE, colort <- "#000000", colort <- fcol)
        ifelse(missing(ecol) == TRUE, colore <- "#000000", colore <- ecol)
        ifelse(missing(lty) == TRUE, lty <- "solid", NA)
        if (missing(lbs) == TRUE) {
            label <- ""
            ifelse(missing(fsize) == TRUE, fsize <- 16L, NA)
        }
        else {
            label <- " "
            colort <- "transparent"
            ifelse(missing(fsize) == TRUE, fsize <- 1.6, fsize <- fsize/10L)
        }
        if (is.null(attrs) == TRUE) {
            attrs = list(graph = list(rankdir = "BT"), edge = list(arrowsize = "0", 
                style = "lty", minlen = "1", color = colore), 
                node = list(shape = shape, color = color, fixedsize = FALSE, 
                  fontsize = fsize, fillcolor = fillcolor, fontcolor = colort, 
                  label = label))
        }
        po <- x & (1L - t(x))
        diag(po) <- 0L
        if (missing(lbs) == TRUE && is.null(dimnames(x)[[1]]) == 
            TRUE) {
            rownames(po) <- colnames(po) <- as.character(utils::as.roman(c(seq_len(dim(x)[1]))))
        }
        else if (missing(lbs) == FALSE) {
            if (isTRUE(length(lbs) == 1L) == TRUE && isTRUE(dim(x)[1] > 
                1L) == TRUE) {
                tlbs <- vector("list", length = dim(x)[1])
                for (i in seq_len(dim(x)[1])) tlbs[[i]] <- lbs[[1]][[i]]
                lbs <- tlbs
                rm(tlbs)
            }
            else {
                NA
            }
            ifelse(missing(sep) == TRUE, sep <- ",", NA)
            ifelse(is.list(lbs) == TRUE, lbs <- unlist(jnt(lbs, 
                sep = sep)), NA)
            rownames(po) <- colnames(po) <- NULL
        }
        if (isTRUE(ncol(x) == nrow(x)) == TRUE) {
            for (i in seq_len(ncol(po))) {
                tmp <- outer(po[, i], po[i, ], pmin.int)
                po <- pmin(po, (1L - tmp))
            }
            rm(tmp)
        }
        else {
            stop("binary operation on non-conformable arrays")
        }
        if (missing(incmp) == FALSE && isTRUE(incmp == FALSE) == 
            TRUE) {
            px <- po
            out <- vector()
            for (i in seq_len(nrow(po))) {
                ifelse(isTRUE(sum(px[i, ] + px[, i]) == 0L) == 
                  TRUE, out[length(out) + 1L] <- i, NA)
            }
            rm(i)
            inn <- which(!(seq_len(nrow(po)) %in% out))
            po <- as.matrix(po[inn, inn])
        }
        else {
            NA
        }
        ifelse(missing(cex.main) == TRUE, cex.main <- graphics::par()$cex.main, 
            NA)
        ifelse(missing(col.main) == TRUE, col.main <- graphics::par()$col.main, 
            NA)
        omr <- graphics::par()$mar
        omi <- graphics::par()$mai
        if (missing(mar) == TRUE) {
            mar <- c(0, 0, 0, 0)
        }
        else {
            mar <- omr
        }
        obg <- graphics::par()$bg
        ifelse(missing(bg) == TRUE, bg <- graphics::par()$bg, 
            NA)
        graphics::par(bg = bg)
        ifelse(is.null(main) == TRUE, graphics::par(mar = mar), 
            graphics::par(mar = mar + c(0, 0, cex.main, 0)))
        ifelse(missing(ffamily) == FALSE && isTRUE(ffamily %in% 
            names(grDevices::postscriptFonts())) == TRUE, graphics::par(family = ffamily), 
            NA)
        if (missing(lbs) == FALSE) {
            X <- Rgraphviz::plot(methods::as(po, Class = "graphNEL"), 
                attrs = attrs, main = main, cex.main = cex.main, 
                col.main = col.main, ...)
            yp <- vector()
            xp <- vector()
            for (i in seq_along(X@AgNode)) {
                xp[length(xp) + 1L] <- X@AgNode[[i]]@center@x
                yp[length(yp) + 1L] <- X@AgNode[[i]]@center@y
            }
            rm(i)
            if (missing(fcol) == TRUE) {
                if (missing(fstyle) == TRUE || (missing(fstyle) == 
                  FALSE && isTRUE(fstyle %in% c("italic", "bold", 
                  "bolditalic") == FALSE))) {
                  graphics::text(cbind(xp, yp), labels = lbs, 
                    cex = fsize, ...)
                }
                else if (missing(fstyle) == FALSE) {
                  if (isTRUE(fstyle == "italic") == TRUE) {
                    graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                      function(Xx) bquote(italic(.(Xx))))), cex = fsize, 
                      ...)
                  }
                  else if (isTRUE(fstyle == "bold") == TRUE) {
                    graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                      function(Xx) bquote(bold(.(Xx))))), cex = fsize, 
                      ...)
                  }
                  else if (isTRUE(fstyle == "bolditalic") == 
                    TRUE) {
                    graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                      function(Xx) bquote(bolditalic(.(Xx))))), 
                      cex = fsize, ...)
                  }
                }
            }
            else {
                if (missing(fstyle) == TRUE || (missing(fstyle) == 
                  FALSE && isTRUE(fstyle %in% c("italic", "bold", 
                  "bolditalic") == FALSE))) {
                  graphics::text(cbind(xp, yp), labels = lbs, 
                    col = fcol, cex = fsize, ...)
                }
                else if (missing(fstyle) == FALSE) {
                  if (isTRUE(fstyle == "italic") == TRUE) {
                    graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                      function(Xx) bquote(italic(.(Xx))))), col = fcol, 
                      cex = fsize, ...)
                  }
                  else if (isTRUE(fstyle == "bold") == TRUE) {
                    graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                      function(Xx) bquote(bold(.(Xx))))), col = fcol, 
                      cex = fsize, ...)
                  }
                  else if (isTRUE(fstyle == "bolditalic") == 
                    TRUE) {
                    graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                      function(Xx) bquote(bolditalic(.(Xx))))), 
                      col = fcol, cex = fsize, ...)
                  }
                }
            }
            graphics::par(new = FALSE)
        }
        else if (missing(lbs) == TRUE) {
            Rgraphviz::plot(methods::as(po, Class = "graphNEL"), 
                attrs = attrs, main = main, cex.main = cex.main, 
                col.main = col.main, ...)
        }
        graphics::par(mar = omr)
        graphics::par(bg = obg)
        graphics::par(lend = 0)
        graphics::par(mai = omi)
    }
    else {
        stop("Package \"Rgraphviz\" is required for Hasse and Concept diagrams.")
    }
}
