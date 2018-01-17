diagram <-
function (x, unord = TRUE, attrs = NULL, main = NULL, cex.main, 
    bg, mar, shape, tcex, col, col0, tcol, ecol, lty, lbs, ...) 
{
    if ((is.array(x) == FALSE | is.matrix(x) == FALSE) && isTRUE(attr(x, 
        "class")[1] == "Partial.Order") == FALSE) 
        stop("'x' must be either a matrix or an array object.")
    if (requireNamespace("Rgraphviz", quietly = TRUE)) {
        ifelse(missing(shape) == TRUE || shape %in% c("rectangle", 
            "rect", "circle", "box", "ellipse") == FALSE, shape <- "rectangle", 
            NA)
        ifelse(missing(tcex) == TRUE, fontsize <- 14, fontsize <- tcex)
        ifelse(missing(col) == TRUE, fillcolor <- "transparent", 
            fillcolor <- col)
        ifelse(missing(col0) == TRUE, color <- "transparent", 
            color <- col0)
        ifelse(missing(tcol) == TRUE, colort <- "#000000", colort <- tcol)
        ifelse(missing(ecol) == TRUE, colore <- "#000000", colore <- ecol)
        ifelse(missing(lty) == TRUE, lty <- "solid", NA)
        if (missing(lbs) == TRUE) {
            label <- ""
        }
        else {
            label <- " "
            colort <- "transparent"
        }
        if (is.null(attrs) == TRUE) 
            attrs = list(graph = list(rankdir = "BT"), edge = list(arrowsize = "0", 
                style = "lty", minlen = "1", color = colore), 
                node = list(shape = shape, color = color, fixedsize = FALSE, 
                  fontsize = fontsize, fillcolor = fillcolor, 
                  fontcolor = colort, label = label))
        po <- x & (1L - t(x))
        diag(po) <- 0L
        if (missing(lbs) == TRUE && is.null(dimnames(x)[[1]]) == 
            TRUE) {
            rownames(po) <- colnames(po) <- as.character(utils::as.roman(c(seq_len(dim(x)[1]))))
        }
        else if (missing(lbs) == FALSE) {
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
        if (unord == FALSE) {
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
        obg <- graphics::par()$bg
        ifelse(missing(bg) == TRUE, bg <- graphics::par()$bg, 
            NA)
        graphics::par(bg = bg)
        opm <- graphics::par()$mar
        ifelse(missing(mar) == TRUE, mar <- graphics::par()$mar, 
            NA)
        ifelse(is.null(main) == TRUE, graphics::par(mar = mar), 
            graphics::par(mar = mar + c(0, 0, cex.main, 0)))
        if (missing(lbs) == FALSE) {
            X <- Rgraphviz::plot(methods::as(po, "graphNEL"), 
                attrs = attrs, main = main, cex.main = cex.main, 
                ...)
            yp <- vector()
            xp <- vector()
            for (i in seq_len(length(X@AgNode))) {
                xp[length(xp) + 1L] <- X@AgNode[[i]]@center@x
                yp[length(yp) + 1L] <- X@AgNode[[i]]@center@y
            }
            rm(i)
            if (missing(tcol) == TRUE) {
                graphics::text(cbind(xp, yp), labels = lbs, ...)
            }
            else {
                graphics::text(cbind(xp, yp), labels = lbs, col = tcol, 
                  ...)
            }
            graphics::par(new = FALSE)
        }
        else {
            Rgraphviz::plot(methods::as(po, "graphNEL"), attrs = attrs, 
                main = main, cex.main = cex.main, ...)
        }
        graphics::par(mar = opm)
        graphics::par(bg = obg)
    }
    else stop("Package 'Rgraphviz' needs to be properly installed")
}
