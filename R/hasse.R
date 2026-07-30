## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
hasse <-
function (x, concept, attrs = NULL, main = NULL, incmp, shape = c("rectangle", 
    "circle", "box", "ellipse"), col, col0, fcol, ecol, lty, 
    lbs, ffamily, fstyle, fsize, cex.main, col.main, bg, mar, 
    sep, ...) 
{
    if (requireNamespace("Rgraphviz", quietly = TRUE)) {
        if (isTRUE(shape %in% c("rectangle", "rect", "circle", 
            "box", "ellipse") == FALSE) == TRUE) {
            ifelse(isTRUE(shape == "rectangle") == TRUE, NA, 
                warning(paste("Unknown shape for ", dQuote(deparse(substitute(x))), 
                  ", assume ", sQuote("rectangle"), ".", sep = ""), 
                  call. = FALSE))
            shape <- "rectangle"
        }
        else {
            NA
        }
        ifelse(missing(col) == TRUE, fillcol <- "transparent", 
            fillcol <- col)
        ifelse(missing(col0) == TRUE, col0 <- "transparent", 
            NA)
        ifelse(missing(ecol) == TRUE, ecol <- "#000000", NA)
        ifelse(missing(lty) == TRUE, lty <- "solid", NA)
        if (missing(concept) == TRUE) {
            ifelse(isTRUE("galois" %in% class(x)) == TRUE, concept <- TRUE, 
                concept <- FALSE)
        }
        else {
            ifelse(isTRUE(class(concept) != "logical") == TRUE, 
                warning(paste("Value in ", sQuote("concept"), 
                  " should be TRUE/FALSE.", sep = ""), call. = FALSE), 
                NA)
        }
        po <- x & (1L - t(x))
        diag(po) <- 0L
        if (missing(lbs) == TRUE) {
            if (is.null(dimnames(x)[[1]]) == TRUE) {
                rownames(po) <- colnames(po) <- as.character(utils::as.roman(c(seq_len(dim(x)[1]))))
                lbs <- attr(po, "dimnames")[[1]]
            }
            else {
                lbs <- attr(x, "dimnames")[[1]]
            }
        }
        else {
            ifelse(isTRUE(length(lbs) != dim(po)[1]) == TRUE, 
                lbs <- lbs[seq_len(nrow(po))], NA)
        }
        if (isTRUE(length(lbs) == 1L) == TRUE && isTRUE(dim(x)[1] > 
            1L) == TRUE) {
            tlbs <- vector(mode = "list", length = dim(x)[1])
            for (i in seq_len(dim(x)[1])) tlbs[[i]] <- lbs[[1]][[i]]
            lbs <- tlbs
            rm(tlbs)
        }
        else {
            NA
        }
        ifelse(missing(sep) == TRUE, sep <- ",", NA)
        ifelse(is.list(lbs) == TRUE, lbs <- unlist(jnt(lbs, sep = sep)), 
            NA)
        if (missing(fsize) == TRUE) {
            if (isTRUE(concept == TRUE) == TRUE) {
                fsize <- 20L/max(nchar(lbs))
            }
            else {
                fsize <- 20L/dim(x)[1]
                ifelse(isTRUE(fsize < 1L) == TRUE, fsize <- 1L * 
                  ((1L/dim(x)[1] * dim(x)[1])), NA)
            }
        }
        else {
            if (isTRUE(concept == TRUE) == TRUE) {
                fsize <- fsize/6L
            }
            else {
                NA
            }
        }
        if (is.null(attrs) == TRUE) {
            attrs = list(graph = list(rankdir = "BT"), edge = list(arrowsize = "0", 
                style = "lty", minlen = "1", color = ecol), node = list(shape = shape, 
                color = col0, fixedsize = FALSE, fontsize = fsize, 
                fillcolor = fillcol, fontcolor = "transparent", 
                label = ""))
        }
        rownames(po) <- colnames(po) <- NULL
        if (isTRUE(ncol(x) == nrow(x)) == TRUE) {
            for (i in seq_len(ncol(po))) {
                tmp <- outer(po[, i], po[i, ], pmin.int)
                po <- pmin(po, (1L - tmp))
            }
            rm(tmp)
        }
        else {
            stop("Binary operation on non-conformable arrays.")
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
        options(warn = -1)
        X <- invisible(Rgraphviz::plot(methods::as(po, Class = "graphNEL"), 
            attrs = attrs, main = main, cex.main = cex.main, 
            col.main = col.main, ...))
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
                graphics::text(cbind(xp, yp), labels = lbs, cex = fsize, 
                  ...)
            }
            else if (missing(fstyle) == FALSE) {
                if (isTRUE(fstyle == "italic") == TRUE) {
                  graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                    function(z) bquote(italic(.(z))))), cex = fsize, 
                    ...)
                }
                else if (isTRUE(fstyle == "bold") == TRUE) {
                  graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                    function(z) bquote(bold(.(z))))), cex = fsize, 
                    ...)
                }
                else if (isTRUE(fstyle == "bolditalic") == TRUE) {
                  graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                    function(z) bquote(bolditalic(.(z))))), cex = fsize, 
                    ...)
                }
            }
        }
        else {
            if (missing(fstyle) == TRUE || (missing(fstyle) == 
                FALSE && isTRUE(fstyle %in% c("italic", "bold", 
                "bolditalic") == FALSE))) {
                graphics::text(cbind(xp, yp), labels = lbs, col = fcol, 
                  cex = fsize, ...)
            }
            else if (missing(fstyle) == FALSE) {
                if (isTRUE(fstyle == "italic") == TRUE) {
                  graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                    function(z) bquote(italic(.(z))))), col = fcol, 
                    cex = fsize, ...)
                }
                else if (isTRUE(fstyle == "bold") == TRUE) {
                  graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                    function(z) bquote(bold(.(z))))), col = fcol, 
                    cex = fsize, ...)
                }
                else if (isTRUE(fstyle == "bolditalic") == TRUE) {
                  graphics::text(cbind(xp, yp), labels = as.expression(lapply(lbs, 
                    function(z) bquote(bolditalic(.(z))))), col = fcol, 
                    cex = fsize, ...)
                }
            }
        }
        options(warn = 0)
        graphics::par(new = FALSE)
        graphics::par(mar = omr)
        graphics::par(bg = obg)
        graphics::par(lend = 0)
        graphics::par(mai = omi)
    }
    else {
        stop(paste("Package ", dQuote("Rgraphviz"), " not found.", 
            sep = ""))
    }
}
