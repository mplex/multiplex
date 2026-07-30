## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
diagram <-
function (x, type = c("hasse", "concept", "egg-box"), attrs = NULL, 
    main = NULL, incmp, shape = c("rectangle", "circle", "box", 
        "ellipse"), col, col0, fcol, ecol, lty, lbs, ffamily, 
    fstyle, fsize, cex.main, col.main, bg, mar, sep, ...) 
{
    if (match.arg(type) == "concept" && isTRUE("galois" %in% 
        class(x)) == FALSE) {
        warning(paste(sQuote(deparse(substitute(x))), " is not a ", 
            dQuote("Galois"), " class object.", sep = ""), call. = FALSE)
    }
    else {
        invisible(NA)
    }
    if (match.arg(type) == "hasse" || match.arg(type) == "concept") {
        if (isTRUE("partial.order" %in% tolower(class(x))) == 
            FALSE) {
            warning(paste(sQuote(deparse(substitute(x))), " is not a ", 
                dQuote("Partial.Order"), " class object.", sep = ""), 
                call. = FALSE)
            if (is.matrix(x[[1]]) == TRUE) {
                x <- x[[1]]
            }
            else {
                if (is.array(x) == FALSE | is.matrix(x) == FALSE) 
                  stop(paste(sQuote(deparse(substitute(x))), 
                    " must be matrix, array, or a ", dQuote("Partial.Order"), 
                    " class object.", sep = ""))
            }
        }
        else {
            NA
        }
        if (requireNamespace("Rgraphviz", quietly = TRUE)) {
            switch(match.arg(type), concept = {
                concept <- TRUE
            }, hasse = {
                ifelse(isTRUE("galois" %in% class(x)) == TRUE, 
                  concept <- TRUE, concept <- FALSE)
            })
            hasse(x, concept = concept, attrs = attrs, main = main, 
                incmp, shape, col, col0, fcol, ecol, lty, lbs, 
                ffamily, fstyle, fsize, cex.main, col.main, bg, 
                mar, sep, ...)
        }
        else {
            stop(paste("Package ", sQuote("Rgraphviz"), " not found.", 
                sep = ""))
        }
    }
    else if (match.arg(type) == "egg-box") {
        ifelse(isTRUE(tolower(class(x)[1]) != "semigroup") == 
            TRUE, S <- suppressWarnings(as.semigroup(x)), S <- x)
        Sclass <- class(S)[2]
        gs <- green.rel(S)
        if (isTRUE(length(unique(unlist(gs$clu))) > 1) == TRUE) {
            dcl <- gs$D
            class(dcl) <- c("Semigroup", Sclass, "Green.Rels", 
                "D-class")
            return(noquote(dcl))
        }
        else {
            warning("Single L- and R-class found in semigroup and Green relations returned.", 
                call. = FALSE)
            lrdcl <- vector(mode = "list", length = 3)
            lrdcl[1:3] <- gs[7:9]
            names(lrdcl) <- c("R", "L", "D")
            class(lrdcl) <- attr(gs, "class")
            return(lrdcl)
        }
    }
}
