diagram <-
function (x, type = c("hasse", "concept", "egg-box"), attrs = NULL, 
    main = NULL, incmp, cex.main, bg, mar, shape, col, col0, 
    fcol, ecol, lty, lbs, ffamily, fstyle, fsize, col.main, sep, 
    ...) 
{
    if (match.arg(type) == "concept" && isTRUE(class(x)[[2]] != 
        "galois") == TRUE) {
        message("Class of \"x\" is not \"galois\".")
    }
    else {
        invisible(NA)
    }
    if (match.arg(type) == "hasse" || match.arg(type) == "concept") {
        if ((is.array(x) == FALSE | is.matrix(x) == FALSE) && 
            isTRUE(attr(x, "class")[1] == "Partial.Order") == 
                FALSE) {
            if (is.matrix(x[[1]]) == TRUE) {
                x <- x[[1]]
            }
            else {
                stop("\"x\" must be matrix, array, or a \"Partial.Order\" class object.")
            }
        }
        if (requireNamespace("Rgraphviz", quietly = TRUE)) {
            hasse(x, attrs = attrs, main = main, incmp, cex.main, 
                bg, mar, shape, col, col0, fcol, ecol, lty, lbs, 
                ffamily, fstyle, fsize, col.main, sep, ...)
        }
        else {
            stop("Package \"Rgraphviz\" is required for Hasse and Concept diagrams.")
        }
    }
    else if (match.arg(type) == "egg-box") {
        ifelse(isTRUE(tolower(class(x)[1]) != "semigroup") == 
            TRUE, S <- as.semigroup(x), S <- x)
        Sclass <- class(S)[2]
        gs <- green.rel(S)
        if (isTRUE(length(unique(unlist(gs$clu))) > 1) == TRUE) {
            dcl <- gs$D
            class(dcl) <- c("Semigroup", Sclass, "Green.Rels", 
                "D-class")
            return(noquote(dcl))
        }
        else {
            message("Single L- and R-class found in semigroup. Green relations returned.")
            lrdcl <- vector(mode = "list", length = 3)
            lrdcl[1:3] <- gs[7:9]
            names(lrdcl) <- c("R", "L", "D")
            class(lrdcl) <- attr(gs, "class")
            return(lrdcl)
        }
    }
}
