## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(size = 'footnotesize', background = '#FFFFFF', prompt = TRUE, strip.white = FALSE, comment = NA)
options(width=110)
knitr::knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1))  # smaller margin on top and right
})

## ----data, echo=-6------------------------------------------------------------------------------------------
# Fruits data set with attributes
frt <- data.frame(yellow = c(0,1,0,0,1,0,0,0), green = c(0,0,1,0,0,0,0,1), red = c(1,0,0,1,0,0,0,0), 
                  orange = c(0,0,0,0,0,1,1,0), apple = c(1,1,1,1,0,0,0,0), citrus = c(0,0,0,0,1,1,1,1))
# Label the objects
rownames(frt) <- c("PinkLady","GrannySmith","GoldenDelicious","RedDelicious","Lemon","Orange","Mandarin","Lime")
frt

## ----loadmultiplex------------------------------------------------------------------------------------------
# load package
library("multiplex")

## ----galoisFull---------------------------------------------------------------------------------------------
# Galois representation between objects and attributes
galois(frt)

## ----galoisReduc, echo=-2-----------------------------------------------------------------------------------
# Galois derivation with a reduced labeling
gd <- galois(frt, labeling = "reduced")
galois(frt, labeling = "reduced")

## ----strgaloisReduc, size='scriptsize'----------------------------------------------------------------------
str(gd$gc$full)

## ----partialorder, echo=-3----------------------------------------------------------------------------------
# Partial ordering of the formal concepts with established labels
pogdc <- partial.order(gd, type = "galois", lbs = paste("c", seq_len(length(gd$gc$full)), sep = ""))
pogdc

## ----pogc---------------------------------------------------------------------------------------------------
# Assign partial order of reduced context to 'pogd'
pogd <- partial.order(gd, type = "galois")

## ----diagrampogc, fig.pos='H', fig.width=4.5, fig.height=4.5, fig.align='center', fig.cap='Concept Lattice of fruits and color characteristics', echo=-1, small.mar=TRUE----
par(mar=c(0,0,0,0))
# Plot the lattice diagram
suppressPackageStartupMessages(library("Rgraphviz", quietly = TRUE))
diagram(pogd, type = "concept", main = "Fruits & Colors", fcol = "red", col.main = "blue")

## ----diaglevels, echo=TRUE----------------------------------------------------------------------------------
# Diagram levels
if( require("Rgraphviz", quietly = TRUE, warn.conflicts = FALSE)) {
diagram.levels(pogdc) }

## ----diaglevelsperm, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------
# Diagram levels with permutation
if( require("Rgraphviz", quietly = TRUE, warn.conflicts = FALSE)) {
diagram.levels(pogdc, perm = TRUE) }

## ----princfltrlbs, echo=TRUE--------------------------------------------------------------------------------
# Principal order filter of the third concept 
fltr(3, pogd)

## ----princfltrlbs2, echo=TRUE, eval=FALSE-------------------------------------------------------------------
# # Principal order filter of the concept with these labels
# fltr(c("red", "RedDelicious"), pogd)

## ----filter, echo=TRUE--------------------------------------------------------------------------------------
# Order filter of two concepts
fltr(c("Lemon", "Lime"), pogd)

## ----ideal, echo=TRUE---------------------------------------------------------------------------------------
# Order ideal of two concepts
fltr(c(9, 11), pogd, ideal = TRUE)

## ----bipp, echo=TRUE, eval=TRUE, fig.pos='H', fig.width=3.0, fig.height=3.0, fig.align='center', fig.env='figure', fig.cap='Bipartite graph', small.mar=TRUE----
# Load the "multigraph" package and plot bipartite graph
suppressPackageStartupMessages(library("multigraph", quietly = TRUE))
bmgraph(frt, pch = 16:15, fsize = 6)

## ----binp2, echo=TRUE, eval=FALSE---------------------------------------------------------------------------
# # Plot proyection of bipartite network
# bmgraph(frt, layout = "force", seed = 1, cex = 3, fsize = 7, vcol = 8, pch = 16:15, rot = 25)

## ----binp, fig.pos='H', fig.width=4.5, fig.height=4.5, fig.align='center', fig.env='figure', fig.cap='Bipartite graph with a force-directed layout', echo=FALSE, small.mar=TRUE----
bmgraph(frt, layout = "force", seed = 1, cex = 3, fsize = 7, vcol = 8, pch = 16:15, rot = 25)

