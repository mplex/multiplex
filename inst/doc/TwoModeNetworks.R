## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(size = "footnotesize", background = "#FFFFFF", prompt = TRUE, strip.white = FALSE, comment = NA)
options(width=110)
knitr::knit_hooks$set(small.mar = function(before, options, envir) {
    if (before) par(mar = c(4, 4, .1, .1)) 
})

## ----data, echo=-6------------------------------------------------------------------------------------------
# fruits data set with attributes
frt <- data.frame(yellow = c(0,1,0,0,1,0,0,0), green = c(0,0,1,0,0,0,0,1), red = c(1,0,0,1,0,0,0,0), 
                  orange = c(0,0,0,0,0,1,1,0), apple = c(1,1,1,1,0,0,0,0), citrus = c(0,0,0,0,1,1,1,1))
# fruit varieties as objects
rownames(frt) <- c("PinkLady","GrannySmith","GoldenDelicious","RedDelicious","Lemon","Orange","Mandarin","Lime")
frt

## ----loadmultiplex------------------------------------------------------------------------------------------
# load package
library("multiplex")

## ----galoisFull---------------------------------------------------------------------------------------------
# Galois derivations between fruits and their attributes
galois(frt)

## ----galoisReduc, echo=TRUE---------------------------------------------------------------------------------
# Galois derivation with a reduced labeling
frt |> galois(labeling = "reduced")

## ----strgaloisReduc, size="scriptsize"----------------------------------------------------------------------
# structure of Galois derivation with full labeling
frt |> 
  galois(labeling = "reduced") |> 
  getElement("gc") |> 
  getElement("full") |> 
  str()

## ----partialorder, echo=-5----------------------------------------------------------------------------------
# partial ordering of the formal concepts with established labels
pogdc <- frt |> 
  galois(labeling = "reduced") |> 
  partial.order(type = "galois", lbs = paste("c", seq_len(12), sep = ""))
pogdc

## ----diagrampogc, fig.pos="H", fig.width=4.5, fig.height=4.5, fig.align="center", fig.cap="Concept Lattice of fruits and color characteristics", echo=-1, small.mar=TRUE----
par(mar=c(0,0,0,0))
# plot concept lattice diagram
suppressPackageStartupMessages(require("Rgraphviz", quietly = TRUE))
frt |> galois(labeling = "reduced") |> 
  partial.order(type = "galois") |> 
  diagram(type = "concept", main = "Fruits & Colors", fsize = 17, fcol = "red", col.main = "blue")

## ----diaglevels, echo=TRUE----------------------------------------------------------------------------------
# diagram levels of partial order
require("Rgraphviz", quietly = TRUE, warn.conflicts = FALSE)
frt |> 
  galois(labeling = "reduced") |> 
  partial.order(type = "galois") |> 
  diagram.levels() 

## ----diaglevelsperm, echo=TRUE, message=FALSE, warning=FALSE------------------------------------------------
# diagram levels with permutation
if( require("Rgraphviz", quietly = TRUE, warn.conflicts = FALSE)) {
diagram.levels(pogdc, perm = TRUE) }

## ----pogc, echo=TRUE----------------------------------------------------------------------------------------
# first assign the partial order of the reduced context
pogd <- frt |> galois(labeling = "reduced") |> 
  partial.order(type = "galois")

## ----princfltrlbs, echo=TRUE--------------------------------------------------------------------------------
# principal order filter of the third concept 
fltr(3, pogd)

## ----princfltrlbs2, echo=TRUE, eval=TRUE--------------------------------------------------------------------
# principal order filter of the concept with these labels
fltr(c("red", "RedDelicious"), pogd)

## ----filter, echo=TRUE--------------------------------------------------------------------------------------
# order filter of two concepts
fltr(c("Lemon", "Lime"), pogd)

## ----ideal, echo=TRUE---------------------------------------------------------------------------------------
# order ideal of two concepts
fltr(c(9, 11), pogd, ideal = TRUE)

## ----bipp, echo=TRUE, eval=TRUE, fig.pos="H", fig.width=3.5, fig.height=3.5, fig.align="center", fig.env="figure", fig.cap="Bipartite graph of fruits with attributes", small.mar=TRUE----
# load "multigraph" package and plot bipartite graph
suppressPackageStartupMessages(library("multigraph", quietly = TRUE))
bmgraph(frt, pch = 16:15, lwd = 2, fsize = 6)

## ----binp2, echo=TRUE, eval=FALSE---------------------------------------------------------------------------
#  # plot proyection of bipartite graph
#  bmgraph(frt, layout = "force", seed = 1, cex = 3, fsize = 7, vcol = 8, pch = 16:15, lwd = 2, rot = 15)

## ----binp, fig.pos="H", fig.width=4.4, fig.height=4.4, fig.align="center", fig.env="figure", fig.cap="Bipartite graph of fruit attributes with a force-directed layout", echo=FALSE, small.mar=TRUE----
bmgraph(frt, layout = "force", seed = 1, cex = 3, fsize = 7, vcol = 8, pch = 16:15, lwd = 2, rot = 15)

