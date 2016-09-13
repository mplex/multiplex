[![Build Status](https://travis-ci.org/mplex/multiplex.svg?branch=master)](https://travis-ci.org/mplex/multiplex)
[![CRAN version](http://www.r-pkg.org/badges/version/multiplex)](https://cran.r-project.org/package=multiplex)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/multiplex)](https://cran.rstudio.com/web/packages/multiplex/index.html)


<br />

# multiplex
#### Author: Antonio Rivero Ostoic (@mplex)

<br />


### Abstract

Algebraic procedures for the analysis of multiple social networks are delivered with [this package](https://cran.r-project.org/web/packages/multiplex/index.html). Among other things, it is possible to create and manipulate multivariate network data with different formats, and there are effective ways available to treat multiple networks with routines that combine algebraic systems like the partially ordered semigroup or the semiring structure together with the relational bundles occurring in different types of multivariate network data sets. As well an algebraic approach for two-mode networks is made through Galois derivations between families of the pair of subsets.

<br />

* * *
<br />

### Example: Partially Ordered Semigroup of Relations

```{r }
#### create the data: two types of relations among three elements
set.seed(123)
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
        c(3,3,2))>.5, 3 ) )
```


```{r }
#### dichotomize it with customized cutoff value
dichot(arr, c = 3)
```


```{r }
#### create the semigroup
semigroup(arr)
semigroup(arr, type = "symbolic")
```


```{r }
#### look at the strings
strings(arr)
strings(arr, equat = TRUE, k = 3)
```


```{r }
#### create the partial order
partial.order(strings(arr), type = "strings")
```


```{r }
#### plot the partial order (requires "Rgraphviz")
if( require("Rgraphviz", quietly = TRUE)) {
diagram(partial.order(strings(arr), type = "strings"))
}
```

<br />

* * *

<br />

### Example: Working with a Two-Mode Network data set
<i>(taken from the multiplex [vignette](https://cran.r-project.org/web/packages/multiplex/vignettes/TwoModeNetworks.pdf))</i>

```{r }
#### Fruits data
frt <- data.frame(yellow = c(0,1,0,0,1,0,0,0), green = c(0,0,1,0,0,0,0,1), 
                  red = c(1,0,0,1,0,0,0,0), orange = c(0,0,0,0,0,1,1,0), 
                  apple = c(1,1,1,1,0,0,0,0), citrus = c(0,0,0,0,1,1,1,1))
rownames(frt) <- c("PinkLady", "GrannySmith", "GoldenDelicious", "RedDelicious", 
                   "Lemon", "Orange", "Mandarin", "Lime")

```


```{r }
#### Perform Galois connections among subsets with a reduced labeling
gc <- galois(frt, labeling = "reduced")
```


```{r }
#### Get the partial order of these "concepts"
pogc <- partial.order(gc, type = "galois")
```


```{r }
#### Plot the concept lattice of the partial order
if( require("Rgraphviz", quietly = TRUE)) {
diagram(pogc)
}
```
