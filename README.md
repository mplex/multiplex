

[![CRAN version](http://www.r-pkg.org/badges/version/multiplex)](https://cran.r-project.org/package=multiplex)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/multiplex)](https://cran.rstudio.com/web/packages/multiplex/index.html)


<br />

# multiplex
#### Author: Antonio Rivero Ostoic (@mplex)

<br />


### Abstract

Algebraic procedures for the analysis of multiple social networks are delivered with 
[this package](https://cran.r-project.org/web/packages/multiplex/index.html) 
as described in Ostoic (2020) <DOI:10.18637/jss.v092.i11>. 

* `"multiplex"` makes possible, among other things, to create and manipulate multiplex, multimode, and 
multilevel network data with different formats. 

* Effective ways are available to treat multiple networks with routines that combine algebraic systems like the partially ordered 
semigroup with decomposition procedures or semiring structures with the relational 
bundles occurring in different types of multivariate networks. 

* `"multiplex"` provides also an algebraic approach for affiliation networks through Galois derivations between families 
of the pairs of subsets in the two domains of the network with visualization options.


<br />

* * *
<br />

### Example: Partially Ordered Semigroup of Relations

```r
### create network data: two types of relations among three elements
set.seed(123)
arr <- round( replace( array(runif(18), c(3,3,2)), array(runif(18),
        c(3,3,2))>.5, 3 ) )
```


```r
### dichotomize data with customized cutoff value
dichot(arr, c = 3)
```



```r
### string relations
strings(arr)
strings(arr, equat = TRUE, k = 3)
```


```r
### create numerical or symbolic semigroup
semigroup(arr)
semigroup(arr, type = "symbolic")
```

```r
### Green's relations of symbolic semigroup
semigroup(arr, type = "symbolic") |> 
  green.rel()
```



```r
### create the partial order
partial.order(strings(arr), type = "strings")
```


```r
### plot the partial order (requires "Rgraphviz")
if(require("Rgraphviz", quietly = TRUE)) {
   diagram(partial.order(strings(arr), type = "strings"))
   }
```

<br />

* * *

<br />

### Example: Working with a Two-Mode Network data set
<i>(taken from the multiplex [vignette](https://cran.r-project.org/web/packages/multiplex/vignettes/TwoModeNetworks.pdf))</i>

```r
### Fruits data
frt <- data.frame(yellow = c(0,1,0,0,1,0,0,0), green = c(0,0,1,0,0,0,0,1), 
                  red = c(1,0,0,1,0,0,0,0), orange = c(0,0,0,0,0,1,1,0), 
                  apple = c(1,1,1,1,0,0,0,0), citrus = c(0,0,0,0,1,1,1,1))
rownames(frt) <- c("PinkLady", "GrannySmith", "GoldenDelicious", "RedDelicious", 
                   "Lemon", "Orange", "Mandarin", "Lime")

```


```r
### Perform Galois connections among subsets with a reduced labeling
gc <- galois(frt, labeling = "reduced")
```


```r
### Get the partial order of these "concepts"
pogc <- partial.order(gc, type = "galois")
```


```r
### Plot the concept lattice of the partial order
if(require("Rgraphviz", quietly = TRUE)) {
   diagram(pogc)
   }
```

