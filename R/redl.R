## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
redl <-
function (a, b) 
{
    red = list(full = a, reduc = b)
    class(red) = c("Galois", "reduced")
    red
}

