## (GPL-3) Antonio Rivero Ostoic, multiplex@post.com
coor <-
function (a, b) 
{
    out = list(net = a, coord = b)
    class(out) = c("GML")
    out
}

