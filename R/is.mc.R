is.mc <-
function (B, C, A, ord = NULL) 
{
    return(!(all(dichot(transf(B, "toarray", ord = ord) + transf(C, 
        "toarray", ord = ord), c = 2) == transf(A, "toarray", 
        ord = ord))))
}
