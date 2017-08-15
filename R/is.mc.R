is.mc <-
function (B, C, A, ord = NULL) 
{
    return(!(all(dichot(trnf(B, tolist = FALSE, ord = ord) + 
        trnf(C, tolist = FALSE, ord = ord), c = 2) == trnf(A, 
        tolist = FALSE, ord = ord))))
}
