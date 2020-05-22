JmaxT <- function(Jmax, Ea, Ed, delS, Tair){
    Rgas <- 8.314
    Abszero <- 273.15
    Tref <- 25 + Abszero
    Tk <- Tair + Abszero
    jmaxt <- Arrh(Jmax, Ea, Tair) * (1 + exp((delS * Tref - Ed) / Rgas / Tref)) / 
        (1 + exp((delS * Tk - Ed) / Rgas / Tk))
    
    # return
    return(jmaxt)
}