Arrh <- function(Kc, Ea, Tair){
    Rgas <- 8.314
    Abszero <- 273.15
    arh <- Kc * exp(Ea * (Tair - 25) / Rgas / (Tair + Abszero) / (25 + Abszero))
    return(arh)
}