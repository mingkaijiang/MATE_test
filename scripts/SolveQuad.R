SolveQuad <- function(Gs, Vmax, Cs, Gamstar, Km){
    A <- 1 / Gs
    B <- -(Vmax / Gs) - Cs - Km
    C <- Vmax * (Cs - Gamstar)
    Discrim <- B * B - 4 * A * C
    SolveQuad <- ifelse (Discrim > 0, (-B - sqrt(Discrim)) / (2 * A), 0)
    return(SolveQuad)
}





#SolveQuad <- function(Gs, Vmax, Cs, Gamstar, Km){
#	A <- 1 / Gs
#	B <- -Vmax / Gs - Cs - Km
#	C <- Vmax * (Cs - Gamstar)
#	Discrim <- B * B - 4 * A * C
#	solvq <- 0
#	if(Discrim > 0) solvq <- (-B - sqrt(Discrim)) / (2 * A)
#return(solvq)
#}
