calculate_quantum_efficiency <- function (CiCa, Ca, GamStar) {
    
    
    Ci <- Ca * CiCa
    
    alpha_j <- 0.26
    
    a1 <- alpha_j / 4.0
    a2 <- 2.0 * GamStar
    
    return(a1 * (Ci - GamStar) / (a2 + Ci))
}