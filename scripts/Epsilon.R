# Integrate LUE
Epsilon <- function(alpha, amax, k, par, theta, daylen){
    gamma <- 2000000
    qq <- ifelse(amax > 0, pi * k * alpha * par  * gamma / (2 * daylen * 3600 * amax), 0)
    sin1 = sin(pi / 24)
    sin2 = sin(pi * 3 / 24)
    sin3 = sin(pi * 5 / 24)
    sin4 = sin(pi * 7 / 24)
    sin5 = sin(pi * 9 / 24)
    sin6 = sin(pi * 11 / 24)
    g1 = sin1 / (1 + qq * sin1 + sqrt((1 + qq * sin1) ^ 2 - 4 * theta * qq * sin1))
    g2 = sin2 / (1 + qq * sin2 + sqrt((1 + qq * sin2) ^ 2 - 4 * theta * qq * sin2))
    g3 = sin3 / (1 + qq * sin3 + sqrt((1 + qq * sin3) ^ 2 - 4 * theta * qq * sin3))
    g4 = sin4 / (1 + qq * sin4 + sqrt((1 + qq * sin4) ^ 2 - 4 * theta * qq * sin4))
    g5 = sin5 / (1 + qq * sin5 + sqrt((1 + qq * sin5) ^ 2 - 4 * theta * qq * sin5))
    g6 = sin6 / (1 + qq * sin6 + sqrt((1 + qq * sin6) ^ 2 - 4 * theta * qq * sin6))
    
    #6-point Gaussian quadrature
    #gg = 0.08566225 * (g1 + g6) + 0.1803808 * (g2 + g5) + 0.233957 * (g3 + g4)
    
    #Trapezoidal rule - seems more accurate
    gg <- 0.16666666667 * (g1 + g2 + g3 + g4 + g5 + g6)
    eps <- ifelse(amax > 0,  alpha * gg * pi, 0)
    
    # return
    return(eps)
}