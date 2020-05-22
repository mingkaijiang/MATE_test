# Leuning ci/ca
CiDivCa <-  function(camb, gamma, m, vpd, vpdo){
    
    bb <- 0
    
    cica <- gamma / camb
    
    if(vpd < vpdo) {
        bb <- m * (1 - vpd / vpdo)
    }
    
    if(bb > 0) {
        cica <- ((1 - 1 / bb) + sqrt((1 - 1 / bb) * (1 - 1 / bb) + 4 * gamma / (bb * camb))) * 0.5
    }
    
    # return
    return(cica)
}