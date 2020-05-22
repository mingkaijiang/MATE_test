LeafPsi <- function(SoilPsi, Ksr, MaxT, psiLmin, psiLmax){
    LeafPsi <- ifelse(SoilPsi < psiLmin, SoilPsi,
                      ifelse(SoilPsi - MaxT / Ksr > psiLmax,SoilPsi - MaxT / Ksr,
                             (Ksr * SoilPsi * (psiLmax - psiLmin) + psiLmin * MaxT) / (MaxT + Ksr * (psiLmax - psiLmin))))
    return(LeafPsi )
}