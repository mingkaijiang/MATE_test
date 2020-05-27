#### prepare parameter settings

#### Control parameters
FixLAI <- 0.0   ## 0 fix by input, 1 vary by carbon stock
VPDData <- 1.0
VPDUnits <- 2.0
OptimW <- 0.0
OptimV <- 0.0
Rfmult <- 1.0
#APAR <- NA
LAIcover <- 0.5
Kext <- 0.5
LAI2 <- 0.0
Latitude <- -35
Kext2 <- 0.5

#### Light use efficiency and NPP
#LUEmax <- NA
#Fco2 <- NA
Cfracts <- 0.48
Alpha <- 0.05
Vcmax25 <- 82.0
Jmax25 <- 191.0
mateoption <- 0.0
Theta <- 0.7
CiCa <- 0.8
CUE <- 0.5
Gsmax <- 400.0
EaV <- 51560
EaJ <- 43790
EdJ <- 200000
delSJ <- 644.4338
Fwpmax <- 0.6
Fwpmin <- 0.0
Fvpmax <- 1.0
Fvpmin <- 4.0
LUE2 <- 1.3
Fwgmax <- 0.6
Fwgmin <- 0.0
Fvgmax <- 100.0
Fvgmin <- 1000.0

### Water balance
PAW0 <- 250.0
PAW1 <- 50.0
Wcapac <- 250
Wcapac1 <- 50.0
phiE <- -0.68
b <- 2.79
phiLmax <- -1.4
phiLmin <- -4.0
ksr <- 4.0
#Watermin <- NA
WUE0 <- 4.0
Wetloss <- 0.5
Albedo <- 0.2
Fractup1 <- 0.6

### Canopy development
Shootz <- 0.01
Sapwoodz <- 0.01
Stemz <- 0.01
Branchz <- 0.009727332
Rootz <- 0.01
Density <- 420
Heighto <- 4.8295
Htpower <- 0.34597
FormFactor <- 0.33

### Carbon allocation model (trees only)
## Leaf / stem allocation (trees)
Callocf <- 0.47
Leafsap0 <- 7500
Leafsap1 <- 2700
Height0 <- 5.0
Height1 <- 30.0
Allocsensf <- 0.5
Siginit <- 5.09434
Signew <- 7.085
#FormFactor <- 1.0

## Branch / stem allocation (trees)
Callocb <- 0.42
Branch0 <- 0.54
Branch1 <- 0.8722
Allocsensb <- 0.5

## Root allocation (trees)
Callocr <- 0.3

### Biomass turnover (trees)
Fdecay <- 0.2
Fdecaydry <- 0.6
Watdecaywet <- 0.3
Watdecaydry <- 0.05
Rdecay <- 2.4
Bdecay <- 0.03
Wdecay <- 0.0
Sapturnover <- 0.1
Pi <- 3.141593

cicamodel <- 1.0
Leuningm1 <- 4.0
Leuningmo <- 1.0
LeuningDo <- 6.0
PAWcrit <- 100.0
Gamma <- 54.5
Rd <- 0.0
WUEmodel <- 1.0
WUE0 <- 15.0



    
    
    
    