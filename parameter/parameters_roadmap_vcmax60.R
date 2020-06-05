#### prepare parameter settings

#### Control parameters
FixLAI <- 1.0   ## 1 fix by input, 0 vary by carbon stock

mateoption <- 0.0  # 0 = Ross (Leuning ci/ca, PAW function), 1 = JEES (SWP, leaf water potential)

OptimW <- 0.0
OptimV <- 0.0

cicamodel <- 1.0   # 1 = Leuning

WUEmodel <- 1.0    # 1 = based on Ci/Ca

Latitude <- -33.62
LAIcover <- 0.5
Kext <- 0.5


#### Light use efficiency and NPP
Cfracts <- 0.48
Alpha <- 0.05
Vcmax25 <- 60.0
Jmax25 <- 120.0
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
Gamma <- 54.5
Rd <- 0.0

### Canopy development
Shootz <- 1.25   # initial leaf carbon biomass (t/ha)
Sapwoodz <- 1.0
Stemz <- 2.5
Branchz <- 1.25
Rootz <- 1.25
Density <- 420
Heighto <- 4.8295
Htpower <- 0.34597
FormFactor <- 0.33

### Carbon allocation model (trees only)
## Leaf / stem allocation (trees)
Callocf <- 0.47
Leafsap0 <- 7500  # leaf to sapwood ratio when height = height0
Leafsap1 <- 2700  # leaf / sapwood ratio when height = height1
Height0 <- 5.0
Height1 <- 30.0
Allocsensf <- 0.5
Siginit <- 5.09434   # initial SLA (m2/kg DM)
Signew <- 7.085      # SLA of new leaf

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

Leuningm1 <- 4.0
Leuningmo <- 1.0
LeuningDo <- 6.0
PAWcrit <- 100.0
WUE0 <- 15.0

### Water balance
Rfmult <- 1.0  ### water balance variable, not control parameter
PAW0 <- 250.0
PAW1 <- 50.0
Wcapac <- 250
Wcapac1 <- 50.0
phiE <- -0.68
b <- 2.79
phiLmax <- -1.4
phiLmin <- -4.0
ksr <- 4.0
WUE0 <- 4.0
Wetloss <- 0.5
Albedo <- 0.2
Fractup1 <- 0.6

### grass stuffs
LAI2 <- 0.0    ## lai for grass layer
Kext2 <- 0.5
LUE2 <- 1.3
Fwgmax <- 0.6
Fwgmin <- 0.0
Fvgmax <- 100.0
Fvgmin <- 1000.0
