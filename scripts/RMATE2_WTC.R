RMATE2_WTC <- function(matefile, 
                       outputfile,
                       sourcefile,
                       runfrom, 
                       nrows){
    
    ### read in parameters
    source(sourcefile)
    
    #::::: Read daily data
    MATEdailydata <- read.csv(matefile)
    
    # Convert Excel type date to a normal date string.
    MATEdailydata$Date <- as.Date(as.character(MATEdailydata$Date), format="%Y-%m-%d")
    
    # Start at 'runfrom', toss previous met data
    MATEdailydata <- MATEdailydata[runfrom:nrow(MATEdailydata),]
    
    # and stop somewhere, if provided
    #if(!is.na(nrows) & nrows > nrow(DD)){
    if(!is.na(nrows) & nrows > nrow(MATEdailydata)){
        MATEdailydata <- MATEdailydata[1:nrows,]
    }
    
    # Date in POSIXlt format
    DATES <- strptime(MATEdailydata$Date, format="%Y-%m-%d")
    
    # shorthand
    DD <- MATEdailydata
    
    # Declination, temperatures, VPD.
    DD$Declin <- 23.4 * pi/180*cos(2*pi/365*(DD$DOY+10)) * ifelse(Latitude<0,-1,1)
    DD$Daylen <- acos(-tan(Latitude*pi/180)*tan(DD$Declin))*24/pi
    DD$Tavday <- (DD$Tmin + DD$Tmax)/2+(DD$Tmax - DD$Tmin)/(3*pi)
    
    
    DD$VPD <- DD$VPDav 
    DD$GamStar <- Arrh(42.75,37830, DD$Tav)
    DD$Km <- Arrh(404.9, 79430, DD$Tav)*(1+205000/Arrh(278400,36830, DD$Tav))
    DD$Jm <- JmaxT(Jmax25,EaJ,EdJ,delSJ,DD$Tav)
    DD$Vm <- Arrh(Vcmax25,EaV,DD$Tav)

    # Number of simulations
    SIMROWS <- nrow(DD)
    
    #::::: Initialization (only needed for vectors).
    # Note that all vectors are now filled with NA, so that they can be output if not actually calculated.
    Leaf <-	Sapwood <- Stem <- Branch <- Root <- Lai <- Height <- LeafSapAreaActual <- LeafSapAreaTarget <- 
        TargetBranch <- Leafallocn <- Branchallocn <- Stemallocn <- TotalAlloc <- Leafturnoverrate <- Stemvol <- 
        rep(NA, SIMROWS)
    
    Fabs <- APAR <- fPAW <- fVPD <- soilPsi <- leafPsi <- fPAW <- PAWcur <- Transp <- m <- CiCa <- CiCaAv <- Gs <- Ac <- Aj <- Asat <- LUE <-
        LUE1000 <- LUEgCMJPAR <- NPPtCha <- NPPgCm2 <- CumNPP <- CumAPAR <- LAIgrass <- Fabsgrass <- 
        APARgrass <- fPAWgrass <- fVPDgrass  <- LUEgrass <- NPPtChagrass <- NPPgCm2grass <- totalNPP <-
        CumNPPgrass <- CumtotalNPP <- rep(NA, SIMROWS)
    
    PAW1cur <- WUE <- Transp <- Erain <- epsil <- Radlong <- Radnet <- Evapgrdvegorsoil <- Drainage <-	
        CumPrecip <- CumTransp <- CumInterception <- CumEvapsoil <- CumDrainage <- rep(NA, SIMROWS)
    
    #:::::::::: FOR LOOP :::::::::#
    
    # Start main loop
    for(i in 1:SIMROWS){
        
        # Update PAW first (PAW is total, PAW1 top layer).
        if(i == 1){
            PAWcur[i] <- PAW0
            PAW1cur[i] <- PAW1
        } else {
            PAWcur[i] <- max(0, min(Wcapac, PAWcur[i-1] + Erain[i-1] - Transp[i-1] - Evapgrdvegorsoil[i-1]))
            PAW1cur[i] <- max(0, min(Wcapac1, PAW1cur[i-1] + Erain[i-1] - Transp[i-1]*Fractup1 - Evapgrdvegorsoil[i-1]))
        }
        
        # Soil water potential
        soilPsi[i] <- phiE*((0.15+PAWcur[i]/2000)/(0.15+Wcapac/2000+0.01))^(-b)
        
        # Leaf turnover
        Leafturnoverrate[i] <- max(Fdecay, min(Fdecaydry,Fdecaydry-(Fdecaydry-Fdecay) * 
                                                   (PAWcur[i]/Wcapac-Watdecaydry)/(Watdecaywet-Watdecaydry)))
        
        # LAI
        if(i == 1){
            Lai[i] <- ifelse(FixLAI == 1 ,DD$LAI[i], Siginit/10/Cfracts*Shootz)
        } else {
            Lai[i] <- ifelse(FixLAI == 1, DD$LAI[i], Lai[i-1] + (NPPtCha[i-1]*Leafallocn[i-1]*Signew/10/Cfracts - 
                                                                     Leafturnoverrate[i-1]*Lai[i-1])/365.25)
        }
        
        # Simulate NPP
        Fabs[i] <- ifelse(Lai[i]==0,
                          0,
                          (1-exp(-Kext*Lai[i]/min(1,Lai[i]/LAIcover)))*min(1,Lai[i]/LAIcover))
        
        # The '2' converts from radiation to PAR, make this an input?
        APAR[i] <- DD$Radtot[i] * Fabs[i] * 2
        
        # fVPD is not actually used.
        fVPD[i] <- ifelse(OptimV==0,
                          max(0,min(1,(Fvpmin-DD$VPD[i])/(Fvpmin-Fvpmax))),
                          1)
        
        # JEES-MATE
        if(mateoption == 1){
            leafPsi[i] <- LeafPsi(soilPsi[i],ksr,Gsmax*fVPD[i]*DD$VPD[i]/100,phiLmin,phiLmax)
            fPAW[i] <- max(0,min(1,(leafPsi[i]-phiLmin)/(phiLmax-phiLmin)))

            Gs[i] <- Gsmax*fPAW[i]*fVPD[i]/1000

            Ac[i] <- SolveQuad(Gs[i]/1.6,DD$Vm[i],DD$Ca[i],DD$GamStar[i],DD$Km[i])		
            Aj[i] <- SolveQuad(Gs[i]/1.6,DD$Jm[i]/4,DD$Ca[i],DD$GamStar[i],2*DD$GamStar[i])

            AsatAM[i] <- ifelse(Gs[i]==0, 0, min(Aj[i], Ac[i]))
            
            LUE[i] <- Epsilon(Alpha*fPAW[i],Asat[i],Kext,DD$Radtot[i], Theta, DD$Daylen[i])

        }
        
        # ROSS-MATE
        if(mateoption == 0){
            
            # Slope parameter in the Leuning model
            m[i] <- min(Leuningm1,Leuningmo+(Leuningm1-Leuningmo)*PAWcur[i]/PAWcrit)
            if(OptimW == 1)m[i] <- Leuningm1
            
            # Different options for ci/ca model.
            if(cicamodel==1){
                CiCa[i] <- CiDivCa(DD$Ca[i],Gamma,m[i],DD$VPD[i],LeuningDo)
            }
            if(cicamodel==2){
                CiCa[i] <- 1-sqrt((1.6*(DD$VPD[i]/101) * (DD$Ca[i]-optimlambda) )/(optimlambda*DD$Ca[i]^2))
            }
            CiCaAv[i] <- CiCa[i] 
            
            # Vcmax limited assimilation rate
            Ac[i] <- max(0,(DD$Ca[i]*CiCa[i] - DD$GamStar[i]))/(DD$Ca[i]*CiCa[i]+ DD$Km[i])*DD$Vm[i]

            # Jmax limited (at light saturation)
            Aj[i] <- (DD$Jm[i]/4) * ((DD$Ca[i]*CiCa[i] - DD$GamStar[i])/(DD$Ca[i]*CiCa[i] + 2*DD$GamStar[i]))

            # Note that these are gross photosynthetic rates.
            Asat[i] <- min(Aj[i], Ac[i])

            ### Quantum efficiency
            Alpha <- calculate_quantum_efficiency(CiCaAv[i], DD$Ca[i], DD$GamStar[i])
            
            # LUE
            LUE[i] <- Epsilon(Alpha,Asat[i],Kext,DD$Radtot[i], Theta, DD$Daylen[i])

        }
        
        # mol C mol-1 PAR

        # Obsolete: was for drawing graphs in Excel-MATE
        LUE1000[i] <- 1000*LUE[i] 
        
        # Conversions
        LUEgCMJPAR[i] <- LUE[i]*12*4
        NPPgCm2[i] <- APAR[i]*LUE[i]*CUE*12
        NPPtCha[i] <- NPPgCm2[i] * 365.25 * 10^-6 * 10^4
        
        # Cumulative totals
        if(i == 1){
            CumNPP[i] <- NPPgCm2[i]
            CumAPAR[i] <- APAR[i]
            CumNPPgrass[i] <- NPPgCm2grass[i]
            CumtotalNPP[i] <- ifelse(is.na(CumNPPgrass[i]) == T, CumNPP[i], CumNPP[i] + CumNPPgrass[i])
        } else {
            CumNPP[i] <- CumNPP[i-1] + NPPgCm2[i]
            CumAPAR[i] <- CumAPAR[i-1] + APAR[i]
            CumNPPgrass[i] <- CumNPPgrass[i-1] + NPPgCm2grass[i]
            CumtotalNPP[i] <- ifelse(is.na(CumNPPgrass[i]) == T, CumtotalNPP[i-1] + CumNPP[i], CumtotalNPP[i-1] + CumNPP[i] + CumNPPgrass[i])
        }
        
        # Optional grass layer
        LAIgrass[i] <- LAI2
        Fabsgrass[i] <- 1-exp(-Kext2*LAI2)
        APARgrass[i] <- Fabsgrass[i]*(DD$Radtot[i]- APAR[i])
        fPAWgrass[i] <- min(1, max(0,(PAWcur[i]/Wcapac1-Fwgmin)/(Fwgmax-Fwgmin)))
        fVPDgrass[i] <- max(0, min(1,(Fvgmin - DD$VPDav)/(Fvgmin-Fvgmax)))
        LUEgrass[i] <- LUE2*fPAWgrass[i]*fVPDgrass[i]
        NPPtChagrass[i] <- Cfracts*LUEgrass[i]*365.25/100*APARgrass[i]
        NPPgCm2grass[i] <- NPPtChagrass[i]*100/365.25
        totalNPP[i] <- NPPgCm2[i] + NPPgCm2grass[i]
        
        # SimulateH2O
        
        # JEES-MATE
        if(mateoption == 1){
            Transp[i] <- ifelse(Asat[i]>0,
                                  Gs[i]*LUE[i]*APAR[i]/2/Asat[i]*18*10^3*DD$VPD[i]/100,
                                  0)
        }
        # ROSS-MATE	
        if(mateoption == 0){
            if(WUEmodel==1){
                WUE[i] <- (12/18)*1000*(DD$Ca[i]*10^-6*(1-CiCaAv[i])/(1.6*DD$VPDav[i]/101))
                #if(WUE[i] > 20) WUE[i] <- 20    # FIX THIS!!!
            }
            if(WUEmodel==2){
                WUE[i] <- WUE0*0.27273/DD$VPDav[i]*DD$Ca[i]/380
            }
            
            Transp[i] <- NPPgCm2[i]/CUE/WUE[i]
        }
        
        # Water balance components
        Erain[i] <- max(0,DD$Precip[i] * Rfmult - Wetloss*Lai[i])
        epsil[i] <- 250300 * exp( 17.269 * DD$Tavday[i] / (DD$Tavday[i] + 237.3) ) / 
            (DD$Tavday[i] + 237.3)^2 / (6.46 + 0.006*DD$Tavday[i] )
        Radlong[i] <- 0.0036*12*(107 - DD$Tavday[i] )
        Radnet[i] <- max(0, DD$Radtot[i]*(1-Albedo)-Radlong[i])
        
        # Understorey evapotranspiration (radiation driven)
        Evapgrdvegorsoil[i] <- min(Radnet[i] * exp(-0.398*Lai[i])*epsil[i]/2.47/(1 + epsil[i] ) * 
                                       min(1, max(0,(PAW1cur[i]/Wcapac1-Fwgmin)/(Fwgmax-Fwgmin))), PAW1cur[i]+Erain[i])
        Drainage[i] <- max(0, PAWcur[i] + Erain[i] - Transp[i] - Evapgrdvegorsoil[i] - Wcapac)   
        
        # Cumulative water balance
        if(i == 1){
            CumPrecip[i] <- Rfmult*DD$Precip[i]
            CumTransp[i] <- Transp[i]
            CumInterception[i] <- Rfmult*DD$Precip[i] - Erain[i]
            CumEvapsoil[i] <- Evapgrdvegorsoil[i]
            CumDrainage[i] <- Drainage[i]
        } else {
            CumPrecip[i] <- Rfmult*DD$Precip[i]
            CumTransp[i] <- Transp[i]
            CumInterception[i] <- Rfmult*DD$Precip[i] - Erain[i]
            CumEvapsoil[i] <- Evapgrdvegorsoil[i]
            CumDrainage[i] <- Drainage[i]
        }
        
        # SimulateGrowth
        if(i == 1){
            Leaf[1] <- Shootz
            Stem[1] <- Stemz
            Sapwood[1] <- Sapwoodz
            Branch[1] <- Branchz
            Root[1] <- Rootz	
        } else {
            Leaf[i] <- Leaf[i-1] + (NPPtCha[i-1]*Leafallocn[i-1] - Leafturnoverrate[i-1]*Lai[i-1])/365.25
            Sapwood[i] <- Sapwood[i-1] + (NPPtCha[i-1]*Stemallocn[i-1]-(Wdecay+Sapturnover)*Sapwood[i-1] )/365.25
            Stem[i] <- Stem[i-1] + (NPPtCha[i-1]*Stemallocn[i-1] - Wdecay*Stem[i-1])/365.25
            Branch[i] <- Branch[i-1] + (NPPtCha[i-1]*Branchallocn[i-1] - Bdecay*Branch[i-1])/365.25
            Root[i] <- Root[i-1] + (NPPtCha[i-1]*Callocr - Rdecay*Root[i-1])/365.25
        }
        
        # Height, LA/SA, allocation
        Height[i] <- Heighto*Stem[i]^Htpower
        LeafSapAreaActual[i] <- Lai[i]/((1/FormFactor)*Sapwood[i]/Cfracts/10)*Height[i]*Density
        LeafSapAreaTarget[i] <- max(min(Leafsap0+(Leafsap1-Leafsap0)*(Height[i]-Height0)/(Height1-Height0),
                                        max(Leafsap0,Leafsap1)), min(Leafsap0,Leafsap1))
        TargetBranch[i] <- Branch0*Stem[i]^Branch1
        Leafallocn[i] <- Callocf*max(min(0.5+0.5*(1-LeafSapAreaActual[i]/LeafSapAreaTarget[i])/Allocsensf,1),0)
        
        if(mateoption == 0){ # original (see Corbeels et al. 2005)
            Branchallocn[i] <- Callocb*max(min(0.5+0.5*(1- Branch[i] / TargetBranch[i] ) /Allocsensb,1),0)
            Stemallocn[i] <- 1 - Callocr - Branchallocn[i] - Leafallocn[i]
        }
        if(mateoption == 1){  # ?????
            Stemallocn[i] <- (1 - Callocr - Leafallocn[i])*0.8
            Branchallocn[i] <- Stemallocn[i]*0.2/0.8
        }
        
        # Total allocation should be one, if not print warning:
        TotalAlloc[i] <- Callocr + Leafallocn[i] + Branchallocn[i] + Stemallocn[i]
        if(!identical(TotalAlloc[i],1)) warning("Problem : Total allocation is ", TotalAlloc[i], "\n")
        
        # Stem volume (m3 ha-1)
        Stemvol[i] <- Stem[i]/Cfracts/Density*1000
        
    } # END MAIN LOOP
    
    
    
    # Dataframe with all the results
    returndfr <- data.frame(Date=as.character(DATES),Year=DATES$year+1900,Month=DATES$mon+1,Day=DATES$mday,
                            DOY=DATES$yday+1,Fabs,APAR,fPAW,
                            fVPD, Gs, leafPsi, Aj, Ac, Asat, LUE,
                            LUE1000,LUEgCMJPAR,NPPtCha,NPPgCm2,CumNPP,CumAPAR,LAIgrass,Fabsgrass,
                            APARgrass,fPAWgrass,fVPDgrass,LUEgrass,NPPtChagrass,NPPgCm2grass,totalNPP,
                            CumNPPgrass,CumtotalNPP,PAWcur,PAW1cur,soilPsi,Transp,Erain,epsil,
                            Radlong,Radnet,Evapgrdvegorsoil,Drainage,	
                            CumPrecip,CumTransp,CumInterception,CumEvapsoil,CumDrainage,
                            Leaf,Sapwood,Stem,Branch,Root,Lai,Height,LeafSapAreaActual,LeafSapAreaTarget, 
                            TargetBranch,Leafallocn,Branchallocn,Stemallocn,TotalAlloc,Leafturnoverrate,Stemvol)
    returndfr <- cbind(returndfr, DD)
    
    write.table(returndfr, outputfile, sep=",", row.names=F)
    
    return(returndfr)
    
    options(warn=0)
}