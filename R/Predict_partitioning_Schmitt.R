# This predicts the coefficient of tissue to FREE plasma fraction via Schmitt's method (2008):  
# fupl: unbound fraction in plasma
# Pow: octonol:water partition coefficient (not log transformed)
# pKa_Donor: compound H dissociation equilibirum constant(s)
# pKa_Accept: compound H association equilibrium constant(s)   
# Dplw: phospholipid:water distribution coefficient
# KAPPAcell2plasma: Ratio of D inside the cell to D in the plasma, as derived from the different pHs and pKas
# FPpl: protein fraction in plasma - from Gardner 1980
predict_partitioning_schmitt <- function(fupl,
                            Pow,
                            pKa_Donor=NA,
                            pKa_Accept=NA,
                            Dplw=NULL,
                            alpha=0.001,
                            FPpl = 75/1000/1.025,
                            plasma.pH = 7.4,
                            temperature=NULL) #Schmitt 2008
{
# For the "rest" tissue containing those tissues in "Physiological Parameter
# Values for PBPK Models" (2004) that are not described by Schmitt (2008)
# we use the average values for the Schmitt (2008) tissues
  tissue.data[tissue.data$Tissue=="rest",2:10]<-apply(tissue.data[1:11,2:10],2,mean)
# Then normalize to one:
  tissue.data[c(1:11,13),2:3] <- tissue.data[c(1:11,13),2:3]/apply(tissue.data[c(1:11,13),2:3],1,sum)

	Ktissue2plasma <- list()
	
	# water fraction in plasma:
	FWpl <- 1 - FPpl
	# protein fraction in interstitium:
  FPint <- 0.37 * FPpl
	# water fraction in interstitium:
  FWint <- FWpl
	
	for (this.tissue in tissue.data$Tissue)
	{
		this.row <- tissue.data$Tissue==this.tissue
		
# Tissue-specific cellular/interstial volume fractions:
    # Cellular fraction of total volume:
		Fcell <- as.numeric(tissue.data[this.row,"Fcell"])
		# interstitial fraction of total volume:
		Fint <- as.numeric(tissue.data[this.row,"Fint"])
		if (is.na(Fint)) Fint <- 0
		
# Tissue-specific cellular sub-fractions:
		# water volume fraction:
		FW <- Fcell * as.numeric(tissue.data[this.row,"FWc"])
		# protein volume fraction:
		FP <- Fcell * as.numeric(tissue.data[this.row,"FPc"])

# Tissue-specific cellular lipid sub-sub-fractions:        
		# neutral lipid volume fraction:
		Fn_L <- Fcell * as.numeric(tissue.data[this.row,"FLc"]) * as.numeric(tissue.data[this.row,"Fn_Lc"])
		if (is.na(Fn_L)) Fn_L <- 0
		# neutral phospholipid volume fraction:
		Fn_PL <- Fcell * as.numeric(tissue.data[this.row,"FLc"]) * as.numeric(tissue.data[this.row,"Fn_PLc"])
		if (is.na(Fn_PL)) Fn_PL <- 0
		# acidic phospholipid volume fraction:
		Fa_PL <- Fcell * as.numeric(tissue.data[this.row,"FLc"]) * as.numeric(tissue.data[this.row,"Fa_PLc"])
		if (is.na(Fa_PL)) Fa_PL <- 0
		
		# tissue pH
		pH <- as.numeric(tissue.data[this.row,"pH"])
	
 #   # plasma:protein partition coefficient
		KPpl = 1/FPpl*(1/fupl-FWpl)

		# neutral phospholipid:water parition coffficient:
		if (is.null(Dplw))
		{
      
			Kn_PL <- 10^(0.999831 - 0.016578*temperature + 0.881721*log10(Pow)) # Based on regression to measured MA's in  Schmitt (2008)
		}else if(is.na(Dplw)){
 	    Kn_PL <- 10^(0.999831 - 0.016578*temperature + 0.881721*log10(Pow)) # Based on regression to measured MA's in  Schmitt (2008)
    }else{
			Kn_PL <- Dplw
		}

    # Need to calculate the amount of un-ionized parent:
    ionization <- calc_ionization(pH,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
    fraction_neutral  <- ionization[["fraction_neutral"]]
    fraction_charged <- ionization[["fraction_charged"]]
    fraction_negative <- ionization[["fraction_negative"]]
    fraction_positive <- ionization[["fraction_positive"]]
  
		# Octonol:water distribution coefficient,
    Dow <- calc_dow(Pow,fraction_neutral=fraction_neutral,alpha=alpha)

		# neutral lipid:water partition coefficient
		Kn_L <- Dow

		# protein:water partition coefficient:
		KP <- 0.163 + 0.0221*Kn_PL
		
		# acidic phospholipid:water partition coefficient:
		Ka_PL <- Kn_PL * (fraction_neutral + 20*fraction_positive + 0.05*fraction_negative)

		# unbound fraction in interstitium:
		fuint <- 1/(FWint +   FPint/FPpl*(1/fupl - FWpl))
		
		# unbound fraction in cellular space:
		fucell <- 1/(FW + Kn_L*Fn_L+Kn_PL*Fn_PL+Ka_PL*Fa_PL+KP*FP)
		
    KAPPAcell2plasma <- calc_dow(Pow,pH=plasma.pH,alpha=alpha,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)/Dow
   
    # We want ratio of tissue concentration to unbound plasma Ctissue:(fub,plasma*Cplasma) = this.PC:
		#this.PC <- ((Fint/fuint + KAPPAcell2plasma*Fcell/fucell))*fupl
		Ktissue2plasma[[this.tissue]] <- as.numeric(((Fint/fuint + KAPPAcell2plasma*Fcell/fucell)))
	}
    
 	return(Ktissue2plasma)
}