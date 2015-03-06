# from Ito and Houston (2004)
calc_hepatic_clearance <- function(chem.name=NULL,chem.cas=NULL,parameters=NULL,species='Human',hepatic.model='well-stirred',fu.hep.correct=T,suppress.messages=F)
{
  model <- hepatic.model
  if(is.null(parameters))parameters <- parameterize_steadystate(chem.cas=chem.cas,chem.name=chem.name,species=species,fu.hep.correct=fu.hep.correct)

  CLint <- get_param("CLint",parameters,"calc_Hepatic_Clearance") # uL/min/10^6 cells
  if(fu.hep.correct)
    fu_hep <- get_param("Fraction_unbound_hepatocyteassay",parameters,"calc_Hepatic_Clearance") 
  else fu_hep <- 1    #try(get_param("Fraction_unbound_hepatocyteassay",parameters,"calc_Hepatic_Clearance")) # fraction set if paramaterize function called with fu_hep_correct=TRUE
  #if (class(fu_hep) == "try-error") fu_hep <- 1
# Correct for fraction of chemical unbound in in vitro hepatocyte assay:
  CLint <- CLint / fu_hep

  fub <- get_param("Fraction_unbound_plasma",parameters,"calc_Hepatic_Clearance") # unitless fraction
  Qhc <- get_param("Qhc",parameters,"calc_Hepatic_Clearance",default=1.24) # L/h/kgBW
  liver.volume.per.kgBW <- get_param("liver.volume.per.kgBW",parameters,"calc_Hepatic_Clearance") #  L/kg BW
  tissue.density <- get_param("tissue.density",parameters,"calc_Hepatic_Clearance") # g/mL
  Dn <- get_param("Dn",parameters,"calc_Hepatic_Clearance",default=0.17) #
  #model <- get_param("model",parameters,"calc_Hepatic_Clearance",default="well-stirred")
  million.cells.per.gliver <- get_param("million.cells.per.gliver",parameters,"calc_Hepatic_Clearance") # 10^6 cells/g-liver
  
  if (!(tolower(model) %in% c("well-stirred","parallel tube","dispersion","unscaled")))
    stop("Model other than \"well-stirred,\" \"parallel tube,\", \"dispersion\", or \"unscaled\" specified.")

  # Convert from uL/min/10^6 cells to uL/min/g-liver to uL/min/kg BW
  CLint <- CLint*million.cells.per.gliver
  # Convert from uL/min/g-liver to uL/min/kg BW
  CLint <- CLint*(liver.volume.per.kgBW*1000*tissue.density)
  # Convert from uL/min/kg BW to L/h/kg BW
  CLint <- CLint/10^6*60

  Qhc <- Qhc / parameters[['BW']]^0.25
  if (tolower(model) == "unscaled")
    CLh <- CLint
  else if (tolower(model) == "well-stirred")
    CLh <- Qhc*fub*CLint/(Qhc+fub*CLint)  
  else if (tolower(model) == "parallel tube")
    CLh <- Qhc*(1-exp(-fub*CLint/Qhc))
  else if (tolower(model) == "dispersion")
  {
    Rn <- fub*CLint/Qhc
    a <- sqrt(1 + 4*Rn*Dn)
    CLh <- Qhc*(1 - 4*a/((1+a)^2*exp((a-1)/2/Dn)-(1-a)^2*exp(-(a+1)/2/Dn)))
  }
  
  if(!suppress.messages) cat("Hepatic clearance calculated with the",hepatic.model,"model in units of L/h/kg.\n")
  
  return(as.numeric(CLh))
}

