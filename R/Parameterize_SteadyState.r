# This function retrieves the paramters needed to run the constant infusion dose model for determining steady-state concentration.

parameterize_steadystate <- function(chem.cas=NULL,chem.name=NULL,species="Human",clint.pvalue.threshold=0.05,default.to.human=F,human.clint.fub=F,fu.hep.correct=T)

{
  PK.physiology.data <- PK.physiology.data
  tissue.data <- tissue.data
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name

  if (!(species %in% colnames(PK.physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(PK.physiology.data)))
    {
      PK.phys.species <- colnames(PK.physiology.data)[toupper(colnames(PK.physiology.data))==toupper(species)]
      warning(paste(species,"coerced to",PK.phys.species,"for physiological data."))
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else PK.phys.species <- species
  this.phys.data <- PK.physiology.data[,PK.phys.species]
  names(this.phys.data) <- PK.physiology.data[,1]
  
  QGFRc <- this.phys.data["GFR"] #mL/min/kgBW
  BW <- this.phys.data["Average BW"]
    
  if (!(paste(species,"Vol (L/kg)") %in% colnames(tissue.data)))
  {
    if (toupper(paste(species,"Vol (L/kg)")) %in% toupper(colnames(tissue.data)))
    {
      tissue.vols <- tissue.data[,toupper(colnames(tissue.data))==toupper(paste(species,"Vol (L/kg)"))]
      tissue.flows <- tissue.data[,toupper(colnames(tissue.data))==toupper(paste(species,"Flow (mL/min/kg^(3/4))"))]
      warning(paste(species,"coerced to",toupper(species),"for tissue data."))
    } else stop(paste("Tissue data for",species,"not found."))
  } else {
    tissue.vols <- tissue.data[,paste(species,"Vol (L/kg)")]
    tissue.flows <- tissue.data[,paste(species,"Flow (mL/min/kg^(3/4))")]
  }
  names(tissue.vols) <- tissue.data[,1]
  names(tissue.flows) <- tissue.data[,1]

  Qhc <- tissue.flows["liver"]  #mL/min/kgBW
  liver.volume.per.kgBW <- tissue.vols["liver"] # L/kg BW
  CLint <- try(get_invitroPK_param("Clint",species,chem.CAS=chem.cas),silent=T)
  if (class(CLint) == "try-error" & default.to.human || human.clint.fub) 
  {
    CLint <- try(get_invitroPK_param("Clint","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for metabolic clerance data."))
  }
  if (class(CLint) == "try-error") stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
    # Check that the trend in the CLint assay was significant:
  CLint.pValue <- get_invitroPK_param("Clint.pValue",species,chem.CAS=chem.cas)
  if (!is.na(CLint.pValue) & CLint.pValue > clint.pvalue.threshold) CLint <- 0
  
  # unitless fraction of chemical unbound with plasma
  fub <- try(get_invitroPK_param("Fub",species,chem.CAS=chem.cas),silent=T)
  if (class(fub) == "try-error" & default.to.human || human.clint.fub) 
  {
    fub <- try(get_invitroPK_param("Fub","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for protein binding data."))
  }
  if (class(fub) == "try-error") stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
  if (fub == 0)
  {
    fub <- 0.005
    warning("Fraction unbound = 0, changed to 0.005.")
  }
 

  Params <- list()
  Params[["CLint"]] <- CLint # uL/min/10^6
  Params[["Fraction_unbound_plasma"]] <- fub # unitless fraction
  Params[["Qhc"]] <- Qhc/1000*60     #        L/h/kgBW
  Params[["QGFRc"]] <- QGFRc/1000*60 #        L/h/kgBW     
  Params[["BW"]] <- BW # kg
  Params[["MW"]] <- get_physchem_param("MW",chem.CAS=chem.cas) # molecular weight g/mol
  
  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas)) # acid dissociation constants
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas)) # basic association cosntants
  Pow <- 10^get_physchem_param("logP",chem.CAS=chem.cas) # Octanol:water partition coeffiecient

# Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
  if (fu.hep.correct) Params[["Fraction_unbound_hepatocyteassay"]] <-calc_fu_hep(Pow,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept) # fraction 

  Params[["million.cells.per.gliver"]] <- 110 # 10^6 cells/g-liver
  Params[["liver.volume.per.kgBW"]] <- liver.volume.per.kgBW # L/kg BW
  Params[["tissue.density"]] <- 1.05 # g/mL
 # Params[["CLmetabolism"]] <- calc_Hepatic_Clearance(Params)
  return(Params)
}

