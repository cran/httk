# This function calculates the constant ratio of the blood concentration to the plasma concentration. It uses the hematocrit and the red blood cell (RBC) parition coefficient as predicted by the Schmitt (2008) method.

calc_ratioblood2plasma <- function(chem.cas=NULL,
                              chem.name=NULL,
                              default.to.human=F,
                              species="Human")
{
  PK.physiology.data <- PK.physiology.data
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
    
  # unitless fraction of chemical unbound with plasma
  fub <- try(get_invitroPK_param("Fub",species,chem.CAS=chem.cas),silent=T)
  if (class(fub) == "try-error" & default.to.human) 
  {
    fub <- try(get_invitroPK_param("Fub","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for protein binding data."))
  }
  if (class(fub) == "try-error") stop(fub)
  if (fub == 0)
  {
    fub <- 0.005
    warning("Fraction unbound = 0, changed to 0.005.")
  }
  
# Check the species argument for capitilization problems and whether or not it is in the table:  
  if (!(species %in% colnames(PK.physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(PK.physiology.data)))
    {
      PK.phys.species <- colnames(PK.physiology.data)[toupper(colnames(PK.physiology.data))==toupper(species)]
      warning(paste(species,"coerced to",PK.phys.species,"for physiological data."))
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else PK.phys.species <- species

# Load the physiological parameters for this species
  this.phys.data <- PK.physiology.data[,PK.phys.species]
  names(this.phys.data) <- PK.physiology.data[,1]

  hematocrit <- this.phys.data["Hematocrit"]
  
  temp <- this.phys.data[['Average Body Temperature']] 
# Load the physico-chemical properties:  
  MW <- get_physchem_param("MW",chem.CAS=chem.cas) #g/mol
  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas))
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas))
  Pow <- 10^get_physchem_param("logP",chem.CAS=chem.cas)
  Dplw <- suppressWarnings(10^(get_physchem_param("logMA",chem.CAS=chem.cas)))
  
# Predict the PCs for all tissues in the tissue.data table:
  PCs <- predict_partitioning_schmitt(fub,Pow,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept,Dplw=Dplw,temperature=temp)
    
  Ratioblood2plasma = 1 - hematocrit + hematocrit * PCs[["red blood cells"]] * fub 
    
  return(as.numeric(Ratioblood2plasma))
}