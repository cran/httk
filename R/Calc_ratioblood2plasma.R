# This function calculates the constant ratio of the blood concentration to the plasma concentration. It uses the hematocrit and the red blood cell (RBC) parition coefficient as predicted by the Schmitt (2008) method.

calc_rblood2plasma <- function(chem.cas=NULL,
                              chem.name=NULL,
                              default.to.human=F,
                              species="Human")
{
   PK.physiology.data <- PK.physiology.data
  parameters <- parameterize_schmitt(chem.cas=chem.cas,chem.name=chem.name,default.to.human=default.to.human,species=species)
  
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
  
# Predict the PCs for all tissues in the tissue.data table:
  PCs <- predict_partitioning_schmitt(parameters=parameters)
    
  Rblood2plasma = 1 - hematocrit + hematocrit * PCs[["red blood cells"]] * parameters$Funbound.plasma
    
  return(as.numeric(Rblood2plasma))
}