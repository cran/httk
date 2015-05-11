# This function predicts partition coefficients for all tissues, then lumps them into a single compartment. The effective volume of distribution is calculated by summing each tissues volume times it's partition coefficient relative to plasma. Plasma, and the paritioning into RBCs are also added to get the total volume of distribution in L/KG BW.
calc_vdist<- function(chem.cas=NULL,
                              chem.name=NULL,
                              parameters=NULL,
                              default.to.human=F,
                              species="Human",suppress.messages=F)
{
  PK.physiology.data <- PK.physiology.data

  if(is.null(parameters)){
    parameters <- parameterize_schmitt(chem.cas=chem.cas,chem.name=chem.name,default.to.human=default.to.human,species=species)
    schmitt.params <- T
  }else schmitt.params <- F
  
  
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
  plasma.vol <- this.phys.data["Plasma Volume"]/1000 # L/kg BW
  if(schmitt.params){  
   # Predict the PCs for all tissues in the tissue.data table:
    PCs <- predict_partitioning_schmitt(parameters=parameters)
   # Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
    lumped_params <- get_lumped_tissues(PCs,tissuelist=NULL,species=species)

    RBC.vol <- plasma.vol/(1 - hematocrit)*hematocrit
    vol.dist <- plasma.vol + RBC.vol*lumped_params$Ktissue2plasma[["red blood cells"]]*parameters$Funbound.plasma+lumped_params$Ktissue2plasma[["rest"]]*lumped_params$vol[["rest"]]*parameters$Funbound.plasma   
  }else{
    necess <- c("Funbound.plasma","hematocrit","Vrestc","Krest2plasma","Krbc2plasma")
    if(!all(necess %in% names(parameters))){
      if(is.null(chem.cas) & is.null(chem.name))stop('chem.cas or chem.name must be specified when not including Funbound.plasma, hematocrit, Vrestc, Krest2plasma, and Krbc2plasma in parameters.')
      params <- parameterize_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human)
      parameters <- c(parameters,params[!(names(params) %in% names(parameters))])
      if(!suppress.messages)warning('Unspecified pbtk model parameters included in the calculation.  Include all necessary parameters (Funbound.plasma, hematocrit, Vrestc, Krest2plasma, and Krbc2plasma) to use a different set of parameters in the calculation.')
    }
    RBC.vol <- plasma.vol/(1 - parameters$hematocrit)*parameters$hematocrit 
    vol.dist <- plasma.vol + RBC.vol*parameters[["Krbc2plasma"]]*parameters$Funbound.plasma
    lastchar <- function(x){substr(x, nchar(x), nchar(x))}
    firstchar <- function(x){substr(x, 1,1)}
    scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V"&lastchar(names(parameters))=="c"]
    PCs <- names(parameters)[firstchar(names(parameters))=="K"]
    comps <- intersect(substr(scaled.volumes,2,nchar(scaled.volumes)-1),substr(PCs,2,nchar(PCs)-7)) 
    comps <-  comps[!(comps %in% c('art','ven'))]        
    for(this.comp in comps){
      eval(parse(text=paste('vol.dist <- vol.dist + ', parameters[[scaled.volumes[grep(this.comp,scaled.volumes)]]],'*', parameters[[PCs[grep(this.comp,PCs)]]],'*',parameters$Funbound.plasma))) # L 
    }
  }
    
  if(!suppress.messages)cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"volume of distribution returned in units of L/kg BW.\n")
    
  return(as.numeric(vol.dist))
}