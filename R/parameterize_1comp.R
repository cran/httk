parameterize_1comp <- function(chem.cas=NULL,chem.name=NULL,species='Human',default.to.human=F)
{
 PK.physiology.data <- PK.physiology.data
if(is.null(chem.cas) & is.null(chem.name)) stop('Must specify chem.name or chem.cas')
params <- list()

params[['Vdist']] <- calc_vdist(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,suppress.messages=T)

params[['kelim']] <- calc_elimination_rate(chem.cas=chem.cas,chem.name=chem.name,species=species,suppress.messages=T,default.to.human=default.to.human)

params[['kgutabs']] <- 1

params[['Rblood2plasma']] <- calc_rblood2plasma(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human)

params[['million.cells.per.gliver']] <- 110

 
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
  
  params[['hematocrit']] <- this.phys.data[["Hematocrit"]]

if(is.null(chem.cas)) chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
params[['MW']] <- get_physchem_param("MW",chem.CAS=chem.cas)

  Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.CAS=chem.cas),silent=T)
  if (class(Fgutabs) == "try-error") Fgutabs <- 1
  
  params[['Fgutabs']] <- Fgutabs



return(params)

}