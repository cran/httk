parameterize_1comp <- function(chem.cas=NULL,chem.name=NULL,species='Human',default.to.human=F,fu.hep.correct=TRUE)
{
if(is.null(chem.cas) & is.null(chem.name)) stop('Must specify chem.name or chem.cas')
params <- list()

params[['volume.of.distribution']] <- calc_volume_of_distribution(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human,suppress.messages=T)

params[['ke']] <- calc_elimination_rate(chem.cas=chem.cas,chem.name=chem.name,species=species,suppress.messages=T,fu.hep.correct=fu.hep.correct)

params[['kgutabs']] <- 1

params[['Ratioblood2plasma']] <- calc_ratioblood2plasma(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human)

if(is.null(chem.cas)) chem.cas <- get_chem_id(chem.name=chem.name)[['chem.cas']]
params[['MW']] <- get_physchem_param("MW",chem.CAS=chem.cas)



return(params)

}