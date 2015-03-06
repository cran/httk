# This function calculats an elimination rate for a one compartment model where 
# eelimination is entirely due to metablism by the liver and glomerular filtration
# in the kidneys.
calc_elimination_rate <- function(chem.cas=NULL,chem.name=NULL,parameters=NULL,species="Human",suppress.messages=F,fu.hep.correct=TRUE)
{
    
    if(is.null(parameters))parameters <- parameterize_steadystate(chem.cas=chem.cas,chem.name=chem.name,species=species,fu.hep.correct=fu.hep.correct)
      

    clearance <- calc_total_clearance(parameters=parameters,fu.hep.correct=fu.hep.correct,suppress.messages=T) #L/h/kgBW
    Vd <- calc_volume_of_distribution(chem.cas=chem.cas,chem.name=chem.name,species=species,suppress.messages=T) 
    if(!suppress.messages)cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"elimination rate returned in units of 1/h.\n")
    return(as.numeric(clearance/Vd))
}
