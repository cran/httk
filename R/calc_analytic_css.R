 calc_analytic_css <- function(chem.name=NULL,chem.cas = NULL,parameters=NULL,daily.dose=1,output.units='uM',model = 'pbtk',species='Human',concentration='plasma',suppress.messages=F,fu.hep.correct=T,recalc.blood2plasma=F)
 {
    if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Must specify chem.cas, chem.name, or parameters.')
    
    dose <- daily.dose
    good.units <- c("uM","mg/L")
    if (!(tolower(output.units) %in% tolower(good.units))) stop(paste("Do not know how to calculate units",output.units,". Please select from: ",paste(good.units,collapse=", ")))
    dose <- dose / 24 
    if(is.null(parameters)){
      if(is.null(chem.cas)){
        out <- get_chem_id(chem.name=chem.name)
        chem.cas <- out$chem.cas
      }
      MW <- get_physchem_param('MW',chem.CAS=chem.cas)
    }else{
      MW <- parameters[['MW']]
    } 
    if(tolower(output.units)=='um'){ 
         dose <- dose / 1000 / MW * 1000000
    }else if(tolower(output.units) != 'mg/l') stop('Output.units can only be uM or mg/L.')
     
    if(tolower(model)=='pbtk')
    {
       if(is.null(parameters)){
         parameters <- parameterize_pbtk(chem.cas=chem.cas,species=species,fu.hep.correct=fu.hep.correct) 
       }else{
         name.list <- c("BW","CLmetabolismc","Fraction_unbound_plasma","hematocrit","kdermabs","Kgut2plasma","kgutabs","kinhabs","Kkidney2plasma","Kliver2plasma","Klung2plasma","Krbc2plasma","Krest2plasma","MW","Qcardiacc" ,"Qgfrc","Qgutf","Qkidneyf","Qliverf","Qlungf","Ratioblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vrestc","Vvenc")
         if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_pbtk."))
       }  
       if(recalc.blood2plasma) parameters[['Ratioblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2plasma']] * parameters[['Fraction_unbound_plasma']]  


       
       Qcardiac <-  parameters[["Qcardiacc"]] / parameters[['BW']]^0.25  
       Qgfr <-  parameters[["Qgfrc"]] / parameters[['BW']]^0.25    
       CLmetabolism <-  parameters[["CLmetabolismc"]]  
       Kliver2plasma <- parameters[['Kliver2plasma']]

       Qgut <- parameters[["Qgutf"]] * Qcardiac
       Qliver <- parameters[["Qliverf"]] * Qcardiac
       Qkidney <- parameters[['Qkidneyf']] * Qcardiac
       Qrest <- Qcardiac-Qgut-Qliver-Qkidney
       Ratioblood2plasma <- parameters[['Ratioblood2plasma']]
       fub <- parameters[["Fraction_unbound_plasma"]]
       CLint <-  parameters[['CLmetabolismc']]

  
       Css <- (dose * (Qliver + Qgut) / (fub * CLmetabolism / Ratioblood2plasma + (Qliver + Qgut))) / (Qcardiac - (Qliver + Qgut)**2 /(fub * CLmetabolism / Ratioblood2plasma + (Qliver + Qgut)) - Qkidney**2 / (Qgfr * fub / Ratioblood2plasma + Qkidney) - Qrest)
     if (tolower(concentration)=='plasma')
      {
        Css <- Css / parameters[['Ratioblood2plasma']]
      } else if (tolower(concentration)!='blood') stop("Only blood and plasma concentrations are calculated.")
    }
    else if (tolower(model)=='3compartmentss')
    {
      if (is.null(parameters)){
        parameters <- parameterize_steadystate(chem.cas=chem.cas,species=species,fu.hep.correct=fu.hep.correct)
      }else{
        name.list <- c("CLint","Fraction_unbound_plasma","Qhc","QGFRc","BW","MW","million.cells.per.gliver","liver.volume.per.kgBW","tissue.density")
        if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_steadystate."))
      }
      if(parameters$Fraction_unbound_plasma == 0) stop('Fraction unbound plasma cannot be zero.  Use calc_mc_css or get_wetmore_css to predict steady state for this chemical with three compartment steady state model.')
      
      
      Css <- dose/(parameters$QGFRc/parameters[['BW']]^.25 * parameters$Fraction_unbound_plasma + calc_hepatic_clearance(parameters=parameters,fu.hep.correct=fu.hep.correct,suppress.messages=T))
      if (tolower(concentration)=='blood')
      {
        Rb2p <- calc_ratioblood2plasma(chem.name=chem.name,chem.cas=chem.cas)
        Css <- Css * Rb2p
      } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")      
    }else if(tolower(model) == '1compartment'){
      if(is.null(parameters)){
        parameters <- parameterize_1comp(chem.cas=chem.cas,species=species)
      }else{
        name.list <- c("volume.of.distribution","ke","kgutabs","Ratioblood2plasma","MW")
        if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_1comp."))
      }
      
      Css <- dose / parameters$ke / parameters$volume.of.distribution
      if (tolower(concentration)=='blood')
      {
        Css <- Css * parameters[['Ratioblood2plasma']]
      } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")
    }else if(tolower(model) == '3compartment'){
      if (is.null(parameters)){
        parameters <- parameterize_3comp(chem.cas=chem.cas,species=species)
      }else{
        name.list <- c("BW","CLmetabolismc","Fraction_unbound_plasma","hematocrit","Kgut2plasma","Krbc2plasma","kgutabs","Kliver2plasma","Krest2plasma","MW","Qcardiacc","Qgfrc","Qgutf","Qliverf","Ratioblood2plasma","Vgutc","Vliverc","Vrestc")
        if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_3comp."))
        name.list2 <- c("BW","CLmetabolismc","Fraction_unbound_plasma","hematocrit","kdermabs","Kgut2plasma","kgutabs","kinhabs","Kkidney2plasma","Kliver2plasma","Klung2plasma","Krbc2plasma","Krest2plasma","MW","Qcardiacc" ,"Qgfrc","Qgutf","Qkidneyf","Qliverf","Qlungf","Ratioblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vrestc","Vvenc")
        if(any(name.list2[which(!name.list2 %in% name.list)] %in% names(parameters)))stop("Parameters are from parameterize_pbtk.  Use parameters from parameterize_3comp.")
      }
      if(recalc.blood2plasma) parameters[['Ratioblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2plasma']] * parameters[['Fraction_unbound_plasma']]
      

      
      Css <- dose * parameters[['BW']]^0.25  / (parameters$CLmetabolismc * parameters[['BW']]^0.25  + parameters$Qgfrc * (parameters$Qliverf + parameters$Qgutf) / ((parameters$Qliverf + parameters$Qgutf) + parameters$Fraction_unbound_plasma * parameters$Qgfrc / parameters$Ratioblood2plasma)) / parameters$Fraction_unbound_plasma
      if (tolower(concentration)=='blood')
      {
         Css <- Css * parameters[['Ratioblood2plasma']]
      } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")
    }else stop("Model must be either \"pbtk\", \"1compartment\", \"3compartmentss\", or \"3compartment\".")
    
  if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      cat(paste(toupper(substr(concentration,1,1)),substr(concentration,2,nchar(concentration)),sep=''),"concentrations returned in",output.units,"units.\n")
    }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),concentration,"concentrations returned in",output.units,"units.\n")
  }    
       

 #  Css <- dose * Ratioblood2plasma * (Qliver + Qgut) * (Qkidney * Ratioblood2plasma  + Qgfr * fub)  / ((##(Ratioblood2plasma * Qkidney + Qgfr * fub) * (Qcardiac - Qrest) - Qkidney**2 * Ratioblood2plasma) * ((Qliver + Qgut) * Ratioblood2plasma +  CLmetabolism * fub * Kliver2plasma) - (Qliver + Qgut)**2 * (Qkidney *Ratioblood2plasma + Qgfr * fub)*Ratioblood2plasma)
  
    #Css <- dose/(QGFRc*fub+calc_Hepatic_Clearance(Params))
    return(as.numeric(Css))
}