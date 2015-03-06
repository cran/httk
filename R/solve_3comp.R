solve_3comp <- function(chem.name = NULL,
                    chem.cas = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    daily.dose =1, # Assume dose is in mg/kg BW/day
                    doses.per.day=0,
                    initial.values=NULL,
                    plots=F,
                    suppress.messages=F,
                    species="Human",
                    iv.dose=F,
                    output.units='uM',
                    fu.hep.correct=T,
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    recalc.blood2plasma=F,
                    ...)
{
  Aintestine <- Aportven <- Aliver <- Asyscomp <- Cportven <- Cliver <- Csyscomp <- NULL
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Parameters, chem.name, or chem.cas must be specified.')
  if(iv.dose) doses.per.day <- 0

  if(doses.per.day != 0){
    dose <- daily.dose / doses.per.day
  }else{
    dose <- daily.dose
  }
   
  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}

  if (is.null(parameters)){
        parameters <- parameterize_3comp(chem.cas=chem.cas,chem.name=chem.name,species=species,fu.hep.correct=fu.hep.correct)
      }else{
        name.list <- c("BW","CLmetabolismc","Fraction_unbound_plasma","hematocrit","Krbc2plasma","Kgut2plasma","kgutabs","Kliver2plasma","Krest2plasma","MW","Qcardiacc","Qgfrc","Qgutf","Qliverf","Ratioblood2plasma","Vgutc","Vliverc","Vrestc")
        if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_3comp."))
        name.list2 <- c("BW","CLmetabolismc","Fraction_unbound_plasma","hematocrit","kdermabs","Krbc2plasma","Kgut2plasma","kgutabs","kinhabs","Kkidney2plasma","Kliver2plasma","Klung2plasma","Krest2plasma","MW","Qcardiacc" ,"Qgfrc","Qgutf","Qkidneyf","Qliverf","Qlungf","Ratioblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vrestc","Vvenc")
        if(any(name.list2[which(!name.list2 %in% name.list)] %in% names(parameters)))stop("Parameters are from parameterize_pbtk.  Use parameters from parameterize_3comp.")
      }
      
   if(tolower(output.units)=='um' |  tolower(output.units) == 'mg/l') use.amounts <- F
   if(tolower(output.units)=='umol' |  tolower(output.units) == 'mg') use.amounts <- T
   
  if(tolower(output.units)=='um' | tolower(output.units) == 'umol'){
       moldose = as.numeric(dose * parameters[["BW"]] / 1000 / parameters[["MW"]] * 1000000)
     }else if(tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg'){
       moldose <- dose * parameters[['BW']]
     }else stop('Output.units can only be uM, umol, mg, or mg/L.')
 
  
  parameters[['Vportven']] <- parameters[['Vgutc']] * parameters[['BW']]
  parameters[['Vliver']] <- parameters[['Vliverc']] * parameters[['BW']]
  parameters[['Vsyscomp']] <- parameters[['Vrestc']] * parameters[['BW']]
  parameters[['Qkidneyf']] <- parameters[["CLbiliary"]] <-  parameters[["kdermabs"]] <-parameters[["kinhabs"]] <- parameters[["MW"]]   <- parameters[['Vgutc']] <- parameters[['Vrestc']]  <- parameters[["Vliverc"]]  <- NULL
 parameters[["Qcardiac"]] <- parameters[["Qgut"]] <- parameters[["Qliver"]] <- parameters[["CLmetabolism"]]  <- parameters[["Qgfr"]] <- 0
    
  
  if (use.amounts)
  {
    CompartmentsToInitialize <-c("Aintestine","Aportven","Aliver","Asyscomp")
  } else {
    CompartmentsToInitialize <-c("Aintestine","Cportven","Cliver","Csyscomp")
  }

  for (this.compartment in CompartmentsToInitialize)
  {
  # If the compartment has a value specified in the list initial.values, then set it to that value:
    if (this.compartment %in% names(initial.values))
    {
      eval(parse(text=paste(this.compartment,"<-",initial.values[[this.compartment]])))
    }
  # Otherwise set the value to zero:
    else eval(parse(text=paste(this.compartment,"<- 0")))
  }

   if (use.amounts) 
   {
     if(iv.dose){
       state <- c(Aintestine = Aintestine,Aportven = Aportven,Aliver = Aliver, Asyscomp = Asyscomp + moldose,Ametabolized = 0, Atubules = 0,AUC=0)
     }else{    
       state <- c(Aintestine = Aintestine + moldose,Aportven = Aportven,Aliver = Aliver, Asyscomp = Asyscomp,Ametabolized = 0, Atubules = 0,AUC=0)
     }
   }else{
     if(iv.dose){
       state <- c(Aintestine = Aintestine,Aportven = Cportven * parameters[['Vportven']],Aliver = Cliver * parameters[['Vliver']], Asyscomp = Csyscomp * parameters[['Vsyscomp']]  + moldose,Ametabolized = 0, Atubules = 0,AUC=0)
     }else{   
       state <- c(Aintestine = Aintestine + moldose,Aportven = Cportven * parameters[['Vportven']],Aliver = Cliver * parameters[['Vliver']], Asyscomp = Csyscomp * parameters[['Vsyscomp']],Ametabolized = 0, Atubules = 0,AUC=0)
     }
   }    
  
  if(recalc.blood2plasma) parameters[['Ratioblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2plasma']] * parameters[['Fraction_unbound_plasma']]

  
  parameters <- initparms3comp(parameters[!(names(parameters) %in% c("Fraction_unbound_hepatocyteassay","Krbc2plasma","hematocrit"))])


  
  state <-initState3comp(parameters,state)
  
  if (is.null(times)) times <- round(seq(0, days, 1/(24*tsteps)),8)
  start <- times[1]
  end <- times[length(times)]
  if(doses.per.day==0)
  {
    out <- ode(y = state, times = times,func="derivs3comp", parms=parameters, method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod3comp", nout=length(Outputs3comp),outnames=Outputs3comp,...)
  } else {
    dosing <- seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day)
    length <- length(dosing)
    eventdata <- data.frame(var=rep('Aintestine',length),time = round(dosing,8),value = rep(moldose,length), method = rep("add",length))                          
    out <- ode(y = state, times = times, func="derivs3comp", parms = parameters, method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod3comp", nout=length(Outputs3comp),outnames=Outputs3comp,events=list(data=eventdata),...)
  }


  colnames(out)[[which(colnames(out)=='Cserum')]] <- 'Cplasma'
  

  
  if(plots==T)
  {
    plot(out,select=c(CompartmentsToInitialize,"Cplasma","AUC","Ametabolized","Atubules"))
  }
  
  out <- out[,c("time",CompartmentsToInitialize,"Cplasma","AUC","Ametabolized","Atubules")]
if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      cat("Values returned in",output.units,"units.\n")
    }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),"values returned in",output.units,"units.\n")
    if(tolower(output.units) == 'mg'){
      cat("AUC is area under plasma concentration in mg/L * days units with Ratioblood2plasma =",parameters[['Ratioblood2plasma']],".\n")
    }else if(tolower(output.units) == 'umol'){
      cat("AUC is area under plasma concentration in uM * days units with Ratioblood2plasma =",parameters[['Ratioblood2plasma']],".\n")
    }else cat("AUC is area under plasma concentration curve in",output.units,"* days units with Ratioblood2plasma =",parameters[['Ratioblood2plasma']],".\n")
  }
  return(out) #concentration returned in umoles/L
}


