solve_1comp <- function(chem.cas=NULL,
                        chem.name=NULL,
                        times=NULL,
                        parameters=NULL,
                        daily.dose = 1,
                        doses.per.day=0,
                        days=10,
                        tsteps=4,
                        suppress.messages=F,
                        species='Human',
                        output.units='uM',
                        plots=F,
                        initial.value=0,
                        iv.dose = F,
                        fu.hep.correct=T,
                        ...)
{
  
  if(iv.dose) doses.per.day <- 0
  if(doses.per.day != 0)
  {
    dose <- daily.dose / doses.per.day
  }else{
    dose <- daily.dose
  }
    
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Parameters, chem.name, or chem.cas must be specified.')
  if(is.null(parameters)){  parameters <- parameterize_1comp(chem.name=chem.name,chem.cas=chem.cas,species=species,fu.hep.correct=fu.hep.correct) 
  }else{
    name.list <- c("volume.of.distribution","ke","kgutabs","Ratioblood2plasma","MW")
    if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_1comp."))
  }
  
  volume.of.distribution <- parameters[['volume.of.distribution']]
  ke <- parameters[['ke']]
  ka <- parameters[['kgutabs']]
   
  if(tolower(output.units)=='um' |  tolower(output.units) == 'mg/l') use.amounts <- F
  if(tolower(output.units)=='umol' |  tolower(output.units) == 'mg') use.amounts <- T 
   
  if(tolower(output.units)=='um' | tolower(output.units) == 'umol')
  {
    dose <- as.numeric(dose / 1000 / parameters[["MW"]] * 1000000)
  } else if(!(tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg'))
  {
    stop('Output.units can only be uM, umol, mg, or mg/L.')

  }

  if(use.amounts)
  {
    if(iv.dose)
    {
      state <- c(Agutlumen=0,Compartment = (initial.value + dose) / volume.of.distribution,Ametabolized = 0,AUC=0)
    } else {
      state <- c(Agutlumen=dose,Compartment = initial.value / volume.of.distribution,Ametabolized = 0,AUC=0)
    }
  }else{
    if(iv.dose)
    {
      state <- c(Agutlumen=0,Compartment = initial.value + dose / volume.of.distribution,Ametabolized = 0,AUC=0)
    } else{
      state <- c(Agutlumen=dose,Compartment = initial.value,Ametabolized = 0,AUC=0)
    }
  }
  
  parameters <- c(ka=ka*24,volume=volume.of.distribution,ke=ke*24)

  eventfun<-function(t,y,parameters)
  {
    with(as.list(y),{
      Agutlumen <- Agutlumen + dose
      return(c(Agutlumen,Compartment,Ametabolized,AUC))
      })
  }
  
  DerivativeFunction <- function(t, state, parameters)
  {
    with(as.list(c(state, parameters)),{
      # rate of change
    dAgutlumen <- -ka * Agutlumen
    dCompartment <- ka / volume* Agutlumen - ke * Compartment
    dAmetabolized <- ke * Compartment * volume
    dAUC <- Compartment
  
  
    # return the rate of change
    list(c(dAgutlumen,dCompartment,dAmetabolized,dAUC))
    }) # end with(as.list ...
  }
  
  
  if (is.null(times)){
    times <- round(seq(0, days, 1/(24*tsteps)),8)
  }else{
    start <- times[1]
    end <- times[length(times)]
    times <- sort(round(c(times,seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day)),8))
  } 
  start <- times[1]
  end <- times[length(times)] 
  if (doses.per.day==0)
  {
    out <- ode(y = state, times = times, func = DerivativeFunction, parms = parameters,...)
  } else {

    out <- ode(y = state, times = times, func = DerivativeFunction, parms = parameters,
                 events=list(func=eventfun,time = round(seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day),8)),...)
  }

  if (use.amounts == T) out[,'Compartment'] <- out[,'Compartment'] * volume.of.distribution 
  if (plots==T) plot(out,select=c('Compartment','AUC'))
  
  out <- out[,c('time','Compartment','AUC')]
  if (!suppress.messages)
  {
    if(is.null(chem.cas) & is.null(chem.name))
    {
      cat("Values returned in",output.units,"units.\n")
    } else cat(species,"values returned in",output.units,"units.\n")
  }

  return(out)
}