solve_pbtk <- function(chem.name = NULL,
                    chem.cas = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    daily.dose = 1, # Assume dose is in mg/kg BW/day
                    doses.per.day=0,
                    initial.values=NULL,
                    plots=F,
                    suppress.messages=F,
                    species="Human",
                    iv.dose=F,
                    output.units='uM',
                    fu.hep.correct=T,
                    method="lsoda",rtol=1e-8,atol=1e-12,
                    default.to.human=F,
                    recalc.blood2plasma=F,
                    ...)
{
  Aart <- Agut <- Agutlumen <- Alung <- Aliver <- Aven <- Arest <- Akidney <- Cgut <- Vgut <- Cliver <- Vliver <- Cven <- Vven <- Clung <- Vlung <- Cart <- Vart <- Crest <- Vrest <- Ckidney <- Vkidney <- NULL
  if(is.null(chem.cas) & is.null(chem.name) & is.null(parameters)) stop('Parameters, chem.name, or chem.cas must be specified.')
  if(iv.dose) doses.per.day <- 0
  
  if(doses.per.day != 0){
    dose <- daily.dose / doses.per.day
  }else{
    dose <- daily.dose
  }

  lastchar <- function(x){substr(x, nchar(x), nchar(x))}
  firstchar <- function(x){substr(x, 1,1)}

  if(is.null(parameters)){
    parameters <- parameterize_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species,fu.hep.correct=fu.hep.correct,default.to.human=default.to.human) 
  }else{
    name.list <- c("BW","CLmetabolismc","Fraction_unbound_plasma","hematocrit","kdermabs","Kgut2plasma","kgutabs","kinhabs","Kkidney2plasma","Kliver2plasma","Klung2plasma","Krbc2plasma","Krest2plasma","MW","Qcardiacc" ,"Qgfrc","Qgutf","Qkidneyf","Qliverf","Qlungf","Ratioblood2plasma","Vartc","Vgutc","Vkidneyc","Vliverc","Vlungc","Vrestc","Vvenc")
  if(!all(name.list %in% names(parameters)))stop(paste("Missing parameters:",paste(name.list[which(!name.list %in% names(parameters))],collapse=', '),".  Use parameters from parameterize_pbtk."))
  }
  #Ratioblood2plasma <- parameters[["Ratioblood2plasma"]]
  #hematocrit <- parameters[["hematocrit"]]
   #dose converted to umoles/day  from  mg/kg BW/day 
   
     if(tolower(output.units)=='um' |  tolower(output.units) == 'mg/l') use.amounts <- F
     if(tolower(output.units)=='umol' |  tolower(output.units) == 'mg') use.amounts <- T
  
     if(tolower(output.units)=='um' | tolower(output.units) == 'umol'){
       moldose = as.numeric(dose * parameters[["BW"]] / 1000 / parameters[["MW"]] * 1000000)
     }else if(tolower(output.units) == 'mg/l' | tolower(output.units) == 'mg'){
       moldose <- dose * parameters[['BW']]
     }else stop('Output.units can only be uM, umol, mg, or mg/L.')
   
  
  parameters[["CLbiliary"]] <- parameters[["kdermabs"]]  <-parameters[["kinhabs"]] <- parameters[["MW"]] <- parameters[["Qlungf"]]  <- NULL

  
  scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V"&lastchar(names(parameters))=="c"]
        
  for (this.vol in scaled.volumes)
  {
      eval(parse(text=paste(substr(this.vol,1,nchar(this.vol)-1), '<-', parameters[[this.vol]],'*', parameters[["BW"]]))) # L 
  }
  
  
  if (use.amounts)
  {
    CompartmentsToInitialize <-c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest")
  } else {
    CompartmentsToInitialize <-c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")
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
   # if('Aven' %in% names(initial.values)) Aserum <- Aven / Ratioblood2plasma * (1 - hematocrit)
    #if('Aserum' %in% names(initial.values)) Aven <- Aserum * Ratioblood2plasma / (1 - hematocrit) 
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen,Alung = Alung,Aliver = Aliver,
               Aven = Aven + moldose,Arest = Arest,Akidney = Akidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }else{
      state <- c(Aart = Aart,Agut = Agut,Agutlumen = Agutlumen + moldose,Alung = Alung,Aliver = Aliver,
               Aven = Aven,Arest = Arest,Akidney = Akidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }
  }else{
    if(iv.dose){
      state <- c(Agutlumen = Agutlumen,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven + moldose,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }else{
    #if('Cven' %in% names(initial.values)) Cserum <- Cven / Ratioblood2plasma
    #if('Cserum' %in% names(initial.values)) Cven <- Cserum * Ratioblood2plasma
      state <- c(Agutlumen = Agutlumen + moldose,Agut = Cgut * Vgut,Aliver = Cliver * Vliver,Aven = Cven * Vven,Alung = Clung * Vlung,Aart = Cart * Vart,Arest = Crest * Vrest,Akidney = Ckidney * Vkidney,Atubules = 0,Ametabolized = 0,AUC=0)
    }
  }    
  
  if(recalc.blood2plasma) parameters[['Ratioblood2plasma']] <- 1 - parameters[['hematocrit']] + parameters[['hematocrit']] * parameters[['Krbc2plasma']] * parameters[['Fraction_unbound_plasma']]
  

  
  parameters <- initparms(parameters[!(names(parameters) %in% c("Fraction_unbound_hepatocyteassay","Krbc2plasma"))])

  
  state <-initState(parameters,state)
  
  if (is.null(times)) times <- round(seq(0, days, 1/(24*tsteps)),8)
  start <- times[1]
  end <- times[length(times)] 
     
  if(doses.per.day==0)                                
  {
    out <- ode(y = state, times = times,func="derivs", parms=parameters, method=method,rtol=rtol,atol=atol,dllname="httk",initfunc="initmod", nout=length(Outputs),outnames=Outputs,...)
  } else {
    length <- length(seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day))
    eventdata <- data.frame(var=rep('Agutlumen',length),time = round(seq(start + 1/doses.per.day,end-1/doses.per.day,1/doses.per.day),8),value = rep(moldose,length), method = rep("add",length))                          
    out <- ode(y = state, times = times, func="derivs", parms = parameters,method=method,rtol=rtol,atol=atol, dllname="httk",initfunc="initmod", nout=length(Outputs),outnames=Outputs,events=list(data=eventdata),...)
  }
  
 
  colnames(out)[[which(colnames(out)=='Aserum')]] <- 'Aplasma'
  colnames(out)[[which(colnames(out)=='Cserum')]] <- 'Cplasma'
  
  
  if(plots==T)
  {
    if(use.amounts){
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC"))
    }else{
      plot(out,select=c(CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC"))
    }
    
  }
    if(use.amounts){
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Aplasma","AUC")]
    }else{
      out <- out[,c("time",CompartmentsToInitialize,"Ametabolized","Atubules","Cplasma","AUC")]
    }
    
  
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
    
  return(out) 
}