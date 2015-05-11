calc_css <- function(parameters=NULL,
                    chem.name=NULL,
                    chem.cas=NULL, 
                    species='Human',
                    p = .01,
                    daily.dose=1,
                    doses.per.day=3,
                    days = 10,
                    output.units = "uM",
                    concentration='plasma',
                    suppress.messages=F,
                    model='pbtk',
                    default.to.human=F,
                    ...)
{
  
  if(is.null(parameters)){
    if(tolower(model)=='pbtk'){
      parameters <- parameterize_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human)
    }else if(tolower(model)=='3compartment'){
      parameters <- parameterize_3comp(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human)
    }else if(tolower(model)=='1compartment'){
      parameters <- parameterize_1comp(chem.cas=chem.cas,chem.name=chem.name,species=species,default.to.human=default.to.human)
    }
  } 

  css <- calc_analytic_css(parameters=parameters,daily.dose=daily.dose,concentration='plasma',model=model,suppress.messages=T) 
  conc <- (1 - p) * css 

  if(tolower(model) == 'pbtk'){
    out <- solve_pbtk(parameters=parameters, daily.dose=daily.dose,doses.per.day=doses.per.day,days = days,suppress.messages=T,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")]
  }else if(tolower(model) =='3compartment'){
    out <- solve_3comp(parameters=parameters, daily.dose=daily.dose,doses.per.day=doses.per.day, days = days,suppress.messages=T,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cgut","Cliver","Crest")]
  }else if(tolower(model)=='1compartment'){
    out <- solve_1comp(parameters=parameters,daily.dose=daily.dose,doses.per.day=doses.per.day, days = days,suppress.messages=T,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Ccompartment")]
  }else stop('The model options are only: 1compartment, 3compartment, and pbtk.')
  
  day <- days
  
  while(out[dim(out)[1],'AUC'] - out[match(days - 1,out[,'time']),'AUC'] < conc)
  {
    if(day < 3600)
    {
      days <- days * 3
    }else{
      days <- days * 2
    }
    day <- day + days
    
     if(tolower(model) == 'pbtk'){
    out <- solve_pbtk(parameters=parameters,initial.values = Final_Conc, daily.dose=daily.dose,doses.per.day=doses.per.day, days = days,suppress.messages=T,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cart","Cven","Clung","Cgut","Cliver","Ckidney","Crest")]
  }else if(tolower(model) =='3compartment'){
    out <- solve_3comp(parameters=parameters,initial.values = Final_Conc, daily.dose=daily.dose,doses.per.day=doses.per.day, days = days,suppress.messages=T,...)
    Final_Conc <- out[dim(out)[1],c("Agutlumen","Cgut","Cliver","Crest")]
  }else if(tolower(model)=='1compartment'){
    out <- solve_1comp(parameters=parameters,daily.dose=daily.dose,doses.per.day=doses.per.day, days = days,suppress.messages=T,initial.values=Final_Conc,...)
    Final_Conc <- out[dim(out)[1],c('Agutlumen','Ccompartment')]
  }
  
    if(day > 36500) break 
  }
  
  if(out[dim(out)[1],'AUC'] - out[match(days - 1,out[,'time']),'AUC'] < conc)
  {
    if(!suppress.messages)cat("Steady state not reached after 100 years.")
    the.day <- 36500
  }else{
    dif <- out[match(1,out[,'time']):dim(out)[1],'AUC'] - out[1:match(days - 1,out[,'time']),'AUC']
    if(day == days & dif[1] > conc)
    {
      the.day <- 1
    }else{
      the.day <- day - days + 1 + ceiling(min(which(dif > conc)) / length(dif) * (days - 1))
    }
  }
  
  if (tolower(output.units) == tolower("mg/L")) 
  {
      out[,'AUC'] <- out[,'AUC']/1e+06 * parameters[["MW"]] * 1000
      css <- css /1e+06 * parameters[["MW"]] * 1000
      if(tolower(model)=='1compartment'){
        out[,'Ccompartment'] <- out[,'Ccompartment']/1e+06 * parameters[["MW"]] * 1000
      }else{  
        out[,'Cplasma'] <- out[,'Cplasma']/1e+06 * parameters[["MW"]] * 1000
      }
  } else if (tolower(output.units) != tolower("uM")) stop("Currently can only return units of mg/L and uM")
  
  if(tolower(concentration)=='plasma'){
    if(tolower(model)=='1compartment'){
      max=as.numeric(max(out[,'Ccompartment']))
    }else{
      max=as.numeric(max(out[,'Cplasma']))
    }
    avg=as.numeric(out[dim(out)[1],'AUC'] - out[match(days-1,out[,'time']),'AUC'])
  }else if(tolower(concentration)=='blood'){
    if(tolower(model)=='pbtk'){
      max=as.numeric(max(out[,'Cven']))
    }else if(tolower(model) == '3compartment'){
      max=as.numeric(max(out[,'Cplasma'] * parameters[['Rblood2plasma']]))
    }else{
      max=as.numeric(max(out[,'Ccompartment'] * parameters[['Rblood2plasma']]))
    }   
   avg=as.numeric((out[dim(out)[1],'AUC'] - out[match(days-1,out[,'time']),'AUC'])*parameters[['Rblood2plasma']])
  }else stop("Only blood and plasma concentrations are calculated.")
  if(!suppress.messages){
    if(is.null(chem.cas) & is.null(chem.name)){
      cat(paste(toupper(substr(concentration,1,1)),substr(concentration,2,nchar(concentration)),sep=''),"concentrations returned in",output.units,"units.\n")
    }else cat(paste(toupper(substr(species,1,1)),substr(species,2,nchar(species)),sep=''),concentration,"concentrations returned in",output.units,"units.\n")
  }
  return(list(frac=as.numeric((out[match(days - (day - the.day),out[,'time']),'AUC'] - out[match(days - 1 - (day - the.day),out[,'time']),'AUC']) / css), 
    max=max,
    avg=avg,
    the.day =as.numeric(the.day)))
}