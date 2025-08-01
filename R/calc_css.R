#'Find the steady state concentration and the day it is reached.
#'
#'@description This function finds the day a chemical comes within the specified
#'  range of the analytical steady state venous blood or plasma
#'  concentration(from calc_analytic_css) for the multiple compartment, three
#'  compartment, and one compartment models, the fraction of the true steady
#'  state value reached on that day, the maximum concentration, and the average
#'  concentration at the end of the simulation.
#'
#'@param chem.name Either the chemical name, CAS number, or parameters must be
#'  specified.
#'
#'@param chem.cas Either the chemical name, CAS number, or parameters must be
#'  specified.
#'
#'@param dtxsid EPA's DSSTox Structure ID
#'  (\url{https://comptox.epa.gov/dashboard}) the chemical must be identified by
#'  either CAS, name, or DTXSIDs
#'
#'@param parameters Chemical parameters from parameterize_pbtk function,
#'  overrides chem.name and chem.cas.
#'
#'@param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#'  default "Human").
#'
#'@param f Fractional distance from the final steady state concentration that
#'  the average concentration must come within to be considered at steady state.
#'
#'@param daily.dose Total daily dose, mg/kg BW.
#'
#'@param doses.per.day Number of oral doses per day.
#'
#'@param days Initial number of days to run simulation that is multiplied on
#'  each iteration.
#'
#'@param output.units Units for returned concentrations, defaults to uM (specify
#'  units = "uM") but can also be mg/L.
#'
#'@param suppress.messages Whether or not to suppress messages.
#'
#'@param tissue Desired tissue concentration (default value is NULL, will depend
#'  on model -- see \code{steady.state.compartment} in model.info file for
#'  further details.)
#'
#'@param model Model used in calculation, 'pbtk' for the multiple compartment
#'  model,'3compartment' for the three compartment model, and '1compartment' for
#'  the one compartment model.
#'
#'@param f.change Fractional change of daily steady state concentration reached
#'  to stop calculating.
#'
#'@param dosing The dosing object for more complicated scenarios. Defaults to
#'  repeated \code{daily.dose} spread out over \code{doses.per.day}
#'
#'@param dose.units The units associated with the dose received.
#'
#'@param route Route of exposure (either "oral", "iv", or "inhalation" default
#'  "oral").
#'
#'@param parameterize.args.list Named list of any additional arguments passed to
#'  model parameterization function (other than the already-named arguments).
#'  Default `list()` to pass no additional arguments.
#'
#'@param ... Additional arguments passed to \code{\link{solve_model}} (defaults
#' model is "pbtk").
#'
#'@return \item{frac}{Ratio of the mean concentration on the day steady state is
#'  reached (baed on doses.per.day) to the analytical Css (based on infusion
#'  dosing).} \item{max}{The maximum concentration of the simulation.}
#'  \item{avg}{The average concentration on the final day of the simulation.}
#' \item{the.day}{The day the average concentration comes within 100 * p
#' percent of the true steady state concentration.}
#'
#'@seealso \code{\link{calc_analytic_css}}
#'
#'@author Robert Pearce, John Wambaugh
#'
#'@keywords steady-state
#'
#' @examples
#'
#' calc_css(chem.name='Bisphenol-A',doses.per.day=5,f=.001,output.units='mg/L')
#'
#'\donttest{
#' parms <- parameterize_3comp(chem.name='Bisphenol-A')
#' parms$Funbound.plasma <- .07
#' calc_css(chem.name='Bisphenol-A',parameters=parms,model='3compartment')
#'
#' out <- solve_pbtk(chem.name = "Bisphenol A",
#'   days = 50,
#'   daily.dose=1,
#'   doses.per.day = 3)
#' plot.data <- as.data.frame(out)
#'
#' css <- calc_analytic_css(chem.name = "Bisphenol A")
#' library("ggplot2")
#' c.vs.t <- ggplot(plot.data,aes(time, Cplasma)) + geom_line() +
#' geom_hline(yintercept = css) + ylab("Plasma Concentration (uM)") +
#' xlab("Day") + theme(axis.text = element_text(size = 16), axis.title =
#' element_text(size = 16), plot.title = element_text(size = 17)) +
#' ggtitle("Bisphenol A")
#'
#' print(c.vs.t)
#'
#'
#' calc_css(chem.name='nicotine', model="1compartment")
#' 
#' calc_css(chem.name='nicotine', model="3compartment")
#' 
#' calc_css(chem.name="endrin")
#'}
#'
#'@importFrom purrr compact
#'@export calc_css
calc_css <- function(chem.name=NULL,
                    chem.cas=NULL, 
                    dtxsid=NULL,
                    parameters=NULL,
                    species='Human',
                    f = .01,
                    daily.dose=1,
                    doses.per.day=3,
                    dose.units = "mg/kg",
                    route = "oral",
                    days = 21,
                    output.units = "uM",
                    suppress.messages=FALSE,
                    tissue=NULL,
                    model='pbtk',
                    f.change = 0.00001,
                    dosing=NULL,
                    parameterize.args.list = list(),
                    ...)
{
  # We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')
  
# We need to know model-specific information (from modelinfo_[MODEL].R]) 
# to set up the solver:
  if (is.null(model)) stop("Model must be specified.")
  model <- tolower(model)

# Model has to be in the model.list:
  if (!(model %in% names(model.list)))            
  {
    dynamic.models <- names(model.list)[unlist(lapply(model.list,
          function(x) !is.null(x$solve.func)))]
    stop(paste("Model",model,"not available. Please select from:",
      paste(dynamic.models,collapse=", ")))
# Model has to be dynamic (to find steady-state, model must depend on time):
  } else if (is.null(model.list[[model]]$solve.func))
  {
    stop(paste("Model",model,"is not dynamic -- calc_css is not applicable."))
# Otherwise, proceed:
  } else {
    #Set more convenient names for various model-related variables (e.g. route
    #of exposure) stored in the list of lists, model.list, compiled in the
    #various models' associated "modelinfo_xxx.R" files:
    #
    # name of function that generates the model parameters:
    parameterize_function <- model.list[[model]]$parameterize.func
    # the names of the state variables of the model (so far, always in units of 
    # amounts)
    state.vars <- model.list[[model]]$state.vars
    # The compartment where we test for steady-state:
    # Check if set in function call:
    if (!is.null(tissue))
    {
      ss.compartment <- tissue
    # Then check to see if model sets a default:
    } else if (!is.null(model.list[[model]]$steady.state.compartment))
    {
      ss.compartment <- model.list[[model]]$steady.state.compartment
    # Otherwise plasma:
    } else ss.compartment <- "plasma"
  }   

  # We only want to call the parameterize function once:
  if (is.null(parameters))
  {
    parameters <- do.call(parameterize_function,
                          args=purrr::compact(c(list(
      chem.cas=chem.cas,
      chem.name=chem.name,
      dtxsid=dtxsid,
      species=species,
      suppress.messages=suppress.messages),
      parameterize.args.list)))
  }

  if (is.null(dosing))
  {
    dosing <- list(
      initial.dose=0,
      dosing.matrix=NULL,
      daily.dose=daily.dose,
      doses.per.day=doses.per.day
    )
  }
  
  # We need to find out what concentrations (roughly) we should reach before
  # stopping:
  analyticcss_params <- list(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid = dtxsid,
    species = species,
    parameters=parameters,
    dose=daily.dose,
    concentration='plasma',
    route=route,
    dose.units=dose.units,
    model=model,
    output.units = output.units,
    suppress.messages=TRUE,
    parameterize.args.list = parameterize.args.list
  )
  # Check to see if there is analytic Css funtion:
  if (!is.null(model.list[[model]]$analytic.css.func))
  {
     css <- do.call("calc_analytic_css", 
                 args=purrr::compact(c(
                                       analyticcss_params)))
  } else {
  # Otherwise set css to infinite:
    css <- Inf
  }
  
  # Use this conentration to cutoff the iterations:
  target.conc <- (1 - f) * css 

# Identify the concentration that we are intending to check for steady-state:
  target <- paste("C",ss.compartment,sep="") 

  # Initially simulate for a time period of length "days":
  # We monitor the state parameters plus the target (we need the state vars
  # in case we need to restart the simulation):
  monitor.vars <- unique(c(state.vars, target))
  
# set an initial precision (larger is faster):
  atol <- rtol <- 1e-6
# set an inintial of time steps per hour (smaller is faster):
  tsteps <- 4
# Initial call to solver, maybe we'll get lucky and achieve rapid steady-state
  out <- try(do.call(solve_model,
# we use purrr::compact to drop NULL values from arguments list:
      args=purrr::compact(c(list(    
      parameters=parameters,
    model=model, 
    dosing=dosing,
    route=route,
    species = species,
    input.units=dose.units,
    suppress.messages=TRUE,
    days=days,
    output.units = output.units,
    monitor.vars=monitor.vars,
    parameterize.args.list = parameterize.args.list,
    atol = atol,
    rtol = rtol,
    tsteps = tsteps),
    ...))))
    # Check for an error:
    RETRY <- inherits(out, "try-error")
    # Can only check if it ran long enough if not an error:
    if ("time" %in% colnames(out)) RETRY <- RETRY | !(days %in% out[,"time"])
# If the initial run crashes, try decreasing the tolerance (more precise,
# slower calculations):
    while (RETRY &
           atol > 1e-13) 
    {
      cat("Optimizing solver parameters...\n")
      atol <- atol/10
      rtol <- atol
      tsteps <- round(tsteps*1.5)
      out <- try(do.call(solve_model,
  # we use purrr::compact to drop NULL values from arguments list:
        args=purrr::compact(c(list(    
        parameters=parameters,
      model=model, 
      dosing=dosing,
      route=route,
      species = species,
      input.units=dose.units,
      suppress.messages=TRUE,
      days=days,
      output.units = output.units,
      monitor.vars=monitor.vars,
      parameterize.args.list = parameterize.args.list,
      atol = atol,
      rtol = rtol,
      tsteps = tsteps),
      ...))))
    # Check for an error:
    RETRY <- inherits(out, "try-error")
    # Can only check if it ran long enough if not an error:
    if ("time" %in% colnames(out)) RETRY <- RETRY | !(days %in% out[,"time"])
  }            
    
# Make sure we have the compartment we need: 
  if (!(target %in% colnames(out))) stop(paste(
    "Requested tissue",ss.compartment,"is not an output of model",model))
    
  Final_State <- out[dim(out)[1],monitor.vars]
  total.days <- days
  additional.days <- days
  NMinusOne_Conc <- try(out[match((additional.days - 1), 
                       floor(out[,'time'])), target])
  NMinusTwo_Conc <- try(out[match((additional.days - 2), 
                 floor(out[,'time'])), target])
                 
  # Check for problems:
  if (inherits(NMinusOne_Conc, "try-error") |
      inherits(NMinusTwo_Conc, "try-error")) stop(paste(
      "Could not solve model",
      model,
      "for",
      days,
      "days and route",
      route,
      "with tolerance",
      atol))

  # Check for problems:
  if (is.na(NMinusOne_Conc) |
      is.na(NMinusTwo_Conc)) 
    stop(paste(
      "Could not solve model",
      model,
      "for",
      days,
      "days and route",
      route,
      "with tolerance",
      atol))
      
  # Calculate the fractional change on the last simulated day:
  if (NMinusTwo_Conc > 0)
  {
    conc.delta <- abs(NMinusOne_Conc -
                  NMinusTwo_Conc) /
                  NMinusTwo_Conc 
  } else conc.delta <- 0
  
# Until we reach steady-state, keep running the solver for longer times, 
# restarting each time from where we left off:
  while(all(out[,target] < target.conc) & 
       (conc.delta > f.change))
  {
    cat("Extending simulation...\n")
    if(additional.days < 1000)
    {
      additional.days <- additional.days * 5
    }
    total.days <- total.days + additional.days

    out <- do.call(solve_model,
# we use purrr::compact to drop NULL values from arguments list:
      args=purrr::compact(c(list(    
      parameters=parameters,
      model=model,
      initial.values = Final_State[state.vars],  
      dosing=dosing,
      route=route,
      species = species,
      input.units=dose.units,
      days = additional.days,
      output.units = output.units,
      monitor.vars=monitor.vars,
      parameterize.args.list = parameterize.args.list,   
      suppress.messages=TRUE,
      atol = atol,
      rtol = rtol,
      tsteps=tsteps,
      ...))))

    Final_State <- out[dim(out)[1],monitor.vars]
    NMinusOne_Conc <- try(out[match((additional.days - 1), 
                         floor(out[,'time'])), target])
    NMinusTwo_Conc <- try(out[match((additional.days - 2), 
                   floor(out[,'time'])), target])
                   
    # Check for problems:
    if (inherits(NMinusOne_Conc, "try-error") |
        inherits(NMinusTwo_Conc, "try-error")) browser()
  
    # Check for problems:
    if (is.na(NMinusOne_Conc) |
        is.na(NMinusTwo_Conc)) 
      stop(paste(
        "Could not solve model",
        model,
        "for",
        days,
        "days and route",
        route,
        "with tolerance",
        atol))

    # Calculate the fractional change on the last simulated day:
    if (NMinusTwo_Conc > 0)
    {
      conc.delta <- abs(NMinusOne_Conc -
                    NMinusTwo_Conc) /
                    NMinusTwo_Conc 
    } else conc.delta <- 0
              
    if(total.days > 36500) break 
  }
  
# Calculate the day Css is reached:
  if (total.days < 36500)
  {
    # The day the simulation started:
    sim.start.day <- total.days - additional.days
    
    # Find the first day the current simulation reached max concentration:
    sim.css.day <- floor(min(out[which(out[,target]/max(out[,target])==1),
                         "time"]))
    
    # The overall day the simulation reached Css:
    css.day <- sim.start.day+sim.css.day
    # Fraction of analytic Css achieved:
    last.day.subset<-subset(out,out[,"time"]<(sim.css.day+1) &
                                out[,"time"]>=(sim.css.day))
    frac_achieved <- as.numeric(mean(last.day.subset[,target])/css)
  } else{ 
   if(!suppress.messages)cat("Analytic css not reached after 100 years.")
   css.day  <- 36500
   frac_achieved <- as.numeric(max(out[,target])/css)  
  }     
  
  # Calculate the peak concentration:
  max.conc <- as.numeric(max(out[,target]))
  # Calculate the mean concentration on the last day:
  avg.conc <- mean(as.numeric(subset(out, 
    out[,"time"] > additional.days-1)[,target]))
   
  return(list(
    avg=set_httk_precision(avg.conc),
    frac=set_httk_precision(frac_achieved), 
    max=set_httk_precision(max.conc),
    the.day =set_httk_precision(as.numeric(css.day))))
}
