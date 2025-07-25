#' Calculate the steady state concentration for the sum of clearances
#' steady-state model with exhalation
#'
#' This function calculates the analytic steady state plasma or venous blood 
#' concentrations as a result of infusion dosing.
#'
#' @param chem.name Either the chemical name, CAS number, or the parameters must 
#' be specified.
#'
#' @param chem.cas Either the chemical name, CAS number, or the parameters must 
#' be specified.
#'
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#'  default "Human").
#'  
#' @param parameters Chemical parameters from \code{\link{parameterize_sumclearances}} overrides chem.name and chem.cas.
#'
#' @param hourly.dose Hourly dose rate mg/kg BW/h.
#'
#' @param concentration Desired concentration type, 'blood' or default 'plasma'.
#'
#' @param suppress.messages Whether or not the output message is suppressed.
#'
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical 
#' in the blood to plasma using the input parameters. Use this if you have 
#' 'altered hematocrit, Funbound.plasma, or Krbc2pu.
#'
#' @param tissue Desired tissue concentration (defaults to whole body 
#'concentration.)
#'
#' @param restrictive.clearance If TRUE (default), then only the fraction of
#' chemical not bound to protein is available for metabolism in the liver. If 
#' FALSE, then all chemical in the liver is metabolized (faster metabolism due
#' to rapid off-binding). 
#'
#' @param bioactive.free.invivo If FALSE (default), then the total concentration is treated
#' as bioactive in vivo. If TRUE, the the unbound (free) plasma concentration is treated as 
#' bioactive in vivo. Only works with tissue = NULL in current implementation.
#'
#' @param route Route of exposure ("inhalation" or [DEFAULT] "oral").
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical to
#' basolateral data \code{Caco2.Pab}, default is Caco2.options = list(Caco2.Pab.default = 1.6,
#' Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the default value for 
#' Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE uses Caco2.Pab to calculate
#' fabs.oral, otherwise fabs.oral = \code{Fabs}. Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#' 
#' 
#' @param dosing List of dosing metrics used in simulation, which includes
#' the namesake entries of a model's associated dosing.params. For steady-state
#' calculations this is likely to be either "daily.dose" for oral exposures or
#' "Cinhaled" for inhalation.
#'
#' @param dose.units The units associated with the dose received.
#' 
#' @param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
#'  
#' @return Steady state plasma concentration in mg/L units
#'
#' @seealso \code{\link{calc_analytic_css}}
#'
#' @seealso \code{\link{parameterize_steadystate}}
#'
#' @author John Wambaugh
#'
#' @keywords sumclearances steady-state
#'
#' @export calc_analytic_css_sumclearances
calc_analytic_css_sumclearances <- function(chem.name=NULL,
                                   chem.cas = NULL,
                                   dtxsid = NULL,
                                   species="Human",
                                   parameters=NULL,
                                   dosing=list(daily.dose=1),
                                   hourly.dose = NULL,
                                   dose.units = "mg",
                                   concentration='plasma',
                                   Caco2.options = NULL,
                                   suppress.messages=FALSE,
                                   recalc.blood2plasma=FALSE,
                                   tissue=NULL,
                                   route="oral",
                                   restrictive.clearance=TRUE,
                                   bioactive.free.invivo = FALSE,
                                   ...)
{
  if (!is.null(hourly.dose))
  {
     warning("calc_analytic_css_sumclearances deprecated argument hourly.dose replaced with new argument dose, value given assigned to dosing.")
     dosing <- list(daily.dose = 24*hourly.dose)
  }
  
  parameterize.arg.list <- list(...)
  
# Load from modelinfo file:
  THIS.MODEL <- "sumclearances"
  param.names <- model.list[[THIS.MODEL]]$param.names
  param.names.schmitt <- model.list[["schmitt"]]$param.names
  parameterize_function <- model.list[[THIS.MODEL]]$parameterize.func

  wrong.dose.params <- names(dosing)[!(names(dosing) %in%
                             model.list[[THIS.MODEL]]$routes[[route]][["dosing.params"]])]
  if (length(wrong.dose.params) > 0) stop(
      paste("Dosing params",
      paste(wrong.dose.params,collapse=", "), 
      "not correct for model",
      THIS.MODEL, 
      "and route", 
      route))
      
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('parameters, chem.name, chem.cas, or dtxsid must be specified.')

# Expand on any provided chemical identifiers if possible (if any but not
# all chemical descriptors are NULL):
  chem_id_list  = list(chem.cas, chem.name, dtxsid)
  if (any(unlist(lapply(chem_id_list, is.null))) &
      !all(unlist(lapply(chem_id_list, is.null)))){
  out <- get_chem_id(
    chem.cas=chem.cas,
    chem.name=chem.name,
    dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name                                
  dtxsid <- out$dtxsid  
  }
  
# Fetch some parameters using parameterize_steadstate, if needed:
  if (is.null(parameters))
  {
    if (recalc.blood2plasma) 
    {
      warning("Argument recalc.blood2plasma=TRUE ignored because parameters is NULL.")
    }
    
    parameters <- do.call(what=parameterize_function, 
                          args=purrr::compact(c(
                            list(chem.cas=chem.cas,
                                 chem.name=chem.name,
                                 species = species,
                                 suppress.messages=suppress.messages,
                                 Caco2.options = Caco2.options,
                                 restrictive.clearance = restrictive.clearance
                                 ),
                            ...)))

  } else {
    if (!all(param.names %in% names(parameters)))
    {
      stop(paste("Missing parameters:",
                 paste(param.names[which(!param.names %in% names(parameters))],
                   collapse=', '),
                 ".  Use parameters from parameterize_steadystate."))
    }
  }
  if (any(parameters$Funbound.plasma == 0)) 
  {
    stop('Fraction unbound plasma cannot be zero.')
  }
#  if (is.na(parameters$hepatic.bioavailability)) browser() 
  if (recalc.blood2plasma) 
  {
    rb2p_args <- purrr::compact(
      c(list(chem.cas=chem.cas,
           parameters=parameters,
           hematocrit=parameters$hematocrit,
           species = species),
        #any additional args specified in ... (captured in parameterize.arg.list)
        parameterize.arg.list[
          setdiff(
            intersect(
              names(parameterize.arg.list),
              names(formals(calc_rblood2plasma))
            ),
            c("chem.cas", "parameters", "hematocrit", "species")
          )
        ]
    )
    )
    
    parameters$Rblood2plasma <- do.call(what = calc_rblood2plasma,
                                        args = rb2p_args)
  }

  Fup <- parameters$Funbound.plasma
  Rb2p <- parameters$Rblood2plasma 
  BW <- parameters$BW

  # Oral bioavailability:
  Fabsgut <- parameters$Fabsgut
  Fhep <- parameters$hepatic.bioavailability
  
  # Total blood flow (gut plus arterial) into liver:
  Qtotalliver <- parameters$Qtotal.liverc/BW^0.25 #L/h/kg BW
  
  # Glomerular filtration (for kidney elimination):
  Qgfr <- parameters$Qgfrc/BW^0.25 #L/h/kg BW

  # Scale up from in vitro Clint to a whole liver clearance:
  Clhep_args <- purrr::compact(
    c(list( parameters=parameters,
            hepatic.model='well-stirred',
            restrictive.clearance = restrictive.clearance,
            suppress.messages=TRUE,
           species = species),
      #any additional args specified in ... (captured in parameterize.arg.list)
      parameterize.arg.list[
        setdiff(
          intersect(
            names(parameterize.arg.list),
            names(formals(calc_hep_clearance))
          ),
          c("parameters", "hepatic.model", "restrictive.clearance", "suppress.messages", "species")
        )
      ]
    )
  )
    
    Clhep <- do.call(what = calc_hep_clearance,
                     args = Clhep_args) #L/h/kg body weight

  # Inhalation parameters
  Qalv <- parameters$Qalvc/BW^0.25 #L/h/kg BW
  Kblood2air <- parameters$Kblood2air
 
  # Calculate steady-state blood Css, Pearce et al. (2017) equation section 2.2:
  if (route %in% "oral")
  { 
    hourly.dose <- dosing[["daily.dose"]] /
                     BW / 
                     24 * 
                     convert_units(MW = parameters[["MW"]], 
                                   dose.units, 
                                   "mg") # mg/kg/h

# Steady-state chemical concentration in blood in units of mg/L:
      Css_blood <- hourly.dose * # Oral dose rate mg/kg/h
                   Fabsgut * # Fraction of dose absorbed from gut (in vivo or Caco-2)
                   Fhep * # Fraction of dose that escapes first-pass hepatic metabolism
                   Rb2p / # Blood to plasma concentration ratio
                   (
                     Qgfr * Fup + # Glomerular filtration to proximal tubules (kidney) L/h/kg BW
                     Rb2p * Qalv/Kblood2air + # Exhalation clearance L/h/kg BW
                     Clhep # Well-stirred hepatic metabolism (liver) L/h/kg BW
                   )
  } else if (route %in% "inhalation") {
    CinhaledmgpL <- dosing[["Cinhppmv"]] * 
                      convert_units(MW = parameters[["MW"]],
                                    dose.units,
                                    "mg/L", 
                                    state="gas") # mg/l
    
# Steady-state chemical concentration in blood in units of mg/L:
    Css_blood <- CinhaledmgpL * # Inhaled concentration mg/L
                 Qalv * # Alveolar air flow L/h
                 Rb2p / # Blood to plasma concentration ratio  
                 (
                   Clhep + # Well-stirred hepatic metabolism (liver) L/h/kg BW
                   Fup * Qgfr  + # Glomerular filtration from blood L/h L/h/kg BW
                   Qalv * Rb2p / Kblood2air # Exhalation rate L/h L/h/kg BW
                 ) 
  } else stop("Route must be either oral or inhalation.")
  
  # Convert from blood to plasma:
  Css <- Css_blood/Rb2p
    
    
# Check to see if a specific tissue was asked for:
  if (!is.null(tissue))
  {
    # We need logP, the pKa's, and membrane affinity, which currently isn't one 
    # of the sumclearances parameters, so unless the user provides these parameters,
    # they need to give a chemical identifier like chem.name/chem.cas/dtxsid, or
    # we can't find them in the chem.physical_and_invitro.data set and run:
    if (!any(c("Pow", "MA", "pKa_Accept", "pKa_Donor") %in% 
             names(parameters))) {
      #We do a lookup of these needed parameters using a targeted version of 
      #get_physchem_param for the 3 compss model, add_schmitt.param_to_3compss
      #(function definition nested at bottom):
        parameters <- add_schmitt.param_to_3compss(parameters = parameters,
           chem.cas = chem.cas, chem.name = chem.name, dtxsid = dtxsid)
    }

    #The parameters used in predict_partitioning_schmitt may be a compound
    #data.table/data.frame or list object, however, depending on the source 
    #of the parameters. In calc_mc_css, for example, parameters is received 
    #as a "data.table" object. Screen for processing appropriately, and 
    #pass our parameters to predict_partitioning_schmitt so we can get
    #the needed pc's.
    if (any(class(parameters) == "data.table")){
      parameters_in <- parameters[, param.names.schmitt[param.names.schmitt %in% 
                                                          names(parameters)],
                                  with = FALSE]
    }else if (is(parameters,"list")){
      parameters_in <- parameters[param.names.schmitt[param.names.schmitt %in% 
                                                        names(parameters)]]
    }else stop('httk is only configured to process parameters as objects of 
               class list or class compound data.table/data.frame.')
    
    #put together argument list for predict_partitioning_schmitt
    #use purrr::compact to remove any NULL or empty lists
    schmitt_args <- purrr::compact(
      c(list(
        parameters = parameters_in,
        species = species,
        model = THIS.MODEL
      ),
      #any additional args specified in ... (captured in parameterize.arg.list)
      parameterize.arg.list[
        setdiff(
          intersect(
            names(parameterize.arg.list),
            names(formals(predict_partitioning_schmitt))
          ),
          c("parameters", "species", "model")
        )
      ]
      )
    )
    
    pcs <- do.call(what = predict_partitioning_schmitt,
                   args = schmitt_args)
    
    
    if (!paste0('K',tolower(tissue)) %in% 
      substr(names(pcs),1,nchar(names(pcs))-3))
    {
      stop(paste("Tissue",tissue,"is not available."))
    }

    Css <- Css * pcs[[names(pcs)[substr(names(pcs),2,nchar(names(pcs))-3)==tissue]]] * Fup   
  }

  if(tolower(concentration) != "tissue"){
    
    if (tolower(concentration)=='blood')
    {
      Css <- Css * Rb2p
      
    }else if(bioactive.free.invivo == TRUE & tolower(concentration) == 'plasma'){
      
      Css <- Css * parameters[['Funbound.plasma']]
      
    } else if (tolower(concentration)!='plasma') stop("Only blood and plasma concentrations are calculated.")      
  }
  return(Css)
}
