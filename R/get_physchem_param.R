#' Get physico-chemical parameters from chem.physical_and_invitro.data table
#'
#' This function retrieves physico-chemical properties ("param") for the 
#' chemical specified by chem.name or chem.cas from the table
#' \code{\link{chem.physical_and_invitro.data}}. This 
#' function is distinguished from \code{\link{get_invitroPK_param}} in that
#' there are no species-specific values. Physically meaningful values for 
#' ionization equilibria are NA/none (that is, no ionization), a single value, 
#' or a series of values separated by commas. If logMA (log10 membrane affinity) 
#' is NA, we use calc_ma() to predict it later on in the model parameterization 
#' functions.
#' 
#' @details 
#' Note that this function works with a local version of the 
#' \code{\link{chem.physical_and_invitro.data}} table to allow users to 
#' add/modify chemical
#' data (for example, adding new data via \code{\link{add_chemtable}} or 
#' loading in silico predictions distributed with httk via
#' \code{\link{load_sipes2017}}, \code{\link{load_pradeep2020}},
#' \code{\link{load_dawson2021}}, or \code{\link{load_honda2023}}).
#' 
#' User can request the following via argument param (case-insensitive):
#' \tabular{lll}{
#' \strong{Parameter} \tab \strong{Description} \tab \strong{Units} \cr
#' MW  \tab Molecular weight \tab g/mole \cr 
#' pKa_Donor \tab Hydrogen donor ionization equilibria (acidic pKa) \tab pH \cr
#' pKa_Accept \tab Hyrdogen acceptor ionization equilibria (basic pKa \tab pH \cr 
#' logMA \tab log10 Membrane Affinity \tab unitless \cr
#' logP \tab log10 Octanol:Water Partition Coefficient (hydrophobicity) \tab unitless \cr
#' logPwa \tab log10 Water:Air Partition Coefficient \tab unitless \cr
#' logHenry \tab log10 Henry's Law Constant \tab atm-m3/mole \cr
#' logWSol \tab log10 Water Solubility \tab moles/L: Water solubility at 25C \cr
#' MP \tab Melting point \tab  deg C \cr
#' }
#' 
#' @param param The desired parameters, a vector or single value.
#' @param chem.name The chemical names that you want parameters for, a vector or single value
#' @param chem.cas The chemical CAS numbers that you want parameters for, a vector or single value
#' @param dtxsid EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @seealso \code{\link{chem.physical_and_invitro.data}} 
#' @seealso \code{\link{get_invitroPK_param}} 
#' @seealso \code{\link{add_chemtable}} 
#'
#' @return The parameters, either a single value, a named list for a single chemical, or a list of lists
#' 
#' @author John Wambaugh and Robert Pearce
#'
#' @examples 
#'
#' get_physchem_param(param = 'logP', chem.cas = '80-05-7')
#' get_physchem_param(param = c('logP','MW'), chem.cas = c('80-05-7','81-81-2'))
#' # This function should be case-insensitive:
#' try(get_physchem_param(chem.cas="80-05-7","LogP"))
#' # Asking for a parameter we "don't" have produces an error:
#' try(get_physchem_param(chem.cas="80-05-7","MA"))
#' get_physchem_param(chem.cas="80-05-7","logMA")
#' # Ionization equilibria can be NA/none, a single value, or a series of values
#' # separated by commas:
#' get_physchem_param(chem.cas="80-05-7","pKa_Donor")
#' get_physchem_param(chem.cas="80-05-7","pKa_Accept")
#' get_physchem_param(chem.cas="71751-41-2","pKa_Donor")
#' get_physchem_param(chem.cas="71751-41-2","pKa_Accept")
#' # If logMA (log10 membrane affinity) is NA, we use calc_ma() to predict it
#' # in the parameterization functions:
#' get_physchem_param(chem.cas="71751-41-2","logMA")
#' parameterize_steadystate(chem.cas="71751-41-2")
#' 
#' @export get_physchem_param 
get_physchem_param <- function(
                        param, 
                        chem.name=NULL,
                        chem.cas=NULL,
                        dtxsid=NULL)
{
# Define list of acceptable paramters:
  GOOD.PARAMS <- c("MW",
                   "pKa_Donor",
                   "pKa_Accept",
                   "logMA",
                   "logP",
                   "logHenry",
                   "logWSol",
                   "MP",
                   "logPwa",
                   "Chemical.Class")
  NON.NUMERIC.PARAMS <- c(
                   "pKa_Donor",
                   "pKa_Accept",
                   "Chemical.Class")
  ACCEPTABLE.NA.PARAMS <- c(
                   "logMA",
                   "pKa_Donor",
                   "pKa_Accept",
                   "Chemical.Class") 

  chem.physical_and_invitro.data <- chem.physical_and_invitro.data
  
  chem.cas0 <- chem.cas
  chem.name0 <- chem.name
  dtxsid0 <- dtxsid
  
# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) ) 
    stop('chem.name, chem.cas, or dtxsid must be specified.')
    
  # Look up the chemical name/CAS, depending on what was provided:
  if (any(is.null(chem.cas),is.null(chem.name),is.null(dtxsid)))
  {
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  }
  
  if (!is.null(chem.cas0) & any(is.na(chem.name)))
  {
    stop(paste(
      "CAS not matched in chem.physical_and_invitro.data for input CAS:", 
      paste(chem.cas0[is.na(chem.name)], collapse = ",")))
  }
  if (!is.null(chem.name0) & any(is.na(chem.cas)))
  {
    stop(paste(
      "Compound name not matched in chem.physical_and_invitro.data for input Compounds:", 
      paste(chem.name0[is.na(chem.cas)], collapse = ",")))
  }
  if (!is.null(dtxsid0) & any(is.na(chem.cas)))
  {
    stop(paste(
      "DTXSID not matched in chem.physical_and_invitro.data for input DTXSID:", 
      paste(dtxsid0[is.na(chem.cas)], collapse = ",")))
  }
  
  if(!all(tolower(param) %in% 
     tolower(GOOD.PARAMS)))
  {
    stop(paste("Requested parameter \"",
      param,
      "\" not among \"",
      paste(GOOD.PARAMS, sep = "\", \""),
      ".\n", sep = ""))
  }
  
  # Match to identifier containing all chemicals -- CHANGED BY AMEADE 2/9/2023
  num.chems <- max(length(chem.cas0), 
                   length(chem.name0), 
                   length(dtxsid0),
                   na.rm=TRUE)
  if (length(dtxsid) == num.chems) this.index <- 
    match(dtxsid, chem.physical_and_invitro.data[,"DTXSID"])
  else if (length(chem.cas) == num.chems) this.index <- 
    match(chem.cas, chem.physical_and_invitro.data[,"CAS"])
  else if (length(chem.name) == num.chems) this.index <- 
    match(chem.name, chem.physical_and_invitro.data[,"Compound"])
  else stop("The chemical identifiers, dtxsid, chem.cas, or chem.name, were not all present in chem.physical_and_invitro.data.")

  # Make code case insensitive:
  PARAM <- colnames(chem.physical_and_invitro.data)[
                    tolower(colnames(chem.physical_and_invitro.data))
                    %in%
                    tolower(param)]
  param <- tolower(param)
  
  # We allow "pKa_Donor","pKa_Accept", and "logMA" to have the value "NA"
  # For the pKa's the value of "NA" means that the chemical doesn't ionize
  # For logMA we have a built-in predictor that can be used if logMA is NA
  if (!any(is.na(suppressWarnings(
    chem.physical_and_invitro.data[this.index,
                                  PARAM[!param %in% 
                                        tolower(ACCEPTABLE.NA.PARAMS)]]))) | 
     any(param %in% tolower(ACCEPTABLE.NA.PARAMS)))
  {
    col.numbers <- NULL
    for (this.param in param) col.numbers <- c(col.numbers,
         which(tolower(colnames(chem.physical_and_invitro.data)) == this.param))
    values <- chem.physical_and_invitro.data[this.index, col.numbers]
# We want to make sure the values returned are numeric, unless they are pKa's
# pKa's can be a comma separated list:
    if (any(!param %in% tolower(c("pKa_Accept", "pKa_Donor"))))
    {
      values.out <- lapply(as.list(values[!param %in% 
                                   tolower(NON.NUMERIC.PARAMS)]), 
                                   as.numeric)
    } else {
      values.out <- list()
    }

# Chemical class is text and should be added to values.out if requested:
    if (any(param %in% tolower(c("Chemical.Class"))))
    { 
      if (is.na(values[param %in% tolower(c("Chemical.Class"))]))
        values[param %in% tolower(c("Chemical.Class"))] <- ""
      values.out[["Chemical.Class"]] <-
        values[param %in% tolower(c("Chemical.Class"))]
    }

# Sometimes pKa's are stored as a semi-colon separated list, we replace the
# semi-colons with commas:
    if(any(param %in% tolower(c("pKa_Donor", "pKa_Accept"))))
    {
      if(length(param) > 1)
      {
# Sadly have to handle this differently if multiple chemical identifiers are 
# given or multiple parameters are requested:
        if(length(chem.cas) > 1)
        {
          if(tolower("pKa_Donor") %in% param)
          {
            values.out[["pKa_Donor"]] <- gsub(";",",",values[,"pKa_Donor"])
          }
          if(tolower("pKa_Accept") %in% param)
          {
            values.out[["pKa_Accept"]] <- gsub(";",",",values[,"pKa_Accept"])
          }
        } else {
          if(tolower("pKa_Donor") %in% param)
          {
            values.out[["pKa_Donor"]] <- 
              unlist(gsub(";",",",values[,"pKa_Donor"]))
          }
          if(tolower("pKa_Accept") %in% param)
          {
            values.out[["pKa_Accept"]] <- 
              unlist(gsub(";",",",values[,"pKa_Accept"]))
          }
        }
      } else {
        if(tolower("pKa_Donor") %in% param)
        {
          values.out[["pKa_Donor"]] <- gsub(";",",",values)
        }
        if(tolower("pKa_Accept") %in% param)
        {
          values.out[["pKa_Accept"]] <- gsub(";",",",values)
        }
      }
    }
    
    if (length(this.index) == 1 & length(param) == 1)
    {
      return(unlist(values.out))
    } else if (length(this.index) >= 1 & length(param) > 1)
    {
      return(values.out)
    } else if (length(this.index) > 1 & length(param == 1))
    {
      if (param %in% tolower(c("pKa_Accept", "pKa_Donor")))
      {
        return(values.out[[PARAM]])
      } else {
        return(unlist(values.out))
      }
    }
  } else {
    if(length(this.index) == 1 & length(param) == 1){
      stop(paste0("Incomplete phys-chem data for ",
                   chem.name,
                   " -- missing ",
                   param,"."))
    }else{
      missing.param <- which(is.na(chem.physical_and_invitro.data[
        this.index,PARAM[!param %in% tolower(ACCEPTABLE.NA.PARAMS)]]), arr.ind = T)
      
      if(length(this.index) >= 1 & length(param) > 1)
      {
        stop(paste0("Missing phys-chem data for combinations of: \n",
          paste(lapply(unique(missing.param[,1]), 
          function(x) paste0(chem.cas[x], ": ", 
            paste(PARAM[!param %in% 
              tolower(ACCEPTABLE.NA.PARAMS)][
                  missing.param[missing.param[,1] %in% x,2]],
               collapse = ", "))),
            collapse = "\n")))
       
      } else if (length(this.index) > 1 & length(param == 1))
      {
        stop(paste0("Incomplete phys-chem data for ", param, " for: \n",
                   paste(chem.cas[missing.param], collapse = ",")
                   ))
      }
    }
  }
}
