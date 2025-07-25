#' Calculate the volume of distribution for a one compartment model.
#'
#' This function predicts partition coefficients for all tissues using
#' \code{\link{predict_partitioning_schmitt}}, then lumps them
#' into a single compartment.
#' 
#' The effective volume of distribution is calculated by summing each tissues
#' volume times it's partition coefficient relative to plasma. Plasma, and the
#' paritioning into RBCs are also added to get the total volume of distribution
#' in L/KG BW. Partition coefficients are calculated using Schmitt's (2008)
#' method.  When species is specified as rabbit, dog, or mouse, the function
#' uses the appropriate physiological data(volumes and flows) but substitues
#' human fraction unbound, partition coefficients, and intrinsic hepatic
#' clearance.
#' 
#' 
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified when Funbound.plasma is not given in parameter list. 
#' @param chem.cas Either the CAS number or the chemical name must be specified
#' when Funbound.plasma is not given in parameter list. 
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param parameters Parameters from parameterize_3comp, parameterize_pbtk or
#' predict_partitioning_schmitt.
#' 
#' @param default.to.human Substitutes missing animal values with human values
#' if true.
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human"). 
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param adjusted.Funbound.plasma Uses Pearce et al. (2017) lipid binding adjustment
#' for Funbound.plasma (which impacts partition coefficients) when set to TRUE (Default).
#' 
#' @param ... Additional parameters passed to parameterize function if 
#' parameters is NULL.
#' 
#' @return \item{Volume of distribution}{Units of L/ kg BW.}
#' 
#' @author John Wambaugh and Robert Pearce
#' 
#' @references
#'
#' \insertRef{schmitt2008general}{httk}
#' 
#' \insertRef{peyret2010unified}{httk}
#' 
#' @keywords Parameter 1compartment
#'
#' @seealso \code{\link{predict_partitioning_schmitt}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#' 
#' @examples
#' 
#' calc_vdist(chem.cas="80-05-7")
#' calc_vdist(chem.name="Bisphenol A")
#' calc_vdist(chem.name="Bisphenol A",species="Rat")
#'
#' # Create a list of parameters (that you can potentially change):
#' p <- parameterize_schmitt(chem.name="propranolol")
#' # Need to use those parameters to predict partition coefficients:
#' PCs <- predict_partitioning_schmitt(parameters = p)
#' 
#' # Lump the tissues into a single volume of distribution
#' calc_vdist(parameters=c(p,PCs))
#' # Should be the same as chemical by name:
#' calc_vdist(chem.name="propranolol")
#'
#' \donttest{
#' # Different ways to give the arguments:
#' calc_vdist(chem.cas="80-05-7")
#' params <- parameterize_schmitt(chem.name="triclosan")
#' params <- c(params, predict_partitioning_schmitt(parameters = params))
#' calc_vdist(parameters=params)
#' params <- parameterize_3comp(chem.name="triclosan")
#' calc_vdist(parameters=params)
#' params <- parameterize_pbtk(chem.name="triclosan")
#' calc_vdist(parameters=params)
#' }
#'
#' @export calc_vdist
calc_vdist<- function(chem.cas=NULL,
                      chem.name=NULL,
                      dtxsid=NULL,
                      parameters=NULL,
                      suppress.messages=FALSE,
                      adjusted.Funbound.plasma = TRUE,
                      species="Human",
                      default.to.human = FALSE,
                      ...
                      )
{
  physiology.data <- physiology.data
  Parameter <- NULL

# We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid) &
      is.null(parameters)) 
    stop('Parameters, chem.name, chem.cas, or dtxsid must be specified.')
    
  if (is.null(parameters))
  {
    # Look up the chemical name/CAS, depending on what was provide:
    out <- get_chem_id(
            chem.cas=chem.cas,
            chem.name=chem.name,
            dtxsid=dtxsid)
    chem.cas <- out$chem.cas
    chem.name <- out$chem.name                                
    dtxsid <- out$dtxsid
  
    schmitt.parameters <- do.call(parameterize_schmitt,  
                                  args=purrr::compact(c(
                                    list(chem.cas=chem.cas,
                                         chem.name=chem.name,
                                         dtxsid=dtxsid,
                                         suppress.messages=suppress.messages,
                                         adjusted.Funbound.plasma = adjusted.Funbound.plasma,
                                         species=species,
                                         default.to.human=default.to.human
                                         ),
   # Send only the arguments in ... wanted by the function:
                                    list(...)[names(formals(parameterize_schmitt))]
                                    )))
    parameters <- suppressWarnings(do.call(predict_partitioning_schmitt,
                                   args=purrr::compact(c(
                                     list(parameters=schmitt.parameters,
                                          suppress.messages=suppress.messages),
   # Send only the arguments in ... wanted by the function:
                                    list(...)[names(formals(predict_partitioning_schmitt))]
                                       ))))
                                       
    # Should we use the adjusted Funbound plasma?
    if (adjusted.Funbound.plasma) parameters <- 
          c(parameters,schmitt.parameters['Funbound.plasma'])
      else parameters <- 
        c(parameters,Funbound.plasma=schmitt.parameters[['unadjusted.Funbound.plasma']])
  }

  if(any(names(parameters) %in% schmitt.specific.names) &
    !all(schmitt.specific.names %in% names(parameters)))
    stop("All predict_partitioning_schmitt coefficients must be included if not using pbtk or 3compartment parameters.")
  else if(all(model.list[["schmitt"]]$param.names %in% names(parameters)))
    schmitt.params  <- T
  else schmitt.params <- F
                                                                                       

  if(schmitt.params & !('funbound.plasma' %in% tolower(names(parameters))))
  {
    if (is.null(chem.cas) & is.null(chem.name))
      stop("Specify chem.name or chem.cas with correct species if not including Funbound.plasma with predict_partitioning_schmitt coefficients.")
    else if(is.null(chem.cas))
    {
      out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
      chem.cas <- out$chem.cas
    }
    fup <- try(get_invitroPK_param("Funbound.plasma",species,chem.cas=chem.cas),silent=TRUE)
    if (is(fup,"try-error") & default.to.human) 
    {
      fup <- try(get_invitroPK_param("Funbound.plasma","Human",chem.cas=chem.cas),silent=TRUE)
      warning(paste(species,"coerced to Human for protein binding data."))
    }
    if (is(fup,"try-error")) stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
    if (fup == 0)
    {
      fup <- 0.005
      warning("Fraction unbound = 0, changed to 0.005.")
    }
    if(adjusted.Funbound.plasma){
      Flipid <- subset(physiology.data,
        Parameter=='Plasma Effective Neutral Lipid Volume Fraction')[,
          which(tolower(colnames(physiology.data)) == tolower(species))]
      pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",
                                                       chem.cas=chem.cas))
      pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",
                                                        chem.cas=chem.cas))
      Pow <- 10^get_physchem_param("logP",chem.cas=chem.cas)
      ion <- calc_ionization(pH=7.4,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)
      dow <- Pow * (ion$fraction_neutral + 0.001 * ion$fraction_charged + 
                      ion$fraction_zwitter)
      fup <- 1 / ((dow) * Flipid + 1 / fup)
    }
    parameters <- c(parameters,Funbound.plasma=fup)  
  }
  
  
# Check the species argument for capitalization problems and whether or not it 
# is in the table:  
  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- colnames(physiology.data)[
        toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species

# Load the physiological parameters for this species
  this.phys.data <- physiology.data[,phys.species]
  names(this.phys.data) <- physiology.data[,1]

  if ("hematocrit" %in% names(parameters))
  {
    hematocrit <- parameters[["hematocrit"]]
  } else hematocrit <- this.phys.data["Hematocrit"]

  plasma.vol <- this.phys.data["Plasma Volume"]/1000 # L/kg BW
  RBC.vol <- plasma.vol/(1 - hematocrit)*hematocrit
  if (all(schmitt.specific.names %in% names(parameters)))
  {
    PC.names <- names(parameters)[regexpr("K",names(parameters))==1] #Should pass only
    #partition coefficients to lump_tissues()
    if (is.data.table(parameters))
    {
      PCs <- parameters[,PC.names,with=FALSE]
    } else {
      PCs <- subset(parameters,names(parameters) %in% PC.names)
    }
   # Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
    lumped_params <- lump_tissues(
      PCs,
      tissuelist=NULL,
      species=species,
      model="1compartment")
 
   vol.dist <- plasma.vol +
      RBC.vol*lumped_params$Krbc2pu*parameters$Funbound.plasma+
      lumped_params$Krest2pu*lumped_params$Vrestc*parameters$Funbound.plasma
    } else {
     if(!all(model.list[["schmitt"]]$param.names %in% names(parameters)) &
        !all(model.list[["3comp"]]$param.names %in% names(parameters)) &
        !all(model.list[["pbtk"]]$param.names %in% names(parameters)))
        stop("Use parameter lists from parameterize_pbtk, parameterize_3compartment, or predict_partitioning_schmitt only.")

    vol.dist <- plasma.vol +
      RBC.vol*parameters[["Krbc2pu"]]*parameters$Funbound.plasma
    lastchar <- function(x){substr(x, nchar(x), nchar(x))}
    firstchar <- function(x){substr(x, 1,1)}
    scaled.volumes <- names(parameters)[firstchar(names(parameters))=="V" &
                                          lastchar(names(parameters))=="c"]
    PCs <- names(parameters)[firstchar(names(parameters))=="K"]
    comps <- intersect(substr(scaled.volumes,2,nchar(scaled.volumes)-1), 
                       substr(PCs,2,nchar(PCs)-3)) 
    comps <-  comps[!(comps %in% c('art','ven'))]        
    for(this.comp in comps){
      eval(parse(text=paste('vol.dist <- vol.dist + ', 
        parameters[[scaled.volumes[grep(this.comp,scaled.volumes)]]],
        '*', 
        parameters[[PCs[grep(this.comp,PCs)]]],
        '*',
        parameters$Funbound.plasma))) # L 
    }
  }
    
  if (!suppress.messages)
  {
    if (is.null(chem.name) & is.null(chem.cas)) 
    {
      cat("Volume of distribution returned in units of L/kg BW.\n")
    }
    else cat(paste(toupper(substr(species,1,1)),
                   substr(species,2,nchar(species)),
                   sep=''),
             "volume of distribution returned in units of L/kg BW.\n")
  }
  return(set_httk_precision(as.numeric(vol.dist)))
}
