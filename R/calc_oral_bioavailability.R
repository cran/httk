#' Functions for calculating the bioavaialble fractions from oral doses
#' 
#' These functions calculate the fraction of chemical absorbed from the gut
#' based upon in vitro measured Caco-2 membrane permeability data.
#' Caco-2 permeabilities (\eqn{10^{-6}} cm/s) are related to 
#' effective permeability based on \insertCite{yang2007prediction;textual}{httk}.
#' These functions calculate the fraction absorbed (calc_fabs.oral -- 
#' \insertCite{darwich2010interplay;textual}{httk}), the fraction
#' surviving first pass gut metabolism (calc_fgut.oral), and the overall systemic
#' oral bioavailability
#' (calc_fbio.oral). Note that the first pass hepatic clearance is calculated within the
#' parameterization and other functions. using \code{\link{calc_hep_bioavailability}} 

#' We assume that systemic oral bioavailability (\eqn{F_{bio}})
#' consists of three components: 
#' (1) the fraction of chemical absorbed from intestinal lumen into enterocytes 
#' (\eqn{F_{abs}}), 
#' (2) the fraction surviving intestinal metabolism 
#' (\eqn{F_{gut}}), and 
#' (3) the fraction surviving first-pass hepatic metabolism 
#' (\eqn{F_{hep}}). This function returns (\eqn{F_{abs}*F_{gut}}).
#' 
#' We model systemic oral bioavailability as 
#' \eqn{F_{bio}=F_{abs}*F_{gut}*F_{hep}}.
#' \eqn{F_{hep}} is estimated from in vitro TK data using 
#' \code{\link{calc_hep_bioavailability}}.
#' If \eqn{F_{bio}} has been measured in vivo and is found in
#' table \code{\link{chem.physical_and_invitro.data}} then we set 
#' \eqn{F_{abs}*F_{gut}} to the measured value divided by \eqn{F_{hep}}.
#' Otherwise, if Caco2 membrane permeability data or predictions
#' are available \eqn{F_{abs}} is estimated using \code{\link{calc_fgut.oral}}.
#' Intrinsic hepatic metabolism is used to very roughly estimate (\eqn{F_{gut}})
#' using \code{\link{calc_fgut.oral}}.
#' If argument keepit100 is used then there is complete absorption from the gut
#' (that is, \eqn{F_{abs}=F_{gut}=1}). 
#' 
#' @param parameters (List) A list of the parameters (Caco2.Pab, Funbound.Plasma, Rblood2plasma,
#' Clint, BW, Qsmallintestine, Fabs, Fgut) used in the calculation, either supplied by user
#' or calculated in \code{\link{parameterize_steadystate}}.
#' 
#' @param chem.cas (Character) Chemical CAS number. (Defaults to `NULL`.)
#' (Note: Either the chemical name, CAS number, or EPA's DSSTox Structure ID
#' must be specified).
#' 
#' @param chem.name (Character) Chemical name. (Defaults to `NULL`.)
#' (Note: Either the chemical name, CAS number, or EPA's DSSTox Structure ID
#' must be specified).
#' 
#' @param dtxsid (Character) EPA's DSSTox Structure ID
#' (\url{https://comptox.epa.gov/dashboard}). (Defaults to `NULL`.)
#' (Note: Either the chemical name, CAS number, or EPA's DSSTox Structure ID
#' must be specified).
#' 
#' @param species (Character) Species desired (either "Rat", "Rabbit", "Dog",
#' "Mouse", or default "Human").
#' 
#' @param default.to.human (Logical) Substitutes missing rat values with
#' human values if TRUE. (Not applicable for `calc_fabs.oral`.)
#' (Defaults to `FALSE`.)
#' 
#' @param suppress.messages (Logical) Whether or not the output message is
#' suppressed. (Defaults to `FALSE`.)
#'  
#' @param Caco2.Pab.default (Numeric) Caco2 apical to basolateral data.
#' (Defaults to  1.6.) (Not applicable for `calc_fbio.oral`.)
#' 
# Caco2 derived values if available.
#'
#' @return \item{fbio.oral}{Oral bioavailability, the fraction of oral dose 
#' reaching systemic distribution in the body.}
#' \item{fabs.oral}{Fraction of dose absorbed, i.e. the fraction of the dose
#' that enters the gutlumen.} \item{fgut.oral}{Fraction of chemical surviving
#' first pass metabolism in the gut.} \item{fhep.oral}{Fraction of chemical
#' surviving first pass hepatic clearance.}
#' 
#' @author Gregory Honda
#' @keywords Parameter
#'
#' @references 
#' \insertRef{darwich2010interplay}{httk}
#' \insertRef{yang2007prediction}{httk}
#' \insertRef{HondaUnpublishedCaco2}{httk}
#' 
#' @export calc_fbio.oral
#' @export calc_fabs.oral
#' @export calc_fgut.oral
#'
calc_fbio.oral <- function(parameters = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = FALSE,
  suppress.messages = FALSE
  )
{
  # Handle fabs first:    
  fabs.oral <- calc_fabs.oral(parameters = parameters,
                              chem.cas=chem.cas,
                              chem.name=chem.name,
                              dtxsid=dtxsid,
                              species=species,
                              suppress.messages=suppress.messages) 

  # Now handle Fgut:
  fgut.oral <- calc_fgut.oral(parameters = parameters,
                            chem.cas=chem.cas,
                            chem.name=chem.name,
                            dtxsid=dtxsid,
                            species=species,
                            suppress.messages=suppress.messages)

  # Determine Fhep.oral
  if (is.null(parameters))
  {
    # Note that the following call to parameterize_steadystate calls this
    # function. However, because parameterize_steadystate creates and passes
    # a list of parameters we will never reach this bit of this function
    # recursively:
    parameters <- parameterize_steadystate(
                   chem.cas=chem.cas,
                   chem.name=chem.name,
                   dtxsid=dtxsid,
                   species=species,
                   suppress.messages=suppress.messages)
  }
  
  fhep.oral <- parameters[["hepatic.bioavailability"]]
  
  # Determine Fbio.oral
  fbio.oral <- fabs.oral*fhep.oral*fgut.oral 
  
  # create output list:
  out <-list("fbio.oral" = fbio.oral,
             "fabs.oral" = fabs.oral,
             "fgut.oral" = fgut.oral,
             "fhep.oral" = fhep.oral
    )

  # Set precision:
  out <- lapply(out, set_httk_precision)
  
  return(out)

}

#' @describeIn calc_fbio.oral Calculate the fraction absorbed in the gut (Darwich et al., 2010)
calc_fabs.oral <- function(parameters = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  suppress.messages = FALSE,
  Caco2.Pab.default = 1.6
  )
{
  # Required parameters
  req.param <- c("Caco2.Pab")
  
  # Header initialization  
  if(is.null(parameters) | !all(req.param %in% names(parameters)))
  {
    # Need a list called parameters:
    if (is.null(parameters)) parameters <- list()
    
  # We need to describe the chemical to be simulated one way or another:
      if (is.null(chem.cas) & 
          is.null(chem.name) & 
          is.null(dtxsid)) 
        stop('chem.name, chem.cas, or dtxsid must be specified.')
    
    # Look up the chemical name/CAS, depending on what was provide:
        out <- get_chem_id(
                chem.cas=chem.cas,
                chem.name=chem.name,
                dtxsid=dtxsid)
        chem.cas <- out$chem.cas
        chem.name <- out$chem.name                                
        dtxsid <- out$dtxsid
      
      # Retrieve the chemical-specific Caco-2 value:
      parameters <-  c(parameters, get_caco2(chem.cas=chem.cas,
                              chem.name=chem.name,
                              dtxsid=dtxsid,
                              Caco2.Pab.default = Caco2.Pab.default,
                              suppress.messages = suppress.messages))
  }
  
  # Determine Fabs.oral based on Caco2 data
    # Yang 2007 equation 10 for Caco2 pH7.4
    peff <- 10^(0.4926 * log10(parameters$Caco2.Pab) - 0.1454) 
    # Fagerholm 1996 -- Yang et al. (2007) equation 14
    if (tolower(species) %in% c("rat"))
      peff <- max((peff - 0.03)/3.6, 0)
    # Darwich et al. (2010) Equation 3
    fabs.oral <- 1 - (1 + 0.54 * peff)^-7

  # Require that the fraction is less than 1:
  fabs.oral <- ifelse(fabs.oral > 1, 1.0, fabs.oral)
    
  return(set_httk_precision(as.numeric(fabs.oral)))
}

#' @describeIn calc_fbio.oral Calculate the fraction of chemical surviving first pass metabolism in the gut
# Is this the Yang et al. (2007) QGut Model?
calc_fgut.oral <- function(parameters = NULL,
  chem.cas = NULL,
  chem.name = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = FALSE,
  suppress.messages = FALSE,
  Caco2.Pab.default = 1.6
  )
{
  if (!is.null(parameters))
  {
    # Required parameters
    req.param <- c("BW", "Clint", "Funbound.plasma", "Rblood2plasma")
    
    if (!all(req.param %in% names(parameters)))
    {
      missing <- req.param[!(req.param %in% names(parameters))]
      stop(paste("Missing parameter",missing,"in function calc_fgut.oral."))
    }
  } else {  
      # Need a list called parameters:
      if (is.null(parameters)) parameters <- list()
      
# We need to describe the chemical to be simulated one way or another:
      if (is.null(chem.cas) & 
          is.null(chem.name) & 
          is.null(dtxsid)) 
        stop('chem.name, chem.cas, or dtxsid must be specified.')
    
    # Look up the chemical name/CAS, depending on what was provide:
        out <- get_chem_id(
                chem.cas=chem.cas,
                chem.name=chem.name,
                dtxsid=dtxsid)
        chem.cas <- out$chem.cas
        chem.name <- out$chem.name                                
        dtxsid <- out$dtxsid

        # If we don't have parameters we can call parameterize_steadystate using the
        # chemical identifiers. Then that function will build up a list of parameters
        # so that when it calls this function a second time (recursively) we WILL have
        # a list of parameters defined. The key is to make sure that the call to this
        # function in parameterize_steadystate occurs after all parameters needed by
        # this function are defined (or we get stuck in a recursive loop).
        parameters <- do.call(parameterize_steadystate,
                              args=purrr::compact(c(list(
                                chem.cas=chem.cas,
                                chem.name=chem.name,
                                dtxsid=dtxsid,
                                suppress.messages=suppress.messages))))
  }
  
  # Retrieve the chemical-specific Caco-2 value:
    if (!("Caco2.Pab" %in% names(parameters)))
      parameters <-  c(parameters, get_caco2(chem.cas=chem.cas,
                                   chem.name=chem.name,
                                   dtxsid=dtxsid,
                                   Caco2.Pab.default = Caco2.Pab.default,
                                   suppress.messages = suppress.messages))

    # Scale up from in vitro Clint to a whole liver clearance:
    clu_hep <- calc_hep_clearance(parameters=parameters,
                             hepatic.model='unscaled',
                             suppress.messages=TRUE) #L/h/kg body weight
    clu_hep <- clu_hep*parameters$BW # L/h 
    clu_gut <- clu_hep/100 # approximate ratio of cyp abundances
    
    # Yang et al. (2007) equation 10 (our data is at pH 7.4):
    peff <- (10^(0.4926 * parameters$Caco2.Pab - 0.1454)) # peff dimensional 10-4 cm/s
    
    if(tolower(species) == "rat")
    {
      # Fagerholm 1996 -- Yang et al. (2007) equation 14
      peff <- max((peff - 0.03)/3.6, 0)
      Asmallintestine <- 71/(100^2) # m2 Ref?
# IF not a rat, we assume human:
    } else {
      if (tolower(species) != "human") 
        if (!suppress.messages)
          warning("Human intestinal permeability and microvilli blood flow used to calculate fraction absorbed by gut")
      
      # m2 area intestine -- Yang et al. (2007)
      Asmallintestine <- 0.66/70*parameters$BW
    }
    
    # Define a rate of intestinal transport to compete with absorption for poorly
    # absorbed chemicals:
    Qintesttransport <- 0.5
    
    # Calculate Qvilli based on Qsmallintestine
    # Tateishi 1997 -- Human Qsmallintestine = 38 ml/min/100 g
    # Weight small intestine -- 1.5875 kg -- 15.875 100 g
    # Fraction of blood flow to gut going to small intestine:
    # For 70 kg human Qtotal.liver = 86.96 L/h (parameterize_pbtk)
    # and Qgut = 68.99 L/h (parameterize_pbtk)
    Qsmallintestinehuman <- 38.7 /1000*60*15.8757 # L/h
    Qsmallintestinef <- Qsmallintestinehuman / 68.99 # Works out to roughly 53%
    if(!is.null(parameters$Qtotal.liverc)){
      this.Qsmallintestinehuman <- Qsmallintestinef *
        68.99/86.96*parameters$Qtotal.liverc*parameters$BW^(3/4)
      Qvilli <- 18/Qsmallintestinehuman*this.Qsmallintestinehuman
    } else if(!is.null(parameters$Qgutf)){
      this.Qsmallintestinehuman <- Qsmallintestinef *
        parameters$Qgutf*parameters$Qcardiacc*parameters$BW^(3/4)
      Qvilli <- 18/Qsmallintestinehuman*this.Qsmallintestinehuman
    } else {
      warning("Because model used does not provide Qtotal.liver or Qgutf, the Yang et al. (2007) value of 18 L/h was used to calculate fraction absorbed by gut")
      Qvilli <- 18 # L/h blood flow to microvilli of intestinal lumen
    }    
    # permeability clearance (CLperm) Yang et al. (2007) equation 8:
    CLperm <- peff * Asmallintestine * 1000/10^4/100*3600 # 10^-4 cm / s * m2 * 1000 L / m^3 / 10^4 * 1 m / 100 cm * 3600 s / h = L / h
    # Qgut "in terms of fundamental parameters" -- Yang et al. (2007) equation 6:
    Qgut <- Qvilli * CLperm / (Qvilli + CLperm)
    # Qgut Model -- Yang et al. (2007) equation 5:
 #   fgut.oral <- Qgut / (Qgut + parameters$Funbound.plasma*clu_gut/parameters$Rblood2plasma) 
 # Metabolism of chemical in enterocyte, not blood:
    fgut.oral <- Qgut / (Qgut + parameters$Funbound.plasma*clu_gut) 
    # Add competitive process (clearance of the gut lumen):
    fgut.oral <- fgut.oral*(Qgut/(Qintesttransport + Qgut))

  # Set reasonable precision:
  fgut.oral <- set_httk_precision(as.numeric(fgut.oral))

  # Require that the fraction is less than 1:
  fgut.oral <- ifelse(fgut.oral > 1, 1.0, fgut.oral)
   
  return(set_httk_precision(as.numeric(fgut.oral)))
}
