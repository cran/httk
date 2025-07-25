#' Parameters for a generic physiologically-based toxicokinetic model
#' 
#' Generate a chemical- and species-specific set of PBPK model parameters.
#' Parameters include 
#' tissue:plasma partition coefficients, organ volumes, and flows 
#' for the tissue lumping scheme specified by argument tissuelist.
#' Tissure:(fraction unbound in) plasma partitition coefficients are predicted
#' via Schmitt (2008)'s method as modified by Pearce et al. (2017) using
#' \code{\link{predict_partitioning_schmitt}}. Organ volumes and flows are
#' retrieved from table \code{\link{physiology.data}}.
#' Tissues must be described in table \code{\link{tissue.data}}. 
#'
#' By default, this function initializes the parameters needed in the functions 
#' \code{\link{solve_pbtk}}, \code{\link{calc_css}}, and others using the httk 
#' default generic PBTK model (for oral and intravenous dosing only).
#'
#' The default PBTK model includes an explicit first pass of the chemical through
#' the liver before it becomes available to systemic blood. We model systemic oral bioavailability as 
#' \ifelse{html}{\out{F<sub>bio</sub>=F<sub>abs</sub>*F<sub>gut</sub>*F<sub>hep</sub>}}{\eqn{F_{bio}=F_{abs}*F_{gut}*F_{hep}}}.
#' Only if \ifelse{html}{\out{F<sub>bio</sub>}}{\eqn{F_{bio}}}
#' has been measured in vivo and is found in
#' table \code{\link{chem.physical_and_invitro.data}} then we set 
#' \ifelse{html}{\out{F<sub>abs</sub>*F<sub>gut</sub>}}{\eqn{F_{abs}*F_{gut}}} 
#' to the measured value divided by 
#' \ifelse{html}{\out{F<sub>hep</sub>}}{\eqn{F_{hep}}} 
#' where \ifelse{html}{\out{F<sub>hep</sub>}}{\eqn{F_{hep}}}
#' is estimated from in vitro TK data using 
#' \code{\link{calc_hep_bioavailability}}.
#' If Caco2 membrane permeability data or predictions
#' are available \ifelse{html}{\out{F<sub>abs</sub>}}{\eqn{F_{abs}}} is estimated
#' using \code{\link{calc_fabs.oral}}.
#' Intrinsic hepatic metabolism is used to very roughly estimate
#' \ifelse{html}{\out{F<sub>gut</sub>}}{\eqn{F_{gut}}}
#' using \code{\link{calc_fgut.oral}}.
#' 
#' Because this model does not simulate exhalation, inhalation, and other 
#' processes relevant to volatile chemicals, this model is by default 
#' restricted to chemicals with a logHenry's Law Constant less than that of 
#' Acetone, a known volatile chemical. That is, chemicals with logHLC > -4.5 
#' (Log10 atm-m3/mole) are excluded. Volatility is not purely determined by the 
#' Henry's Law Constant, therefore this chemical exclusion may be turned off 
#' with the argument "physchem.exclude = FALSE". Similarly, per- and 
#' polyfluoroalkyl substances (PFAS) are excluded by default because the 
#' transporters that often drive PFAS toxicokinetics are not included in this 
#' model. However, PFAS chemicals can be included with the argument 
#' "class.exclude = FALSE".
#' 
#' @param chem.cas Chemical Abstract Services Registry Number (CAS-RN) -- the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param chem.name Chemical name (spaces and capitalization ignored) --  the 
#' chemical must be identified by either CAS, name, or DTXISD
#' 
#' @param dtxsid EPA's 'DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
#' -- the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by 
#' the relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param tissuelist Specifies compartment names and tissues groupings.
#' Remaining tissues in tissue.data are lumped in the rest of the body.
#' However, \code{\link{solve_pbtk}} only works with the default parameters.
#' 
#' @param force.human.clint.fup Forces use of human values for hepatic
#' intrinsic clearance and fraction of unbound plasma if true.
#' 
#' @param clint.pvalue.threshold Hepatic clearance for chemicals where the in
#' vitro clearance assay result has a p-values greater than the threshold are
#' set to zero.
#' 
#' @param adjusted.Funbound.plasma Uses Pearce et al. (2017) lipid binding adjustment
#' for Funbound.plasma (which impacts partition coefficients) when set to TRUE (Default).
#' 
#' @param adjusted.Clint Uses Kilford et al. (2008) hepatocyte incubation
#' binding adjustment for Clint when set to TRUE (Default).
#' 
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param restrictive.clearance In calculating hepatic.bioavailability, protein
#' binding is not taken into account (set to 1) in liver clearance if FALSE.
#' 
#' @param minimum.Funbound.plasma \eqn{f_{up}} is not allowed to drop below
#' this value (default is 0.0001).      
#' 
#' @param Caco2.options A list of options to use when working with Caco2 apical 
#' to basolateral data \code{Caco2.Pab}, default is Caco2.options = 
#' list(Caco2.Pab.default = 1.6, Caco2.Fabs = TRUE, Caco2.Fgut = TRUE, 
#' overwrite.invivo = FALSE, keepit100 = FALSE). Caco2.Pab.default sets the 
#' default value for Caco2.Pab if Caco2.Pab is unavailable. Caco2.Fabs = TRUE 
#' uses Caco2.Pab to calculate fabs.oral, otherwise fabs.oral = \code{Fabs}. 
#' Caco2.Fgut = TRUE uses Caco2.Pab to calculate 
#' fgut.oral, otherwise fgut.oral = \code{Fgut}. overwrite.invivo = TRUE 
#' overwrites Fabs and Fgut in vivo values from literature with 
#' Caco2 derived values if available. keepit100 = TRUE overwrites Fabs and Fgut 
#' with 1 (i.e. 100 percent) regardless of other settings.
#' See \code{\link{get_fbio}} for further details.
#' 
#' @param minimum.Funbound.plasma \eqn{f_{up}} is not allowed to drop below
#' this value (default is 0.0001).                                
#'                                                                                             
#' @param million.cells.per.gliver Hepatocellularity (defaults to 110 10^6 cells/g-liver, from Carlile et al. (1997))
#'
#' @param liver.density Liver density (defaults to 1.05 g/mL from International Commission on Radiological Protection (1975))
#'
#' @param kgutabs Oral absorption rate from gut (determined from Peff)
#' 
#' @param ... Additional arguments, not currently used.
#' 
#' @return \item{BW}{Body Weight, kg.} 
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Fabsgut}{Fraction of the oral dose absorbed, i.e. the fraction of
#' the dose that enters the gutlumen.} 
#' \item{Funbound.plasma}{Fraction of plasma that is not bound.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)}
#' \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound
#' concentration in plasma.}
#' \item{kgutabs}{Rate that chemical enters the gut from gutlumen, 1/h.}
#' \item{Kkidney2pu}{Ratio of concentration of chemical in kidney tissue to
#' unbound concentration in plasma.}
#' \item{Kliver2pu}{Ratio of concentration of chemical in liver tissue to
#' unbound concentration in plasma.} 
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue
#' to unbound concentration in plasma.} 
#' \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to
#' unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.}
#' \item{MW}{Molecular Weight, g/mol.}
#' \item{Qcardiacc}{Cardiac Output, L/h/kg BW^3/4.}
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^3/4, volume of fluid
#' filtered from kidney and excreted.}
#' \item{Qgutf}{Fraction of cardiac output flowing to the gut.}
#' \item{Qkidneyf}{Fraction of cardiac output flowing to the kidneys.}
#' \item{Qliverf}{Fraction of cardiac output flowing to the liver.}
#' \item{Rblood2plasma}{The ratio of the concentration of the chemical in the
#' blood to the concentration in the plasma from available_rblood2plasma.}
#' \item{Vartc}{Volume of the arteries per kg body weight, L/kg BW.}
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.}
#' \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg BW.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#' \item{Vvenc}{Volume of the veins per kg body weight, L/kg BW.} 
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @references 
#'
#' \insertRef{pearce2017httk}{httk}
#'
#' \insertRef{schmitt2008general}{httk}
#'
#' \insertRef{pearce2017evaluation}{httk}
#'
#' \insertRef{kilford2008hepatocellular}{httk}
#' 
#' International Commission on Radiological Protection. Report of the task 
#' group on reference man. Vol. 23. Pergamon, Oxford. 1975.
#'
#' @keywords Parameter pbtk
#'
#' @seealso \code{\link{solve_pbtk}}
#'
#' @seealso \code{\link{calc_analytic_css_pbtk}}
#'
#' @seealso \code{\link{predict_partitioning_schmitt}}
#'
#' @seealso \code{\link{apply_clint_adjustment}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#'
#' @examples
#' 
#'  parameters1 <- parameterize_pbtk(chem.cas='80-05-7')
#' \donttest{
#' 
#'  parameters2 <- parameterize_pbtk(chem.name='Bisphenol-A',species='Rat')
#' 
#'  # Change the tissue lumping (note, these model parameters will not work with our current solver):
#'  compartments <- list(liver=c("liver"),fast=c("heart","brain","muscle","kidney"),
#'                       lung=c("lung"),gut=c("gut"),slow=c("bone"))
#'  parameterize_pbtk(chem.name="Bisphenol a",species="Rat",default.to.human=TRUE,
#'                    tissuelist=compartments)
#'
#' # The following will not work because Diquat dibromide monohydrate's 
#' # Henry's Law Constant (-3.912) is higher than that of Acetone (~-4.5):
#' try(parameters3 <- parameterize_pbtk(chem.cas = "6385-62-2"))
#' # However, we can turn off checking for phys-chem properties, since we know
#' # that  Diquat dibromide monohydrate is not too volatile:
#' parameters3 <- parameterize_pbtk(chem.cas = "6385-62-2",
#'                                  physchem.exclude = FALSE) 
#' }
#' 
#' @export parameterize_pbtk
parameterize_pbtk <- function(
                       chem.cas=NULL,
                       chem.name=NULL,
                       dtxsid=NULL,
                       species="Human",
                       default.to.human=FALSE,
                       tissuelist=list(
                         liver=c("liver"),
                         kidney=c("kidney"),
                         lung=c("lung"),
                         gut=c("gut")),
                       force.human.clint.fup = FALSE,
                       clint.pvalue.threshold=0.05,
                       adjusted.Funbound.plasma=TRUE,
                       adjusted.Clint=TRUE,
                       regression=TRUE,
                       suppress.messages=FALSE,
                       restrictive.clearance=TRUE,
                       minimum.Funbound.plasma=0.0001,
                       class.exclude=TRUE,
                       physchem.exclude=TRUE,
                       million.cells.per.gliver= 110, # 10^6 cells/g-liver Carlile et al. (1997)
                       liver.density= 1.05, # g/mL International Commission on Radiological Protection (1975)
                       kgutabs = NA, # 1/h, Wambaugh et al. (2018)
                       Caco2.options = NULL,
                       ...
                       )
{
  #Give a binding to the physiology.data
  physiology.data <- physiology.data
  
  #We need to describe the chemical to be simulated one way or another:
  if (is.null(chem.cas) & 
      is.null(chem.name) & 
      is.null(dtxsid))
    stop('chem.name, chem.cas, or dtxsid must be specified.')
  
  # Look up the chemical name/CAS, depending on what was provided:
  out <- get_chem_id(chem.cas=chem.cas,
                     chem.name=chem.name,
                     dtxsid=dtxsid)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
  dtxsid <- out$dtxsid

# Make sure we have all the parameters we need:
  check_model(chem.cas=chem.cas, 
            chem.name=chem.name,
            dtxsid=dtxsid,
            model="pbtk",
            species=species,
            class.exclude=class.exclude,
            physchem.exclude=physchem.exclude,
            default.to.human=default.to.human|force.human.clint.fup
            )
  
# Get the intrinsic hepatic clearance:  
  Clint.list <- get_clint(
      dtxsid=dtxsid,
      chem.name=chem.name,
      chem.cas=chem.cas,
      species=species,
      default.to.human=default.to.human,
      force.human.clint=force.human.clint.fup,
      clint.pvalue.threshold=clint.pvalue.threshold,
      suppress.messages=suppress.messages) 
  Clint.point <- Clint.list$Clint.point
  Clint.dist <- Clint.list$Clint.dist

# Get phys-chemical properties:
  MW <- get_physchem_param("MW",chem.cas=chem.cas) #g/mol
  # acid dissociation constants
  pKa_Donor <- suppressWarnings(get_physchem_param(
    "pKa_Donor",
    chem.cas=chem.cas)) 
  # basic association cosntants
  pKa_Accept <- suppressWarnings(get_physchem_param(
    "pKa_Accept",
    chem.cas=chem.cas)) 
  # Octanol:water partition coefficient
  Pow <- 10^get_physchem_param(
    "logP",
    chem.cas=chem.cas) 
    
# Calculate unbound fraction of chemical in the hepatocyte intrinsic 
# clearance assay (Kilford et al., 2008)
  Fu_hep <- calc_hep_fu(parameters=list(
    Pow=Pow,
    pKa_Donor=pKa_Donor,
    pKa_Accept=pKa_Accept)) # fraction 

# Correct for unbound fraction of chemical in the hepatocyte intrinsic 
# clearance assay (Kilford et al., 2008)
  if (adjusted.Clint) Clint.point <- apply_clint_adjustment(
                               Clint.point,
                               Fu_hep=Fu_hep,
                               suppress.messages=suppress.messages)
                                   
# Predict the PCs for all tissues in the tissue.data table:
  schmitt.params <- parameterize_schmitt(
                      chem.cas=chem.cas,
                      species=species,
                      default.to.human=default.to.human,
                      class.exclude=class.exclude,
                      force.human.fup=force.human.clint.fup,
                      suppress.messages=suppress.messages,
                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                      minimum.Funbound.plasma=minimum.Funbound.plasma)
       
  fup <- schmitt.params$Funbound.plasma
  
  PCs <- predict_partitioning_schmitt(
    parameters=schmitt.params,
    species=species,
    default.to.human=default.to.human,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    regression=regression,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    model="pbtk",
    suppress.messages=suppress.messages)

  # Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- lump_tissues(
    PCs,
    tissuelist=tissuelist,
    species=species,
    model="pbtk",
    suppress.messages=suppress.messages)

# Check the species argument for capitalization problems and whether or not 
# it is in the table:  

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
  
  outlist <- list()
  # Begin flows:
  #mL/min/kgBW^(3/4) converted to L/h/kgBW^(3/4):
  QGFRc <- this.phys.data["GFR"]/1000*60 # L/h/kgBW^(3/4) 
  Qcardiacc = this.phys.data["Cardiac Output"]/1000*60 # L/h/kgBW^(3/4)
  flows <- unlist(lumped_params[substr(names(lumped_params),1,1) == 'Q'])
  
  outlist <- c(outlist,c(
    Qcardiacc = as.numeric(Qcardiacc),
    flows[!names(flows) %in% c('Qlungf','Qtotal.liverf')],
    Qliverf= flows[['Qtotal.liverf']] - flows[['Qgutf']],
    Qgfrc = as.numeric(QGFRc))) 
  # end flows  
  
  # Begin volumes
  # units should be L/kgBW  
  Vartc = this.phys.data["Plasma Volume"]/
    (1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW
  Vvenc = this.phys.data["Plasma Volume"]/
    (1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW
  
  outlist <- c(outlist,
               Vartc = as.numeric(Vartc),
               Vvenc = as.numeric(Vvenc),
               lumped_params[substr(names(lumped_params),1,1) == 'V'],
               lumped_params[substr(names(lumped_params),1,1) == 'K'])
  

  # Create the list of parameters:
  BW <- this.phys.data["Average BW"]
  hematocrit = this.phys.data["Hematocrit"]
  
  outlist <- c(outlist,list(BW = as.numeric(BW),
                            Funbound.plasma = fup, # unitless fraction
                            Funbound.plasma.dist = schmitt.params$Funbound.plasma.dist,
                            hematocrit = as.numeric(hematocrit), # unitless ratio
                            MW = MW, #g/mol
                            Pow = Pow,
                            pKa_Donor=pKa_Donor,
                            pKa_Accept=pKa_Accept,
                            MA=schmitt.params[["MA"]]))
  
  # Fraction unbound lipid correction:
  if (adjusted.Funbound.plasma) 
  {
    outlist["Funbound.plasma.adjustment"] <- 
      schmitt.params$Funbound.plasma.adjustment
  } else outlist["Funbound.plasma.adjustment"] <- NA
   
# Blood to plasma ratio:
  outlist <- c(outlist,
    Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,
      species=species,
      class.exclude=class.exclude,
      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
      suppress.messages=suppress.messages))

# Liver metabolism properties:
  outlist <- c(
    outlist,
    list(Clint=Clint.point,
         Clint.dist = Clint.dist,
         Fhep.assay.correction=Fu_hep,  # fraction 
         Clmetabolismc= as.numeric(calc_hep_clearance(
           hepatic.model="unscaled",
           parameters=list(
             Clint=Clint.point, #uL/min/10^6 cells
             Funbound.plasma=fup, # unitless fraction
             Rblood2plasma = outlist$Rblood2plasma, 
             million.cells.per.gliver = million.cells.per.gliver, # 10^6 cells/g-liver
             liver.density = liver.density, # g/mL
             Dn=0.17,
             BW=BW,
             Vliverc=lumped_params$Vliverc, #L/kg
             Qtotal.liverc=
               (lumped_params$Qtotal.liverf*as.numeric(Qcardiacc))), # L/h/kgBW^(3/4)
           suppress.messages=TRUE,
           species = species,
           restrictive.clearance=restrictive.clearance)), #L/h/kg BW
      million.cells.per.gliver=110, # 10^6 cells/g-liver
      liver.density=1.05)) # g/mL
   
# Oral bioavailability parameters:
  outlist <- c(
    outlist, do.call(get_fbio, args=purrr::compact(c(
    list(
      parameters=outlist,
      dtxsid=dtxsid,
      chem.cas=chem.cas,
      chem.name=chem.name,
      species=species,
      suppress.messages=suppress.messages
      ),
    Caco2.options))
    ))

  # If provided by argument:
  if (!is.na(kgutabs)) outlist[["kgutabs"]] <- kgutabs

  # Only include parameters specified in modelinfo:
  outlist <- outlist[model.list[["pbtk"]]$param.names]
  
# Set precision:
  return(lapply(outlist, set_httk_precision))
}
