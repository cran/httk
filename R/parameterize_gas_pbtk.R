#' Parameters for a generic gas inhalation physiologically-based toxicokinetic model
#' 
#' This function initializes the parameters needed for the model 'gas_pbtk', for
#' example \code{\link{solve_gas_pbtk}}. Chemical- and species-specific model 
#' parameters are generated. These include tissue:plasma partition coefficients 
#' via Schmitt (2008)'s method as modified by Pearce et al. (2017). Organ volumes 
#' and flows are retrieved from table \code{\link{physiology.data}}). This model
#' was first described by Linakis et al. (2020).
#' 
#' Per- and 
#' polyfluoroalkyl substances (PFAS) are excluded by default because the 
#' transporters that often drive PFAS toxicokinetics are not included in this 
#' model. However, PFAS chemicals can be included with the argument 
#' "class.exclude = FALSE".
#' 
#' @param chem.name Either the chemical name or the CAS number must be
#' specified. 
#' 
#' @param chem.cas Either the chemical name or the CAS number must be
#' specified. 
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})   
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param default.to.human Substitutes missing animal values with human values
#' if true (hepatic intrinsic clearance or fraction of unbound plasma).
#' 
#' @param tissuelist Specifies compartment names and tissues groupings.
#' Remaining tissues in tissue.data are lumped in the rest of the body.
#' However, solve_pbtk only works with the default parameters.
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
#' @param suppress.messages Whether or not the output messages are suppressed.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
#' 
#' @param vmax Michaelis-Menten vmax value in reactions/min
#' 
#' @param km Michaelis-Menten concentration of half-maximal reaction velocity
#' in desired output concentration units. 
#' 
#' @param exercise Logical indicator of whether to simulate an exercise-induced
#' heightened respiration rate
#' 
#' @param fR Respiratory frequency (breaths/minute), used especially to adjust
#' breathing rate in the case of exercise. This parameter, along with VT and VD
#' (below) gives another option for calculating Qalv (Alveolar ventilation) 
#' in case pulmonary ventilation rate is not known 
#' 
#' @param class.exclude Exclude chemical classes identified as outside of 
#' domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param physchem.exclude Exclude chemicals on the basis of physico-chemical
#' properties (currently only Henry's law constant) as specified by 
#' the relevant modelinfo_[MODEL] file (default TRUE).
#' 
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE. (Default is FALSE.)
#' 
#' @param VT Tidal volume (L), to be modulated especially as part of simulating
#' the state of exercise
#' 
#' @param VD Anatomical dead space (L), to be modulated especially as part of
#' simulating the state of exercise
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
#' @param ... Other parameters
#' 
#' @return \item{BW}{Body Weight, kg.} 
#' \item{Clint}{Hepatic intrinsic clearance, uL/min/10^6 cells}
#' \item{Clint.dist}{Distribution of hepatic intrinsic clearance values
#' (median, lower 95th, upper 95th, p value)} 
#' \item{Clmetabolismc}{Hepatic Clearance, L/h/kg BW.} 
#' \item{Fabsgut}{Fraction of the oral dose absorbed, i.e. the fraction of the
#' dose that enters the gut lumen.}
#' \item{Fhep.assay.correction}{The fraction of chemical unbound in hepatocyte
#' assay using the method of Kilford et al. (2008)} 
#' \item{Funbound.plasma}{Fraction of chemical unbound to plasma.} 
#' \item{Funbound.plasma.adjustment}{Fraction unbound to plasma adjusted as
#' described in Pearce et al. 2017}
#' \item{Funbound.plasma.dist}{Distribution of fraction unbound to plasma
#' (median, lower 95th, upper 95th)}
#' \item{hematocrit}{Percent volume of red blood cells in the blood.}
#' \item{Kblood2air}{Ratio of concentration of chemical in blood to air}
#' \item{Kgut2pu}{Ratio of concentration of chemical in gut tissue to unbound
#' concentration in plasma.} 
#' \item{kgutabs}{Rate that chemical enters the gut from gutlumen, 1/h.} 
#' \item{Kkidney2pu}{Ratio of concentration of chemical in kidney tissue to
#' unbound concentration in plasma.} 
#' \item{Kliver2pu}{Ratio of concentration of chemical in liver tissue to
#' unbound concentration in plasma.} 
#' \item{Klung2pu}{Ratio of concentration of chemical in lung tissue
#' to unbound concentration in plasma.} 
#' \item{km}{Michaelis-Menten concentration of half-maximal activity}
#' \item{Kmuc2air}{Mucus to air partition coefficient}
#' \item{Krbc2pu}{Ratio of concentration of chemical in red blood cells to
#' unbound concentration in plasma.}
#' \item{Krest2pu}{Ratio of concentration of chemical in rest of body tissue to
#' unbound concentration in plasma.} 
#' \item{kUrtc}{Unscaled upper respiratory tract uptake parameter (L/h/kg^0.75)}
#' \item{liver.density}{Density of liver in g/mL}
#' \item{MA}{phospholipid:water distribution coefficient, membrane affinity}
#' \item{million.cells.per.gliver}{Millions cells per gram of liver tissue.} 
#' \item{MW}{Molecular Weight, g/mol.}
#' \item{pKa_Accept}{compound H association equilibrium constant(s)}
#' \item{pKa_Donor}{compound H dissociation equilibirum constant(s)}
#' \item{Pow}{octanol:water partition coefficient (not log transformed)}
#' \item{Qalvc}{Unscaled alveolar ventilation rate (L/h/kg^0.75)}
#' \item{Qcardiacc}{Cardiac Output, L/h/kg BW^3/4.} 
#' \item{Qgfrc}{Glomerular Filtration Rate, L/h/kg BW^0.75, volume of fluid
#' filtered from kidney and excreted.} 
#' \item{Qgutf}{Fraction of cardiac output flowing to the gut.}
#' \item{Qkidneyf}{Fraction of cardiac output flowing to the kidneys.}
#' \item{Qliverf}{Fraction of cardiac output flowing to the liver.}
#' \item{Qlungf}{Fraction of cardiac output flowing to lung tissue.}
#' \item{Qrestf}{Fraction of blood flow to rest of body}
#' \item{Rblood2plasma}{The ratio of the concentration of the chemical in the
#' blood to the concentration in the plasma from available_rblood2plasma.}
#' \item{Vartc}{Volume of the arteries per kg body weight, L/kg BW.}
#' \item{Vgutc}{Volume of the gut per kg body weight, L/kg BW.}
#' \item{Vkidneyc}{Volume of the kidneys per kg body weight, L/kg BW.}
#' \item{Vliverc}{Volume of the liver per kg body weight, L/kg BW.}
#' \item{Vlungc}{Volume of the lungs per kg body weight, L/kg BW.}
#' \item{vmax}{Michaelis-Menten maximum reaction velocity (1/min)}
#' \item{Vmucc}{Unscaled mucosal volume (L/kg BW^0.75}
#' \item{Vrestc}{ Volume of the rest of the body per kg body weight, L/kg BW.}
#' \item{Vvenc}{Volume of the veins per kg body weight, L/kg BW.} 
#'
#' @author Matt Linakis, Robert Pearce, John Wambaugh
#'
#' @references 
#' \insertRef{linakis2020development}{httk}
#'
#' \insertRef{schmitt2008general}{httk}
#'
#' \insertRef{pearce2017evaluation}{httk}
#'
#' \insertRef{kilford2008hepatocellular}{httk}
#'
#' @keywords Parameter
#'
#' @seealso \code{\link{solve_gas_pbtk}}
#'
#' @seealso \code{\link{apply_clint_adjustment}}
#'
#' @seealso \code{\link{predict_partitioning_schmitt}}
#'
#' @seealso \code{\link{available_rblood2plasma}}
#'
#' @seealso \code{\link{calc_kair}}
#'
#' @seealso \code{\link{tissue.data}}
#'
#' @seealso \code{\link{physiology.data}}
#'
#' @seealso \code{\link{get_clint}}
#'
#' @seealso \code{\link{get_fup}}
#'
#' @seealso \code{\link{get_physchem_param}}
#'
#' @examples
#' parameters <- parameterize_gas_pbtk(chem.cas='129-00-0')
#' 
#'\donttest{
#' parameters <- parameterize_gas_pbtk(chem.name='pyrene',species='Rat')
#' 
#' parameterize_gas_pbtk(chem.cas = '56-23-5')
#' 
#' parameters <- parameterize_gas_pbtk(chem.name='Carbon tetrachloride',species='Rat')
#' 
#' # Change the tissue lumping:
#' compartments <- list(liver=c("liver"),fast=c("heart","brain","muscle","kidney"),
#'                       lung=c("lung"),gut=c("gut"),slow=c("bone"))
#' parameterize_gas_pbtk(chem.name="Bisphenol a",species="Rat",default.to.human=TRUE,
#'                    tissuelist=compartments) 
#'}
#' 
#' @export parameterize_gas_pbtk
parameterize_gas_pbtk <- function(chem.cas=NULL,
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
                              vmax = 0,
                              km = 1,
                              exercise = FALSE,
                              fR = 12,
                              VT = 0.75,
                              VD = 0.15,
                              suppress.messages=FALSE,
                              minimum.Funbound.plasma=0.0001,
                              Caco2.options=list(),
                              class.exclude=TRUE,
                              physchem.exclude = TRUE,
                              restrictive.clearance = FALSE,
                              ...)
{
  physiology.data <- physiology.data

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
   
# Make sure we have all the parameters we need:
  check_model(chem.cas=chem.cas, 
            chem.name=chem.name,
            dtxsid=dtxsid,
            model="gas_pbtk",
            species=species,
            class.exclude=class.exclude,
            physchem.exclude=physchem.exclude,
            default.to.human=default.to.human|force.human.clint.fup)
            
  if (is(tissuelist,'list')==FALSE) stop("tissuelist must be a list of vectors.") 

  # Clint has units of uL/min/10^6 cells:
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
  schmitt.params <- parameterize_schmitt(chem.cas=chem.cas,
                                         species=species,
                                         default.to.human=default.to.human,
                                         class.exclude=class.exclude,
                                         force.human.fup=force.human.clint.fup,
                                         suppress.messages=TRUE,
                                         minimum.Funbound.plasma=minimum.Funbound.plasma)
  PCs <- predict_partitioning_schmitt(parameters=schmitt.params,
                                      species=species,
                                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                                      regression=regression,
                                      suppress.messages=suppress.messages,
                                      minimum.Funbound.plasma=minimum.Funbound.plasma)
# Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- lump_tissues(PCs,tissuelist=tissuelist,species=species)
  
# Check to see if we should use the in vitro fup assay correction:  
  if (adjusted.Funbound.plasma)
  {
    fup <- schmitt.params$Funbound.plasma
    if (!suppress.messages) warning(
'Funbound.plasma adjusted for in vitro partitioning (Pearce, 2017). Set adjusted.Funbound.plasma to FALSE to use original value.')
  } else fup <- schmitt.params$unadjusted.Funbound.plasma

# Restrict the value of fup:
  if (fup < minimum.Funbound.plasma) fup <- minimum.Funbound.plasma

  Fabsgut <- try(get_invitroPK_param("Fabsgut",species,chem.cas=chem.cas),silent=TRUE)
  if (is(Fabsgut,"try-error")) Fabsgut <- 1
  
 # Check the species argument for capitilization problems and whether or not it is in the table:  
  if (!(species %in% colnames(physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(physiology.data)))
    {
      phys.species <- colnames(physiology.data)[toupper(colnames(physiology.data))==toupper(species)]
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else phys.species <- species

# Load the physiological parameters for this species
  this.phys.data <- physiology.data[,phys.species]
  names(this.phys.data) <- physiology.data[,1]
  
  outlist <- list()
   # Begin flows:
  #mL/min/kgBW converted to L/h/kgBW:
  QGFRc <- this.phys.data["GFR"]/1000*60 
  Qcardiacc = this.phys.data["Cardiac Output"]/1000*60 
  flows <- unlist(lumped_params[substr(names(lumped_params),1,1) == 'Q'])

  outlist <- c(outlist,c(
    Qcardiacc = as.numeric(Qcardiacc),
    flows[!names(flows) %in% c('Qtotal.liverf')], #MWL removed 'Qlungf', 9/19/19
    Qliverf= flows[['Qtotal.liverf']] - flows[['Qgutf']],
    Qgfrc = as.numeric(QGFRc))) 
  # end flows  
  
  # Begin volumes
  # units should be L/kgBW  
  Vartc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW
  Vvenc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW

  outlist <- c(outlist,
    Vartc = as.numeric(Vartc),
    Vvenc = as.numeric(Vvenc),
    lumped_params[substr(names(lumped_params),1,1) == 'V'],
    lumped_params[substr(names(lumped_params),1,1) == 'K'])
  
  
# Create the list of parameters:
  BW <- this.phys.data["Average BW"]
  hematocrit = this.phys.data["Hematocrit"]
  outlist <- c(outlist,list(BW = as.numeric(BW),
#    kgutabs = 2.18, # 1/h 
    Funbound.plasma = fup, # unitless fraction
    Funbound.plasma.dist = schmitt.params$Funbound.plasma.dist,
    hematocrit = as.numeric(hematocrit), # unitless ratio
    MW = MW, #g/mol
    Pow = Pow,
    pKa_Donor=pKa_Donor,
    pKa_Accept=pKa_Accept,
    MA=schmitt.params[["MA"]],
    kUrtc = 11.0, #Added MWL 9-20-19
    Vmucc = 0.0001)) #Added MWL 9-20-19
# Fraction unbound lipid correction: 
  if (adjusted.Funbound.plasma) 
  {
    outlist["Funbound.plasma.adjustment"] <- schmitt.params$Funbound.plasma.adjustment
  } else outlist["Funbound.plasma.adjustment"] <- NA
    
# Liver metabolism properties:
  # Unbound fraction of chemical in the hepatocyte intrinsic clearance assay:
  outlist["Fhep.assay.correction"] <- Fu_hep # fraction 
  
  # Check if saturable metabolism included:
  if (vmax==0)
  {
    if (!suppress.messages) 
      warning("Cannot calculate saturable metabolism with Vmax = 0. Defaulting to first-order metabolic clearance.")
    outlist <- c(outlist, list(
      vmax=0,
      km=1, #km value of 1 is a dummy value here
      Clint=Clint.point, 
      Clint.dist = Clint.dist,
      Clmetabolismc = as.numeric(calc_hep_clearance(
        chem.name = chem.name,
        hepatic.model="unscaled",
        parameters=list(
          Clint=Clint.point, #uL/min/10^6 cells
          Funbound.plasma=fup, # unitless fraction
          hep.assay.correction=outlist$Fhep.assay.correction, 
          million.cells.per.gliver= 110, # 10^6 cells/g-liver
          liver.density= 1.05, # g/mL
          Dn=0.17,BW=BW,
          Vliverc=lumped_params$Vliverc, #L/kg
          Qtotal.liverc=
               (lumped_params$Qtotal.liverf*as.numeric(Qcardiacc))/1000*60),
          suppress.messages=TRUE,
        species = species,
          restrictive.clearance=restrictive.clearance)), #L/h/kg BW
      million.cells.per.gliver=110, # 10^6 cells/g-liver
      liver.density=1.05)) # g/mL
  } else {
    outlist <- c(outlist,list(
      vmax=vmax,km=km,
      Clint=Clint.point, 
      Clint.dist = Clint.dist, 
      Clmetabolismc=0,                       
      million.cells.per.gliver=110, # 10^6 cells/g-liver
      liver.density=1.05)) # g/mL
  }
 
# Blood to plasma ratio:
  outlist <- c(outlist,
    Rblood2plasma=available_rblood2plasma(chem.cas=chem.cas,
      species=species,
      class.exclude=class.exclude,
      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
      suppress.messages=TRUE))

# Henry's law (water:air partitioning) coefficient:
  outlist[["logHenry"]] <- get_physchem_param(param = 'logHenry', 
                                  chem.cas=chem.cas,
                                  chem.name=chem.name,
                                  dtxsid=dtxsid) #for log base 10 compiled Henry's law values
    
# Get the blood:air and mucus:air partition coefficients:
  Kx2air <- calc_kair(chem.name=chem.name,
                      chem.cas=chem.cas,
                      dtxsid=dtxsid,
                      parameters=outlist,
                      species=species,
                      default.to.human=default.to.human,
                      adjusted.Funbound.plasma=adjusted.Funbound.plasma,
                      suppress.messages=suppress.messages)
  
  Kwater2air <- Kx2air$Kwater2air
  Kblood2air <- Kx2air$Kblood2air
  Kmuc2air <- Kx2air$Kmuc2air
        
# Set airflow based on whether exercising:
  if(exercise){
    Qalvc = ((fR*60) * (VT - VD))/outlist$BW^0.75 #L/h/kg^0.75, 
    #Added 4-30-19 to allow user-input respiratory and/or work values,
    #assumes input units of L and min^-1
  } else {                                               
    Vdot <- this.phys.data["Pulmonary Ventilation Rate"]
    # Linakis et al. (2020) Equation 5 by way of Clewell et al. (2001):
    Qalvc <- Vdot * (0.67) #L/h/kg^0.75
  }
  outlist <- c(outlist,Kblood2air =  Kblood2air,Kmuc2air = Kmuc2air,Qalvc=as.numeric(Qalvc))
    
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
            
  return(lapply(outlist[model.list[["gas_pbtk"]]$param.names],
                set_httk_precision)) 
}
