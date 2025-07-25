# Add the 3compartment model with time-dependent parameters, as specified here 
# in input.var.names

# Model identifier for the model.list:
THIS.MODEL <- "3compartment_lifestage" 

# Dose this model work with Monte Carlo parameter sampling?
model.list[[THIS.MODEL]]$monte.carlo <- TRUE

# Analytic expression for steady-state plasma concentration to be used by
# calc_analytic_css:
model.list[[THIS.MODEL]]$analytic.css.func <- "calc_analytic_css_3comp"

# What units does the analytic function return in calc_analytic_css:
model.list[[THIS.MODEL]]$steady.state.units <- "mg/L"

# When calculating steady-state with calc_css, which compartment do we test? 
# ("C" is preprended):
model.list[[THIS.MODEL]]$steady.state.compartment <- "plasma"

# Function used for generating model parameters:
model.list[[THIS.MODEL]]$parameterize.func <- "parameterize_3comp"

# Function called for running the model:
model.list[[THIS.MODEL]]$solve.func <- "solve_3comp_lifestage"

# Here are the tissues from tissue.data that are considered:
# They should correspond
# in name to the names present in the tissue.data object, if the parameters
# necessary for describing the tissue/compartment aren't going to be provided
# otherwise.
model.list[[THIS.MODEL]]$alltissues=c(
  "adipose",
  "bone",            
  "brain",           
  "gut",            
  "heart",           
  "kidney",          
  "liver",           
  "lung",           
  "muscle", 
  "skin",            
  "spleen",          
  "red blood cells",
  "rest")

# Which tissues from tissue.data are not lumped together when forming
# the model: 3 compartment model has only liver and gut compartments; 
# everything else is lumped.
model.list[[THIS.MODEL]]$tissuelist = list(
               liver=c("liver"),
               gut=c("gut"))

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[[THIS.MODEL]]$param.names <- c(
  "BW",
  "Caco2.Pab",
  "Caco2.Pab.dist",
  "Clint",
  "Clint.dist",
  "Clmetabolismc",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "Funbound.plasma.adjustment",
  "Fabsgut",
  "Fhep.assay.correction",
  "hematocrit",
  "Kgut2pu",
  "Krbc2pu",
  "kgutabs",
  "Kliver2pu",
  "Krest2pu",
  "liver.density",
  "million.cells.per.gliver",
  "MW",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MA",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qliverf",
  "Rblood2plasma",
  "Vgutc",
  "Vliverc",
  "Vrestc"
  )
                    
#
# String representations of the R version of names of
# the parameters are assigned to the C variable name in this scheme.
model.list[[THIS.MODEL]]$Rtosolvermap <- list(
  BW = "BW",
  CLmetabolismc="Clmetabolismc",
  kgutabs="kgutabs",
  Qcardiacc="Qcardiacc",
  Qgfrc="Qgfrc",
  Qgutf="Qgutf",
  Qliverf="Qliverf",
  Vportvenc="Vgutc",
  Vliverc="Vliverc",
  Vsyscompc="Vrestc",
  Fraction_unbound_plasma="Funbound.plasma",
  Kliver2plasma="Kliver2pu",
  Krest2plasma="Krest2pu",
  Ratioblood2plasma="Rblood2plasma"
)

# This function translates the R model parameters into the compiled model
# parameters:
model.list[[THIS.MODEL]]$compiled.parameters.init <- "getParms3comp_lifestage"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[[THIS.MODEL]]$compiled.param.names <- c(
  "BW",
  "Clmetabolismc",
  "kgutabs",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qliverf",
  "Vportvenc",
  "Vliverc",
  "Vsyscompc",
  "Vportven",
  "Vliver",
  "Vsyscomp",
  "Fraction_unbound_plasma",
  "Clmetabolism",
  "Qcardiac",
  "Qgfr",
  "Qgut",
  "Qliver",
  "Kliver2plasma",
  "Krest2plasma",
  "Ratioblood2plasma"
  )

# This function initializes the state vector for the compiled model:
model.list[[THIS.MODEL]]$compiled.init.func <- "initmod3comp_lifestage"

# This is the ORDERED list of input variables given to the C code by the solver
# (from Forcing (Input) functions -- forc):
model.list[[THIS.MODEL]]$input.var.names <- c(
  "d_BW",
  "d_CLmetabolismc",
  "d_Krest2plasma",
  "d_Qcardiacc",
  "d_Qgfrc",
  "d_Vliverc",
  "d_Vportvenc",
  "d_Vsyscompc"
)

#Key forcings objects and names: name of forcing function as it appears in 
#.c model code for specification to ode solver (initforc), fcontrol list
#of arguments for fine-tuning inhalation forcing function in conjunction
#with existing ode integrator methods. Forcings series handled in model 
#solver itself
model.list[[THIS.MODEL]]$forcings.materials <- list(initforc="initforc3comp_lifestage",
                                                    fcontrol = list(method='linear',rule=2))

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[[THIS.MODEL]]$derivative.func <- "derivs3comp_lifestage"

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[[THIS.MODEL]]$derivative.output.names <- c(
  "Cportven",
  "Cliver",
  "Csyscomp",
  "Cplasma"
)

model.list[[THIS.MODEL]]$default.monitor.vars <- c(
  "Cliver",
  "Csyscomp",
  "Cplasma",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units assigned to dosing input:
model.list[[THIS.MODEL]]$allowed.units.input <- list(
       "oral" = c('umol','mg','mg/kg'),
       "iv" = c('umol','mg','mg/kg'))
       
# Allowable units assigned to entries in the output columns of the ode system
model.list[[THIS.MODEL]]$allowed.units.output <- list(
              "oral" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'),
              "iv" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'))
  
model.list[[THIS.MODEL]]$routes <- list(
  "oral" = list(
    # We need to know which compartment gets the dose 
    "entry.compartment" = "Aintestine",
    # desolve events can take the values "add" to add dose C1 <- C1 + dose,
    # "replace" to change the value C1 <- dose
    # or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add",
    "dosing.params" = c("daily.dose",
                        "initial.dose",
                        "doses.per.day",
                        "dosing.matrix")),
  "iv" = list(
    "entry.compartment" = "Asyscomp",
    "dose.type" = "add",
    "dosing.params" = c("initial.dose",
                        "dosing.matrix"))
  )    

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[[THIS.MODEL]]$state.vars <- c(
  "Aintestine",
  "Aportven",
  "Aliver",
  "Asyscomp",
  "Ametabolized",
  "Atubules",
  "AUC"
  )

# Default set of units assigned to correspond to each of the time dependent
# variables of the model system including state variables and any transformed
# outputs (for example, concentrations calculated from amounts.)
# AUC values should also be included.
model.list[[THIS.MODEL]]$compartment.units <-c(
    "Aintestine"="umol",
    "Aportven"="umol",
    "Aliver"="umol",
    "Asyscomp"="umol",
    "Ametabolized"="umol",
    "Atubules"="umol",
    "Cportven"="uM",
    "Cliver"="uM",
    "Csyscomp"="uM",
    "Cplasma"="uM",
    "AUC"="uM*days"
  )

# Compartment state of matter, needed for proper unit conversion, if all
# comaprtments of the same only include one state and set it to "all":
model.list[[THIS.MODEL]]$compartment.state <- list(liquid="all")

# Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[[THIS.MODEL]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
   )

# Function for calculating Clmetabolismc after Clint is varied:
model.list[[THIS.MODEL]]$propagateuv.func <- "propagate_invitrouv_3comp"

# If httk-pop is enabled:
# Function for converting httk-pop physiology to model parameters:
model.list[[THIS.MODEL]]$convert.httkpop.func <- NULL# We want all the standard physiological calculations performed:

# We want all the standard physiological calculations performed:
model.list[[THIS.MODEL]]$calc.standard.httkpop2httk <- TRUE

# These are the model parameters that are impacted by httk-pop:
model.list[[THIS.MODEL]]$httkpop.params <- c(
  "BW",
  "Qcardiacc",
  "Qgfrc",
  "Qgutf",
  "Qliverf",
  "Vportvenc",
  "Vliverc",
  "Vsyscompc",
  "Vportven",
  "Vliver",
  "Vsyscomp",
  "Qcardiac",
  "Qgfr",
  "Qgut",
  "Qliver",
  "Ratioblood2plasma")

model.list[[THIS.MODEL]]$invitro.params <- c("Funbound.plasma",
                                             "Clint",
                                             "unadjusted.Funbound.plasma",
                                             "fup.mean",
                                             "Funbound.plasma.adjustment",
                                             "Caco2.Pab",
                                             "Fabsgut")

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[[THIS.MODEL]]$calcpc <- TRUE

# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[[THIS.MODEL]]$firstpass <- FALSE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[[THIS.MODEL]]$exclude.fup.zero <- TRUE

# These are the parameter names needed to describe steady-state dosing:
model.list[[THIS.MODEL]]$css.dosing.params <- list(
  oral=c("hourly.dose"))

# Filter out volatile compounds with Henry's Law Constant Threshold
model.list[[THIS.MODEL]]$log.henry.threshold <- c(-4.5)

# Filter out compounds belonging to select chemical classes
model.list[[THIS.MODEL]]$chem.class.filt <- c("PFAS")
