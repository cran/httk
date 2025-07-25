# Add the 1compartment model with time-dependent parameters, as specified here 
# in input.var.names

# Model identifier for the model.list:
THIS.MODEL <- "1compartment_lifestage" 

# Does this model work with Monte Carlo parameter sampling?
model.list[[THIS.MODEL]]$monte.carlo <- TRUE

# Analytic expression for steady-state plasma concentration.
model.list[[THIS.MODEL]]$analytic.css.func <- "calc_analytic_css_1comp"

# When calculating steady-state, which compartment do we test? 
# ("C" is preprended):
model.list[[THIS.MODEL]]$steady.state.compartment <- "compartment"

# What units does the analytic function return:
model.list[[THIS.MODEL]]$steady.state.units <- "mg/L"

# Function used for generating model parameters:
model.list[[THIS.MODEL]]$parameterize.func <- "parameterize_1comp"

# Function called for running the model:
model.list[[THIS.MODEL]]$solve.func <- "solve_1comp_lifestage"

# Here are the tissues from tissue.data that are considered:
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

# 1compartment model lumps everything, so list of compartments is empty.
model.list[[THIS.MODEL]]$tissuelist <- NULL

# These are all the parameters returned by the R model parameterization function.
# Some of these parameters are not directly used to solve the model, but describe
# how other parameters were calculated:
model.list[[THIS.MODEL]]$param.names <- c(
  "BW",
  "Caco2.Pab",
  "Caco2.Pab.dist",
  "Clint",
  "Clint.dist",
  "Fabsgut",
  "Fhep.assay.correction",
  "Funbound.plasma",
  "Funbound.plasma.dist",
  "Funbound.plasma.adjustment",
  "hepatic.bioavailability",
  "hematocrit",
  "kelim",
  "kgutabs",
  "liver.density",
  "million.cells.per.gliver",
  "MA",
  "MW",
  "Rblood2plasma",
  'plasma.vol',
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "Vdist")

# This subset of R parameters are needed to initially parametrize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[[THIS.MODEL]]$Rtosolvermap <- list(
  vdist="Vdist",
  ke="kelim",
  kgutabs="kgutabs",
  BW="BW")

# If the model does not include an explicit gut-liver link before systemic
# circulation, then we want to decrease the absorbed dose by the first past
# hepatic extraction factor:
model.list[[THIS.MODEL]]$do.first.pass <- TRUE

# This function translates the R model parameters into the compiled model
# parameters:
model.list[[THIS.MODEL]]$compiled.parameters.init <- "getParms1comp_lifestage"

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[[THIS.MODEL]]$compiled.param.names <- c(
  "vdist",
  "ke",
  "kgutabs",
  "BW")

# This function initializes the state vector for the compiled model:
model.list[[THIS.MODEL]]$compiled.init.func <- "initmod1comp_lifestage"

# This is the ORDERED list of input variables given to the C code by the solver
# (from Forcing (Input) functions -- forc):
model.list[[THIS.MODEL]]$input.var.names <- c(
  "d_BW",
  "d_ke",
  "d_vdist"
)

#Key forcings objects and names: name of forcing function as it appears in 
#.c model code for specification to ode solver (initforc), fcontrol list
#of arguments for fine-tuning inhalation forcing function in conjunction
#with existing ode integrator methods. Forcings series handled in model 
#solver itself
model.list[[THIS.MODEL]]$forcings.materials <- list(initforc="initforc1comp_lifestage",
                                                    fcontrol = list(method='linear',rule=2,f=0))

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[[THIS.MODEL]]$derivative.func <- "derivs1comp_lifestage"

# This is the ORDERED list of variables returned by the derivative function:
model.list[[THIS.MODEL]]$derivative.output.names <- c(
  "Ccompartment")

model.list[[THIS.MODEL]]$default.monitor.vars <- c(
  "Agutlumen",
  "Ccompartment",
  "Ametabolized",
  "AUC")


# Allowable units assigned to dosing input:
model.list[[THIS.MODEL]]$allowed.units.input <- list(
  "oral" = c('umol','mg','mg/kg'),
  "iv" = c('umol','mg','mg/kg'))

# Allowable units assigned to entries in the output columns of the ode system
model.list[[THIS.MODEL]]$allowed.units.output <- list(
  "oral" = c('umol','uM','mg/L','uM*days','mg/L*days'),
  "iv" = c('umol','uM','mg/L','uM*days','mg/L*days'))

# Default set of units assigned to correspond to each of the time dependent
# variables of the model system including state variables and any transformed
# outputs (for example, concentrations calculated from amounts.)
# AUC values should also be included.
model.list[[THIS.MODEL]]$compartment.units <- c(
  "Agutlumen"="umol",
  "Acompartment"="umol",
  "Ametabolized"="umol", 
  "Ccompartment"="uM",
  "AUC" = "uM*days")

# Compartment state of matter, needed for proper unit conversion, if all
# comaprtments of the same only include one state and set it to "all":
model.list[[THIS.MODEL]]$compartment.state <- list(liquid="all")

# These parameters specific the exposure scenario simulated by the model:
model.list[[THIS.MODEL]]$dosing.params <- c(
  "daily.dose",
  "initial.dose",
  "doses.per.day",
  "dosing.matrix")

model.list[[THIS.MODEL]]$routes <- list(
  "oral" = list(
    # We need to know which compartment gets the dose 
    "entry.compartment" = "Agutlumen",
    # desolve events can take the values "add" to add dose C1 <- C1 + dose,
    # "replace" to change the value C1 <- dose
    # or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add",
    "dosing.params" = c("daily.dose",
                        "initial.dose",
                        "doses.per.day",
                        "dosing.matrix")),
  "iv" = list(
    "entry.compartment" = "Acompartment",
    "dose.type" = "add",
    "dosing.params" = c("initial.dose",
                        "dosing.matrix"))
)

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[[THIS.MODEL]]$state.vars <- c(
  "Agutlumen",
  "Acompartment",
  "Ametabolized", 
  "AUC")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[[THIS.MODEL]]$required.params <- c(
  "Clint", 
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW"
)

# Function for calculating Clmetabolismc after Clint is varied:
model.list[[THIS.MODEL]]$propagateuv.func <- "propagate_invitrouv_1comp"

# If httk-pop is enabled:
# Function for converting httk-pop physiology to model parameters:
model.list[[THIS.MODEL]]$convert.httkpop.func <- NULL
# We want all the standard physiological calculations performed:
model.list[[THIS.MODEL]]$calc.standard.httkpop2httk <- TRUE
# These are the model parameters that are impacted by httk-pop:
model.list[[THIS.MODEL]]$httkpop.params <- c(
  "BW",
  "Fabsgut",
  "hepatic.bioavailability",
  "hematocrit",
  "liver.density",
  "million.cells.per.gliver",
  "Rblood2plasma",
  "Vdist")

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
model.list[[THIS.MODEL]]$firstpass <- TRUE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[[THIS.MODEL]]$exclude.fup.zero <- TRUE

# These are the parameter names needed to describe steady-state dosing:
model.list[[THIS.MODEL]]$css.dosing.params <- list(
  oral=c("hourly.dose"))

# Filter out compounds belonging to select chemical classes
model.list[[THIS.MODEL]]$chem.class.filt <- c("PFAS")
