# Add the dermal physiologically-base toxicokinetic (PBTK) model 
# (Meade et al., in prep.) to the list of models:

# Model identifier for the model.list:
THIS.MODEL <- "dermal_1subcomp" 

#Analytic expression for steady-state plasma concentration.
#model.list[[THIS.MODEL]]$analytic.css.func <- "calc_analytic_css_dermal_1subcomp" # added by MB 4/8/2020

# Function used for generating model parameters:
model.list[[THIS.MODEL]]$parameterize.func <- "parameterize_dermal_pbtk" 

# Function called for running the model:
model.list[[THIS.MODEL]]$solve.func <- "solve_dermal_pbtk"

# Here are the tissues from tissue.data that are considered (for example,
# do we include placenta or not?):
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
# the model: The dermal PBTK model has liver, kidney, gut, lung, and skin compartments 
# that draw info from tissue.data; everything else from alltissues should be 
# lumped.
model.list[[THIS.MODEL]]$tissuelist=list(
  liver=c("liver"),
  kidney=c("kidney"),
  lung=c("lung"),
  gut=c("gut"),
  skin=c("skin"))

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
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Klung2pu",
  "Krbc2pu",
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
  "Qkidneyf",
  "Qliverf",
  "Rblood2plasma",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vrestc",
  "Vvenc",
# Added by AEM:
  "Kskin2pu",
  "totalSA",
  "skin_depth",
  "Fskin_exposed",
  "InfiniteDose",
  "Kskin2vehicle",
  "P",
  "Kblood2air",
  "Qalvc",
  "Qskinf",
  "Vskinc"
  )
                    
# This subset of R parameters are needed to initially parameterize the compiled
# code for the solver: (must match ORDER under "parameters" in C code)
model.list[[THIS.MODEL]]$Rtosolvermap <- list(
  skin_depth = "skin_depth",
  InfiniteDose = "InfiniteDose",
  Fskin_exposed = "Fskin_exposed",
  totalSA = "totalSA",
  P = "P",
  Kskin2vehicle = "Kskin2vehicle",
  BW = "BW",
  Clmetabolismc = "Clmetabolismc",
  hematocrit = "hematocrit",
  kgutabs = "kgutabs",
  Kkidney2pu = "Kkidney2pu",
  Kliver2pu = "Kliver2pu",
  Krest2pu = "Krest2pu",
  Kgut2pu = "Kgut2pu",
  Klung2pu = "Klung2pu",
  Kskin2pu = "Kskin2pu",
  Kblood2air = "Kblood2air",
  Qalvc = "Qalvc",
  Qcardiacc = "Qcardiacc",
  Qgfrc = "Qgfrc",
  Qskinf = "Qskinf",
  Qgutf = "Qgutf",
  Qkidneyf = "Qkidneyf",
  Qliverf = "Qliverf",
  Vartc = "Vartc",
  Vgutc = "Vgutc",
  Vkidneyc = "Vkidneyc",
  Vliverc = "Vliverc",
  Vlungc = "Vlungc",
  Vrestc = "Vrestc",
  Vvenc = "Vvenc",
  Vskinc = "Vskinc",
  Fraction_unbound_plasma = "Funbound.plasma", #different to match R httk code
  Rblood2plasma = "Rblood2plasma"
  )

# This function translates the R model parameters into the compiled model
# parameters:
model.list[[THIS.MODEL]]$compiled.parameters.init <- "getParms_dermal_1subcomp" #in .c file

# This is the ORDERED full list of parameters used by the compiled code to 
# calculate the derivative of the system of equations describing the model 
model.list[[THIS.MODEL]]$compiled.param.names <- c( 
  "skin_depth",
  "InfiniteDose",
  "Fskin_exposed",
  "totalSA",
  "SA_exposed",
  "P",
  "Kskin2vehicle",
  "BW",
  "Clmetabolismc",
  "hematocrit",
  "kgutabs",
  "Kkidney2pu",
  "Kliver2pu",
  "Krest2pu",
  "Kgut2pu",
  "Klung2pu",
  "Kskin2pu",
  "Kblood2air",
  "Qalvc",
  "Qcardiacc",
  "Qgfrc",
  "Qskinf",
  "Qgutf",
  "Qkidneyf",
  "Qliverf",
  "Vartc",
  "Vgutc",
  "Vkidneyc",
  "Vliverc",
  "Vlungc",
  "Vrestc",
  "Vvenc",
  "Vskinc",
 "Fraction_unbound_plasma",
  "Rblood2plasma",
  "Clmetabolism",
  "Qalv",
  "Qcardiac",
  "Qgfr",
  "Qskin",
  "Qskin_exposed",
  "Qskin_unexposed",
  "Qgut",
  "Qkidney",
  "Qliver",
  "Qrest",
  "Vart",
  "Vgut",
  "Vkidney",
  "Vliver",
  "Vlung",
  "Vrest",
  "Vven",
  "Vskin",
  "Vskin_exposed",
  "Vskin_unexposed"
  )

# This function initializes the state vector for the compiled model:
model.list[[THIS.MODEL]]$compiled.init.func <- "initmod_dermal_1subcomp" #in .c file

# This is the function that calculates the derviative of the model as a function
# of time, state, and parameters:
model.list[[THIS.MODEL]]$derivative.func <- "derivs_dermal_1subcomp" #in .c file

# This is the ORDERED list of input variables given to the C code by the solver
# (from Forcing (Input) functions -- forc):
model.list[[THIS.MODEL]]$input.var.names <- c(
  "Vvehicle"
  )

# This is the ORDERED list of variables returned by the derivative function
# (from Model variables: Outputs):
model.list[[THIS.MODEL]]$derivative.output.names <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cskin_exposed",
  "Cskin_unexposed",
  "Cvehicle"
  )

#list of variables to be monitored (plotted). This list should be able to be
#constructed from states and outputs. 
model.list[[THIS.MODEL]]$default.monitor.vars <- c(
  "Cgut",
  "Cliver",
  "Cven",
  "Clung",
  "Cart",
  "Crest",
  "Ckidney",
  "Cplasma",
  "Aplasma",
  "Cskin_exposed",
  "Cskin_unexposed",
  "Cvehicle",
  "Ain",
  "Aexhaled",
  "Atubules",
  "Ametabolized",
  "AUC"
  )

# Allowable units assigned to dosing input:
model.list[[THIS.MODEL]]$allowed.units.input <- list(
  "oral" = c('umol','mg','mg/kg'), 
  "iv" = c('umol','mg','mg/kg'), 
  "dermal" = c('umol','mg','mg/kg'), 
  "dermal.washoff" = c('umol','mg','mg/kg'), 
  "dermal.InfiniteDose" = c('mg/L','uM','ppm','ppmw','ug/g')
  )

# Allowable units assigned to entries in the output columns of the ode system
model.list[[THIS.MODEL]]$allowed.units.output <- list(
  "oral" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "iv" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "dermal" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "dermal.washoff" = c('uM','mg/L','umol','mg','uM*days','mg/L*days'), 
  "dermal.InfiniteDose" = c('uM','mg/L','umol','mg','uM*days','mg/L*days')
  )

model.list[[THIS.MODEL]]$routes <- list( 
  "oral" = list( # not used? - AEM, March 2022
# We need to know which compartment gets the dose
    "entry.compartment" = "Agutlumen",
# desolve events can take the values "add" to add dose C1 <- C1 + dose,
# "replace" to change the value C1 <- dose
# or "multiply" to change the value to C1 <- C1*dose
    "dose.type" = "add",
    "dosing.params" = c(
      "daily.dose",
      "initial.dose",
      "doses.per.day",
      "dosing.matrix")),
  "iv" = list( # not used? - AEM, March 2022
    "entry.compartment" = "Aven",
    "dose.type" = "add",
    "dosing.params" = c(
      "initial.dose",
      "dosing.matrix")),
  "dermal" = list(
    "entry.compartment" = "Avehicle",
    "dose.type" = "add",
    "dosing.params" = c(
      "initial.dose",
      "dosing.matrix",
      "forcings")),
  "dermal.washoff" = list(
    "entry.compartment" = "Avehicle",
    "dose.type" = "replace",
    "dosing.params" = c(
      "initial.dose",
      "dosing.matrix",
      "forcings")),
  "dermal.InfiniteDose" = list( 
    "entry.compartment" = "Cvehicle_infinite",
    "dose.type" = "replace",
    "dosing.params" = c(
      "initial.dose",
      "dosing.matrix",
      "forcings"))  
  )

# ORDERED LIST of state variables (must match Model variables: 
# States in C code, each of which is associated with a differential equation),
# mostly calculated in amounts, though AUC (area under plasma concentration
# curve) also appears here: 
model.list[[THIS.MODEL]]$state.vars <- c(
  "Agutlumen",
  "Agut",
  "Aliver",
  "Aven",
  "Alung",
  "Aart",
  "Arest",
  "Akidney", 
  "Atubules",
  "Ametabolized",
  "AUC",
  "Askin_exposed",
  "Askin_unexposed",
  "Avehicle",
  "Ain",
  "Aexhaled",
  "Cvehicle_infinite"
  ) 

# Default set of units assigned to correspond to each of the "outputs" of 
# the model system, and possibly to other state variables to be monitored
# AUC values should also be included.
model.list[[THIS.MODEL]]$compartment.units <- c( 
  "Agutlumen"="umol",
  "Agut"="umol",
  "Aliver"="umol",
  "Aven"="umol",
  "Alung"="umol",
  "Aart"="umol",
  "Arest"="umol",
  "Akidney"="umol",
  "Atubules"="umol",
  "Ametabolized"="umol",
  "Askin_exposed"="umol",
  "Askin_unexposed"="umol",
  "Avehicle"="umol",
  "Ain"="umol",
  "Aexhaled"="umol",
  "Cvehicle_infinite"="uM",
  "Cgut"="uM",
  "Cliver"="uM",
  "Cven"="uM",
  "Clung"="uM",
  "Cart"="uM",
  "Crest"="uM",
  "Ckidney"="uM",
  "Cplasma"="uM",
  "Aplasma"="umol",
  "Cskin_exposed"="uM",
  "Cskin_unexposed"="uM",
  "Cvehicle"="uM",
  "AUC"="uM*days"
  )

# Compartment state of matter, needed for proper unit conversion, if all
# comaprtments of the same only include one state and set it to "all":
model.list[[THIS.MODEL]]$compartment.state <- list(liquid="all")

#Parameters needed to make a prediction (this is used by get_cheminfo):
model.list[[THIS.MODEL]]$required.params <- c(
  "Clint",
  "Funbound.plasma",
  "Pow",
  "pKa_Donor",
  "pKa_Accept",
  "MW",
  "logHenry" # for exhalation
  )

# Do we need to recalculate partition coefficients when doing Monte Carlo?
model.list[[THIS.MODEL]]$calcpc <- TRUE

# Do we need to recalculate first pass metabolism when doing Monte Carlo?
model.list[[THIS.MODEL]]$firstpass <- FALSE

# Do we ignore the Fups where the value was below the limit of detection?
model.list[[THIS.MODEL]]$exclude.fup.zero <- TRUE

#Key forcings objects and names: name of forcing function as it appears in 
#.c model code for specification to ode solver (initforc), fcontrol list
#of arguments for fine-tuning inhalation forcing function in conjunction
#with existing ode integrator methods. Forcings series handled in model 
#solver itself
model.list[[THIS.MODEL]]$forcings.materials <- list(
  initforc="initforc_dermal_1subcomp",
  fcontrol = list(method='constant',rule=2,f=0))

# Different systems of equations are better suited to different ODE solvers:
model.list[[THIS.MODEL]]$default.solver.method <- "lsoda"