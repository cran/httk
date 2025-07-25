#' Solve_PBTK
#' 
#' This function solves for the amounts or concentrations in uM of a chemical
#' in different tissues as functions of time based on the dose and dosing
#' frequency. 
#' In this PBTK formulation. \eqn{C_{tissue}} is the concentration in tissue at 
#' time t. Since the perfusion limited partition coefficients describe 
#' instantaneous equilibrium between the tissue and the free fraction in 
#' plasma, the whole plasma concentration is 
#' \eqn{C_{tissue,plasma} = \frac{1}{f_{up}*K_{tissue2fup}}*C_{tissue}}. 
#' Note that we use a single, 
#' constant value of \eqn{f_{up}} across all tissues. Corespondingly the free 
#' plasma 
#' concentration is modeled as 
#' \eqn{C_{tissue,free plasma} = \frac{1}{K_{tissue2fup}}*C_tissue}. 
#' The amount of blood flowing from tissue x is \eqn{Q_{tissue}} (L/h) at a 
#' concentration 
#' \eqn{C_{x,blood} = \frac{R_{b2p}}{f_{up}*K_{tissue2fup}}*C_{tissue}}, where 
#' we use a 
#' single \eqn{R_{b2p}} value throughout the body.
#' Metabolic clearance is modelled as being from the total plasma 
#' concentration here, though it is restricted to the free fraction in 
#' \code{\link{calc_hep_clearance}} by default. Renal clearance via 
#' glomerulsr filtration is from the free plasma concentration.
#' The compartments used in this model are the gutlumen, gut, liver, kidneys,
#' veins, arteries, lungs, and the rest of the body.
#' The extra compartments include the amounts or concentrations metabolized by
#' the liver and excreted by the kidneys through the tubules.
#' AUC is the area under the curve of the plasma concentration.
#' 
#' Note that the model parameters have units of hours while the model output is
#' in days.
#' 
#' Default NULL value for doses.per.day solves for a single dose.
#' 
#' Model Figure 
#' \if{html}{\figure{pbtk.jpg}{options: width="60\%" alt="Figure: PBTK Model
#' Schematic"}}
#' \if{latex}{\figure{pbtk.pdf}{options: width=12cm alt="Figure: PBTK Model
#' Schematic"}}
#' 
#' When species is specified as rabbit, dog, or mouse, the function uses the
#' appropriate physiological data(volumes and flows) but substitutes human
#' fraction unbound, partition coefficients, and intrinsic hepatic clearance.
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
#' @param chem.name Either the chemical name, CAS number, or the parameters
#' must be specified.
#' 
#' @param chem.cas Either the chemical name, CAS number, or the parameters must
#' be specified.
#' 
#' @param dtxsid EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})  
#' the chemical must be identified by either CAS, name, or DTXSIDs
#' 
#' @param times Optional time sequence for specified number of days.  Dosing
#' sequence begins at the beginning of times.
#' 
#' @param parameters Chemical parameters from parameterize_pbtk function,
#' overrides chem.name and chem.cas.
#' 
#' @param days Length of the simulation.
#' 
#' @param tsteps The number of time steps per hour.
#' 
#' @param daily.dose Total daily dose, defaults to mg/kg BW.
#' 
#' @param dose Amount of a single, initial oral dose in mg/kg BW. 
#' 
#' @param doses.per.day Number of doses per day.
#' 
#' @param initial.values Vector containing the initial concentrations or
#' amounts of the chemical in specified tissues with units corresponding to
#' output.units.  Defaults are zero.
#' 
#' @param plots Plots all outputs if true.
#' 
#' @param suppress.messages Whether or not the output message is suppressed.
#' 
#' @param species Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
#' default "Human").
#' 
#' @param iv.dose Simulates a single i.v. dose if true.
#' 
#' @param input.units Input units of interest assigned to dosing, defaults to
#' mg/kg BW
#' 
#' @param output.units A named vector of output units expected for the model
#' results. Default, NULL, returns model results in units specified in the
#' 'modelinfo' file. See table below for details.
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
#' @param recalc.blood2plasma Recalculates the ratio of the amount of chemical
#' in the blood to plasma using the input parameters, calculated with
#' hematocrit, Funbound.plasma, and Krbc2pu.
#' 
#' @param recalc.clearance Recalculates the the hepatic clearance
#' (Clmetabolism) with new million.cells.per.gliver parameter.
#' 
#' @param dosing.matrix Vector of dosing times or a matrix consisting of two
#' columns or rows named "dose" and "time" containing the time and amount, in
#' mg/kg BW, of each dose.
#' 
#' @param adjusted.Funbound.plasma Uses adjusted Funbound.plasma when set to
#' TRUE along with partition coefficients calculated with this value.
#' 
#' @param regression Whether or not to use the regressions in calculating
#' partition coefficients.
#' 
#' @param restrictive.clearance Protein binding not taken into account (set to
#' 1) in liver clearance if FALSE.
#' 
#' @param minimum.Funbound.plasma Monte Carlo draws less than this value are set 
#' equal to this value (default is 0.0001 -- half the lowest measured Fup in our
#' dataset).
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
#' @param monitor.vars Which variables are returned as a function of time. 
#' The default value of NULL provides "Cgut", "Cliver", "Cven", "Clung", "Cart", 
#' "Crest", "Ckidney", "Cplasma", "Atubules", "Ametabolized", and "AUC"
#'
#' @param ... Additional arguments passed to the integrator (\code{\link[deSolve]{ode}}).
#'
#' @return A matrix of class deSolve with a column for time(in days), each
#' compartment, the area under the curve, and plasma concentration and a row
#' for each time point.
#'
#' @author John Wambaugh and Robert Pearce
#'
#' @references 
#' \insertRef{pearce2017httk}{httk}
#'
#' @seealso \code{\link{solve_model}}
#'
#' @seealso \code{\link{parameterize_gas_pbtk}}
#'
#' @seealso \code{\link{calc_analytic_css_pbtk}} 
#'
#' @keywords Solve pbtk
#'
#' @examples
#' \donttest{
#' 
#' # Multiple doses per day:
#' head(solve_pbtk(
#'   chem.name='Bisphenol-A',
#'   daily.dose=.5,
#'   days=2.5,
#'   doses.per.day=2,
#'   tsteps=2))
#' 
#' # Starting with an initial concentration:
#' out <- solve_pbtk(
#'   chem.name='bisphenola',
#'   dose=0,
#'   days=2.5,
#'   output.units="mg/L", 
#'   initial.values=c(Agut=200))
#'
#' # Working with parameters (rather than having solve_pbtk retrieve them):
#' params <- parameterize_pbtk(chem.cas="80-05-7")
#' head(solve_pbtk(parameters=params, days=2.5))
#'                   
#' # We can change the parameters given to us by parameterize_pbtk:
#' params <- parameterize_pbtk(dtxsid="DTXSID4020406", species = "rat")
#' params["Funbound.plasma"] <- 0.1
#' out <- solve_pbtk(parameters=params, days=2.5)
#' 
#' # A fifty day simulation:
#' out <- solve_pbtk(
#'   chem.name = "Bisphenol A", 
#'   days = 50, 
#'   daily.dose=1,
#'   doses.per.day = 3)
#' plot.data <- as.data.frame(out)
#' css <- calc_analytic_css(chem.name = "Bisphenol A")
#' 
#' library("ggplot2")
#' c.vs.t <- ggplot(plot.data, aes(time, Cplasma)) + 
#'   geom_line() +
#'   geom_hline(yintercept = css) + 
#'   ylab("Plasma Concentration (uM)") +
#'   xlab("Day") + 
#'   theme(
#'     axis.text = element_text(size = 16), 
#'     axis.title = element_text(size = 16), 
#'     plot.title = element_text(size = 17)) +
#'   ggtitle("Bisphenol A")
#' print(c.vs.t)
#'
#' # The following will not work because Diquat dibromide monohydrate's 
#' # Henry's Law Constant (-3.912) is higher than that of Acetone (~-4.5):
#' try(head(solve_pbtk(chem.cas = "6385-62-2")))
#' # However, we can turn off checking for phys-chem properties, since we know
#' # that  Diquat dibromide monohydrate is not too volatile:
#' head(solve_pbtk(chem.cas = "6385-62-2", physchem.exclude = FALSE))
#'
#' # Caco-2 absorption tests:
#' p <- parameterize_pbtk(chem.name="Aminopterin")
#' # calculate what initial dose of 1 mg/kg should be in uM in the gut:
#' initial.dose <- signif(1/1e3*1e6/p[["MW"]]*p[["BW"]]*p[["Fabsgut"]],
#'                        4)
#' # This should be the same as what solve_pbtk givesus:
#' initial.dose == solve_pbtk(chem.cas="80-05-7",days=1)[1,"Agutlumen"]
#' 
#' # By default we now include calculation of Fabs and Fgut (we explicitly model
#' # first-pass hepatic metabolism in the model "pbtk")
#' head(solve_pbtk(chem.cas="80-05-7",days=1))
#' # Therefore if we set Fabs = Fgut = 1 with keetit100=TRUE, we should get a
#' # higher tissue concentrations:
#' head(solve_pbtk(chem.cas="80-05-7",days=1,
#'                 Caco2.options=list(keepit100=TRUE)))
#'
#' # Different ways to call the function:
#' head(solve_pbtk(chem.cas="80-05-7",days=1))
#' head(solve_pbtk(parameters=parameterize_pbtk(chem.cas="80-05-7"),days=1))
#' }
#' 
#' @export solve_pbtk
#'
#' @useDynLib httk
solve_pbtk <- function(chem.name = NULL,
                    chem.cas = NULL,
                    dtxsid = NULL,
                    times=NULL,
                    parameters=NULL,
                    days=10,
                    tsteps = 4, # tsteps is number of steps per hour
                    daily.dose = NULL,
                    dose = NULL,  
                    doses.per.day=NULL,
                    initial.values=NULL,
                    plots=FALSE,
                    suppress.messages=FALSE,
                    species="Human",
                    iv.dose=FALSE,
                    input.units='mg/kg',
                    # output.units='uM',
                    output.units=NULL,
                    default.to.human=FALSE,
                    class.exclude=TRUE,
                    physchem.exclude = TRUE,
                    recalc.blood2plasma=FALSE,
                    recalc.clearance=FALSE,
                    dosing.matrix=NULL,
                    adjusted.Funbound.plasma=TRUE,
                    regression=TRUE,
                    restrictive.clearance = TRUE,
                    minimum.Funbound.plasma=0.0001,
                    Caco2.options = list(),
                    monitor.vars=NULL,
                    ...)
{
  out <- solve_model(
    chem.name = chem.name,
    chem.cas = chem.cas,
    dtxsid = dtxsid,
    times=times,
    parameters=parameters,
    model="pbtk",
    route= ifelse(iv.dose,"iv","oral"),
    dosing=list(
      initial.dose=dose,
      dosing.matrix=dosing.matrix,
      daily.dose=daily.dose,
      doses.per.day=doses.per.day
    ),
    days=days,
    tsteps = tsteps, # tsteps is number of steps per hour
    initial.values=initial.values,
    plots=plots,
    monitor.vars=monitor.vars,
    suppress.messages=suppress.messages,
    species=species,
    input.units=input.units,
    output.units=output.units,
    recalc.blood2plasma=recalc.blood2plasma,
    recalc.clearance=recalc.clearance,
    adjusted.Funbound.plasma=adjusted.Funbound.plasma,
    minimum.Funbound.plasma=minimum.Funbound.plasma,
    parameterize.args.list =list(
                      default.to.human=default.to.human,
                      clint.pvalue.threshold=0.05,
                      restrictive.clearance = restrictive.clearance,
                      regression=regression,
                      Caco2.options=Caco2.options,
                      class.exclude=class.exclude,
                      physchem.exclude=physchem.exclude),
    ...)
  
  return(out) 
}
