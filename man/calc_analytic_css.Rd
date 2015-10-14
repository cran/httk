\name{calc_analytic_css}
\alias{calc_analytic_css}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the analytic steady state concentration.
}
\description{
This function calculates the analytic steady state plasma or venous blood concentrations as a result of infusion dosing for the three compartment and multiple compartment PBTK models.
}
\usage{
calc_analytic_css(chem.name=NULL,chem.cas = NULL,parameters=NULL,daily.dose=1,
                  output.units='uM',model = 'pbtk',species='Human',
                  concentration='plasma',suppress.messages=F,
                  recalc.blood2plasma=F,default.to.human=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chem.name}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{chem.cas}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{parameters}{Chemical parameters from parameterize_pbtk (for model = 'pbtk'), parameterize_3comp (for model = '3compartment), parmeterize_1comp(for model = '3compartmentss') or parameterize_steadystate (for model = '1compartment'), overrides chem.name and chem.cas.}
  \item{daily.dose}{Total daily dose, mg/kg BW.}
  \item{output.units}{Units for returned concentrations, defaults to uM (specify units = "uM") but can also be mg/L.}
  \item{model}{Model used in calculation, 'pbtk' for the multiple compartment model,'3compartment' for the three compartment model, '3compartmentss' for the three compartment steady state model, and '1compartment' for one compartment model.}
  \item{species}{
   Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human"). 
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{suppress.messages}{Whether or not the output message is suppressed.}
  \item{concentration}{Desired concentration type, 'blood' or default 'plasma'.}
  \item{recalc.blood2plasma}{Recalculates the ratio of the amount of chemical in the blood to plasma using the input parameters, calculated with hematocrit, Funbound.plasma, and Krbc2pu.} 
  \item{default.to.human}{
  Substitutes missing rat values with human values if true.
  }
} 
 
\details{

When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 

%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Steady state concentration}{}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Schmitt W. "General approach for the calculation of tissue to plasma partition coefficients." Toxicology In Vitro, 22, 457-467 (2008).
}
\author{
Robert Pearce
}

\examples{
calc_analytic_css(chem.name='Bisphenol-A',output.units='mg/L',
                  model='3compartment',concentration='blood')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Solve}
