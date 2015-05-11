\name{calc_css}
\alias{calc_css}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the steady state concentration and the day it is reached.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function finds the day a chemical comes within the specified range of the analytical steady state venous blood or plasma concentration(from calc_analytic_css) for the multiple compartment, three compartment, and one compartment models, the fraction of the true steady state value reached on that day, the maximum concentration, and the average concentration at the end of the simulation.
}
\usage{
calc_css(parameters=NULL,chem.name=NULL,chem.cas=NULL,species="Human", p = .01,
         daily.dose=1, doses.per.day=3,days = 10,output.units = "uM",
         concentration='plasma',suppress.messages=F,model='pbtk',default.to.human=F,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
 \item{chem.name}{
   Either the chemical name, CAS number, or parameters must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
   Either the chemical name, CAS number, or parameters must be specified.
%%     ~~Describe \code{pred} here~~
}

  \item{p}{
  Fractional distance from the final steady state concentration that the average concentration must come within to be considered at steady state.
%%     ~~Describe \code{pred} here~~
}
  \item{parameters}{Chemical parameters from parameterize_pbtk function, overrides chem.name and chem.cas.}
  \item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").}
 \item{daily.dose}{Total daily dose, mg/kg BW.}
  \item{doses.per.day}{Number of doses per day.}
  \item{days}{Initial number of days to run simulation that is multiplied on each iteration.}
  \item{output.units}{Units for returned concentrations, defaults to uM (specify units = "uM") but can also be mg/L.}
   \item{concentration}{Desired concentration type, 'blood' or default 'plasma'.}
   \item{suppress.messages}{Whether or not to suppress messages.}
   \item{model}{Model used in calculation, 'pbtk' for the multiple compartment model,'3compartment' for the three compartment model, '3compartmentss' for the three compartment steady state model, and '1compartment' for one compartment model.}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true (hepatic intrinsic clearance or fraction of unbound plasma).
  }
  \item{...}{Additional arguments passed to model solver (default of solve_pbtk).}
  


}

\value{
\item{frac}{Fraction of the true steady state concentration reached on the day steady state is reached.}
\item{max}{The maximum concentration of the simulation.}
\item{avg}{The average concentration on the final day of the simulation.}
\item{the.day}{The day the average concentration comes within 100 * p percent of the true steady state concentration.}

}
\details{
 When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 

}

\author{
Robert Pearce
}

\examples{
parms <- parameterize_pbtk(chem.name='Bisphenol-A')
calc_css(parms,concentration='blood')
calc_css(chem.name='Bisphenol-A',doses.per.day=5,p=.001,output.units='mg/L')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Steady State}