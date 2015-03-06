\name{solve_1comp}
\alias{solve_1comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Solve one compartment TK model
}
\description{
This function solves for the amount or concentration of a chemical in plasma or blood for a one compartment model as a function of time based on the dose and dosing frequency.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
solve_1comp(chem.cas=NULL,chem.name=NULL,times=NULL,parameters=NULL,daily.dose=1,
            doses.per.day=0, days=10,tsteps = 4, suppress.messages=F,
            species='Human',output.units='uM',plots=F,initial.value=0,
            iv.dose=F,fu.hep.correct=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{chem.name}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{chem.cas}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{times}{Optional time sequence for specified number of days.}
  \item{parameters}{Chemical parameters from parameterize_1comp function, overrides chem.name and chem.cas.}
  \item{days}{Length of the simulation.}
  \item{tsteps}{The number time steps per hour.}
  \item{daily.dose}{Total daily dose, mg/ kg BW.}
  \item{doses.per.day}{Number of doses per day.}
  \item{species}{Species desired (either "Rat", "Rabbit", "Dog", or default "Human"). }
  \item{iv.dose}{Simulates a single i.v. dose if true.}
  \item{output.units}{Desired units (either "mg/L", "mg", "umol", or default "uM").}
  \item{initial.value}{Initial concentration or amount of chemical in the compartment.  Contains same units as output.units, default of "uM".  Defaults to zero.}
   \item{suppress.messages}{Whether or not the output message is suppressed.}
  \item{plots}{Plots all outputs if true.}  
  \item{fu.hep.correct}{Whether or not to correct the in vitro measured hepatic clearance for the fraction of chemical predicted to bind to hepatocytes using the method of Kilford et al. (2008) (default to TRUE).}
  \item{...}{Additional arguments passed to the integrator.}

}

\details{
Default value of 0 for doses.per.day solves for a single dose.


 When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance.
 
 AUC is area under plasma concentration curve. 
   

%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix with a column for time(in days) and a column for the compartment and the area under the curve (concentration only).
     
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Robert Pearce
}

\examples{
solve_1comp(chem.name='Bisphenol-A')
\dontrun{
solve_1comp(chem.name='Bisphenol-A',doses.per.day=3,tsteps=50,days=20)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
