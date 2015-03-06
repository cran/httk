\name{calc_mc_css}
\alias{calc_mc_css}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the monte carlo steady state concentration.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function finds the analytical steady state plasma concentration(from calc_analytic_css) for the three compartment steady state model (model = '3compartmentss') using a monte carlo simulation (monte_carlo).
}
\usage{
calc_mc_css(chem.cas=NULL,chem.name=NULL,parameters=NULL,daily.dose=1,
            which.quantile=0.95,species="Human",output.units="mg/L",suppress.messages=F,
            censored.params=list(Fraction_unbound_plasma=list(cv=0.3,lod=0.01)),
            vary.params=list(BW=0.3,liver.volume.per.kgBW=0.3,QGFRc=0.3,Qhc=0.3,
            million.cells.per.gliver=0.3,CLint=0.3),samples=1000,
            fu.hep.correct=T,return.samples=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{chem.name}{
  Either the chemical parameters, name, or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
  Either the CAS number, parameters, or the chemical name must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{parameters}{Parameters from parameterize_steadystate.}
  \item{daily.dose}{Total daily dose, mg/kg BW/day.}
  \item{which.quantile}{
   Which quantile from Monte Carlo simulation is requested. Can be a vector.
%%     ~~Describe \code{ssparams.mean} here~~
}
  \item{species}{
   Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human"). 
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{output.units}{Plasma concentration units, either uM or default mg/L.}
  \item{suppress.messages}{Whether or not to suppress output message.}
  \item{censored.params}{The parameters listed in censored.params are sampled from a normal distribution 
that is censored for values less than the limit of detection (specified separately
for each paramter). This argument should be a list of sub-lists. Each sublist 
is named for a parameter in "parameters"
and contains two elements: "CV" (coefficient of variation) and "LOD" (limit of
detection, below which parameter values are censored. New values are sampled 
with mean equal to the value in "parameters" and standard deviation equal to the 
mean times the CV.  Censored values are sampled on a uniform distribution between
0 and the limit of detection.}
  \item{vary.params}{The parameters listed in vary.params are sampled from a normal distribution that is
truncated at zero. This argument should be a list of coefficients of variation 
(CV) for the normal distribution. Each entry in the list is named for a 
parameter in "parameters". New values are sampled with mean equal to the value in
"parameters" and standard deviation equal to the mean times the CV.}
  \item{samples}{Number of samples generated in calculating quantiles.}
  \item{fu.hep.correct}{Whether or not to correct the in vitro measured hepatic clearance for the fraction of chemical predicted to bind to hepatocytes using the method of Kilford et al. (2008) (default to TRUE).}
  \item{return.samples}{Whether or not to return the vector containing the samples from the simulation instead of the selected quantile.}
 
  

%%     ~~Describe \code{pred} here~~
}


\details{
  When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 

}

\author{
John Wambaugh
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
 calc_mc_css(chem.name='Bisphenol A',output.units='uM',which.quantile=.9)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
