\name{calc_total_clearance}
\alias{calc_total_clearance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the total clearance.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the total clearance rate for a one compartment model where 
clearance is entirely due to metablism by the liver and glomerular filtration in the kidneys, identical to clearance of three compartment steady state model.
}
\usage{
calc_total_clearance(chem.cas=NULL,chem.name=NULL,parameters=NULL,species="Human",
                     fu.hep.correct=T,suppress.messages=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{chem.name}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{chem.cas}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{parameters}{Chemical parameters from parameterize_steadystate function, overrides chem.name and chem.cas.}
  \item{species}{
  Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
 \item{fu.hep.correct}{Whether or not to correct the in vitro measured hepatic clearance for the fraction of chemical predicted to bind to hepatocytes using the method of Kilford et al. (2008) (default to TRUE).}
 \item{suppress.messages}{Whether or not the output message is suppressed.} 
 
}

\details{
When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Total Clearance}{Units of L/h/kg BW.}
}

\author{
John Wambaugh
}

\examples{
calc_total_clearance(chem.name="Ibuprofen")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
