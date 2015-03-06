\name{calc_ratioblood2plasma}
\alias{calc_ratioblood2plasma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the constant ratio of the blood concentration to the plasma concentration.
}
\description{
This function calculates the constant ratio of the blood concentration to the plasma concentration. It uses the hematocrit and the red blood cell (RBC) parition coefficient as predicted by the Schmitt (2008) method.
}
\usage{
calc_ratioblood2plasma(chem.cas=NULL, chem.name=NULL, default.to.human=F, species="Human")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chem.name}{
  Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
  Either the CAS number or the chemical name must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true.
  }
  \item{species}{
   Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
}
\details{
When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 
%%  ~~ If necessary, more details than the description above ~~
}

\references{
Schmitt W. "General approach for the calculation of tissue to plasma partition coefficients." Toxicology In Vitro, 22, 457-467 (2008).
}
\author{
John Wambaugh
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
calc_ratioblood2plasma(chem.name="Bisphenol A")
calc_ratioblood2plasma(chem.name="Bisphenol A",species="Rat")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
