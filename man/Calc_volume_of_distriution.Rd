\name{calc_volume_of_distribution}
\alias{calc_volume_of_distribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the volume of distribution for a one compartment model.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
 This function predicts partition coefficients for all tissues, then lumps them into a single compartment. The effective volume of distribution is calculated by summing each tissues volume times it's partition coefficient relative to plasma. Plasma, and the paritioning into RBCs are also added to get the total volume of distribution in L/KG BW. Partition coefficients are calculated using Schmitt's (2008) method. 
}
\usage{
calc_volume_of_distribution(chem.cas=NULL, chem.name=NULL, default.to.human=F,
                            species="Human", suppress.messages=F)
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
\item{suppress.messages}{Whether or not the output message is suppressed.}
}
\details{
When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 

%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Volume of distribution}{Units of L/ kg BW.} 
}
\references{
Schmitt W. "General approach for the calculation of tissue to plasma partition coefficients." Toxicology In Vitro, 22, 457-467 (2008).
Peyret, T., Poulin, P., Krishnan, K., "A unified algorithm for predicting partition coefficients for PBPK modeling of drugs and environmental chemicals." Toxicology and Applied Pharmacology, 249, 197-207 (2010).
}
\author{
John Wambaugh
}


\examples{
calc_volume_of_distribution(chem.cas="80-05-7")
calc_volume_of_distribution(chem.name="Bisphenol A")
calc_volume_of_distribution(chem.name="Bisphenol A",species="Rat")
calc_volume_of_distribution(chem.name="Bisphenol A",species="Rabbit")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
