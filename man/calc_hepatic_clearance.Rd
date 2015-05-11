\name{calc_hepatic_clearance}
\alias{calc_hepatic_clearance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the hepatic clearance.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates the hepatic clearance for a well-stirred model or other type if specified.
}
\usage{
calc_hepatic_clearance(chem.name=NULL,chem.cas=NULL,parameters=NULL,species='Human',
                       hepatic.model='well-stirred',suppress.messages=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{chem.name}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{chem.cas}{Either the chemical name, CAS number, or the parameters must be specified.}
  \item{species}{
  Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").
%%     ~~Describe \code{ssparams.var.inv} here~~
}
  \item{parameters}{Chemical parameters from parameterize_steadystate function, overrides chem.name and chem.cas.}
  \item{hepatic.model}{Model used in calculating hepatic clearance, unscaled, parallel tube, dispersion, or default well-stirred.}  
   \item{suppress.messages}{Whether or not to suppress the output message.}
}

\details{
When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Hepatic Clearance}{Units of L/h/kg BW.}
}

\author{
John Wambaugh
}

\examples{
calc_hepatic_clearance(chem.name="Ibuprofen")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
