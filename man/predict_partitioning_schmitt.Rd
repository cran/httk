\name{predict_partitioning_schmitt}
\alias{predict_partitioning_schmitt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict partition coefficients using the method from Schmitt (2008).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function implements the method from Schmitt (2008) in predicting the tissue to unbound plasma partition 
coefficients from for the tissues contained in the tissue.data table.
}
\usage{
predict_partitioning_schmitt(chem.name=NULL,chem.cas=NULL,species="Human",
                             default.to.human=F,parameters=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
\item{chem.name}{
   Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
   Either the chemical name or the CAS number must be specified.
%%     ~~Describe \code{pred} here~~
}
  \item{species}{
    Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human"). 
}
  \item{default.to.human}{
  Substitutes missing animal values with human values if true (hepatic intrinsic clearance or fraction of unbound plasma).
  }
  \item{parameters}{Chemical parameters from the parameterize_schmitt function, overrides chem.name and chem.cas.}
}

\value{
Returns tissue to unbound plasma partition coefficients for each tissue.

}
\details{
A regression is used for MA when not provided.

}

\author{
Robert Pearce
}

\examples{
predict_partitioning_schmitt(chem.name='ibuprofen')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}

