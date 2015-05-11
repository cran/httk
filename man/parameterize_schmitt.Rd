\name{parameterize_schmitt}
\alias{parameterize_schmitt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parameterize Schmitt's method.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function provides the necessary parameters to run predict_partitioning_schmitt,
excluding the data in tissue.data.
}
\usage{
 parameterize_schmitt(chem.cas=NULL,chem.name=NULL,species="Human",default.to.human=F)
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
  Substitutes missing fraction of unbound plasma with human values if true.
  }
}
\value{

 \item{Funbound.plasma}{unbound fraction in plasma}
 \item{Pow}{octonol:water partition coefficient (not log transformed)}
 \item{pKa_Donor}{compound H dissociation equilibirum constant(s)}
  \item{pKa_Accept}{compound H association equilibirum constant(s)}
  \item{MA}{phospholipid:water distribution coefficient, membrane affinity}
  \item{Fprotein.plasma}{protein fraction in plasma - from Gardner 1980}
   \item{plasma.pH}{pH of the plasma}
   \item{temperature}{body temperature of species}
}


\details{
When species is specified as rabbit, dog, or mouse, the human unbound fraction
is substituted.
}

\author{
Robert Pearce
}

\examples{
parameterize_schmitt(chem.name='bisphenola')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Parameter}
