\name{predict_partitioning_schmitt}
\alias{predict_partitioning_schmitt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict tissue:free plasma partition coefficients using the method from Schmitt (2008).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function implements the method from Schmitt (2008) in predicting the partition
coefficients from for the tissues contained in the tissue.data table.
}
\usage{
predict_partitioning_schmitt(fupl,Pow,pKa_Donor=NA,pKa_Accept=NA,Dplw=NULL,alpha=0.001,
                             FPpl = 75/1000/1.025,plasma.pH = 7.4,temperature=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

 \item{fupl}{unbound fraction in plasma}
 \item{Pow}{octonol:water partition coefficient (not log transformed)}
 \item{pKa_Donor}{compound H dissociation equilibirum constant(s)}
  \item{pKa_Accept}{compound H association equilibirum constant(s)}
  \item{Dplw}{phospholipid:water distribution coefficient, membrane affinity}
  \item{alpha}{Proportionality constant used in distribution coefficient calculation.}
  \item{FPpl}{protein fraction in plasma - from Gardner 1980}
   \item{plasma.pH}{pH of the plasma}
   \item{temperature}{body temperature of species}
}

\value{
Returns partition coefficients for each tissue.

}
\details{
A regression to the data of Endo et al., (2011) is used for Dplw, when not provided.

}

\references{
Endo, S., Escher, B. I. and Goss, K.-U. (2011). Capacities of Membrane Lipids to Accumulate Neutral Organic Chemicals. Environmental Science & Technology 45(14), 5912-5921, 10.1021/es200855w.

Schmitt, W. (2008a). General approach for the calculation of tissue to plasma partition coefficients. Toxicology in vitro 22(2), 457-467, 10.1016/j.tiv.2007.09.010.

Schmitt, W. (2008b). Corrigendum to: "General approach for the calculation of tissue to plasma partition coefficients" [Toxicology in Vitro 22 (2008) 457-467]. Toxicology in Vitro 22(6), 1666, http://dx.doi.org/10.1016/j.tiv.2008.04.020.
}

\author{
John Wambaugh, Robert Pearce, and Jimena Davis
}

\examples{
predict_partitioning_schmitt(fupl=.5,Pow=10)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
