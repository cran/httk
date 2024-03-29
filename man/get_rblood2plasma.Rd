% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_rblood2plasma.R
\name{get_rblood2plasma}
\alias{get_rblood2plasma}
\title{Get ratio of the blood concentration to the plasma concentration.}
\usage{
get_rblood2plasma(
  chem.name = NULL,
  chem.cas = NULL,
  dtxsid = NULL,
  species = "Human",
  default.to.human = FALSE
)
}
\arguments{
\item{chem.name}{Either the chemical name or the CAS number must be
specified.}

\item{chem.cas}{Either the CAS number or the chemical name must be
specified.}

\item{dtxsid}{EPA's 'DSSTox Structure ID (https://comptox.epa.gov/dashboard)
the chemical must be identified by either CAS, name, or DTXSIDs}

\item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
default "Human").}

\item{default.to.human}{Substitutes missing animal values with human values
if true.}
}
\value{
A numeric value for the steady-state ratio of chemical concentration in blood
to plasma
}
\description{
This function attempts to retrieve a measured species- and chemical-specific 
blood:plasma concentration ratio.
}
\details{
A value of NA is returned when the requested value is unavailable.  Values
are retrieved from chem.physical_and_invitro.data. %% ~~ If necessary, more
details than the description above ~~
}
\examples{

get_rblood2plasma(chem.name="Bisphenol A")
get_rblood2plasma(chem.name="Bisphenol A",species="Rat")

}
\author{
Robert Pearce
}
\keyword{Parameter}
