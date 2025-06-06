% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_model.R
\name{check_model}
\alias{check_model}
\title{Check for sufficient model parameters}
\usage{
check_model(
  chem.name = NULL,
  chem.cas = NULL,
  dtxsid = NULL,
  model = NULL,
  species = NULL,
  class.exclude = TRUE,
  physchem.exclude = TRUE,
  default.to.human = FALSE
)
}
\arguments{
\item{chem.name}{Chemical name (spaces and capitalization ignored) --  if
parameters is not specified then the chemical must be identified by either
CAS, name, or DTXISD}

\item{chem.cas}{Chemical Abstract Services Registry Number (CAS-RN) -- if
parameters is not specified then the chemical must be identified by either
CAS, name, or DTXISD}

\item{dtxsid}{EPA's DSSTox Structure ID (\url{https://comptox.epa.gov/dashboard})
 -- if parameters is not specified then the chemical must be identified by 
either CAS, name, or DTXSIDs}

\item{model}{Model to be checked, modelinfo files specify the requrements of each model.}

\item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or
default "Human").}

\item{class.exclude}{Exclude chemical classes identified as outside of 
domain of applicability by relevant modelinfo_[MODEL] file (default TRUE).}

\item{physchem.exclude}{Exclude chemicals on the basis of physico-chemical
properties (currently only Henry's law constant) as specified by 
the relevant modelinfo_[MODEL] file (default TRUE).}

\item{default.to.human}{Substitutes missing fraction of unbound plasma with
human values if true.}
}
\value{
Stops code from running if all parameters needed for model
are not available, otherwise does nothing.
}
\description{
This function halt model evaluation if not all the needed parameters
(as specified in the modelinfo_[MODEL].r file) are available. The function uses
\code{\link{get_cheminfo}}, so if the chemical has been checked against that
function already then evaluation should proceed as expected. If you do not
have the parameters you need and are using a non-human
species try default.to.human = TRUE (there are many more values for human than
any other species). If working in human, try first using
\code{\link{load_dawson2021}}, \code{\link{load_sipes2017}}, or
\code{\link{load_pradeep2020}}.
}
\seealso{
\code{\link{get_cheminfo}}
}
\author{
john Wambaugh
}
\keyword{Parameter}
