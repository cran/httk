\name{get_cheminfo}
\alias{get_cheminfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrieve chemical information from HTTK package
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function provides the information specified in "info=" (can be single entry or vector) for all chemicals for which a pharmacokinetic model can be paramterized for a given species.
}
\usage{
get_cheminfo(info="CAS",species="Human",exclude.fub.zero=NA,fub.lod.default=0.005,
             model='3compartmentss',default.to.human=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{info}{A single character vector (or collection of character vectors) from "Compound", "CAS", "logP", "pKa_Donor"," pKa_Accept", "MW", "Clint", "Clint.pValue", or "Fub". info="all" gives all information for the model and species.} 
\item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").}
\item{exclude.fub.zero}{Whether or not to exclude chemicals with a fraction of unbound plasma equal to zero or include them with a value of fub.lod.default. Defaults to TRUE for 1compartment and FALSE for pbtk models.}
\item{fub.lod.default}{Default value used for fraction of unbound plasma for chemicals where measured value was below the limit of detection. Default value is 0.0005.}
\item{model}{Model used in calculation, 'pbtk' for the multiple compartment model, '1compartment' for the one compartment model, '3compartment' for three compartment model, '3compartmentss' for the three compartment model without partition coefficients, or 'schmitt' for chemicals with logP and fraction unbound (used in predict_partitioning_schmitt).}
\item{default.to.human}{Substitutes missing values with human values if true.}
}

\details{
When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{info}{Table/vector containing values specified in "info" for valid chemicals.}
}

\author{
John Wambaugh
}

\examples{
# List all CAS numbers for which the 1compartment model can be run in humans: 
get_cheminfo()
# List all compound names for which the 1compartment model can be run in rats,
# excluding those chemicals that were below the limit of detection in the 
# plasma binding assay: 
get_cheminfo(info='Compound',exclude.fub.zero=TRUE,species='Rat')
# List all CAS numbers for which the 1compartment model can be run in humans:
get_cheminfo(info=c('compound','fub','logP')) 
get_cheminfo(model="pbtk")
# See all the data for humans:
get_cheminfo(info="all")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
