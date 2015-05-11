\name{get_wetmore_cheminfo}
\alias{get_wetmore_cheminfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Wetmore Chemical Information.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function provides the information specified in "info=" for all chemicals with data from the Wetmore et al. (2012) and (2013) publications.
}
\usage{
get_wetmore_cheminfo(info="CAS",species="Human")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{info}{A single character vector (or collection of character vectors) from "Compound","CAS","MW","Raw.Experimental.Percentage.Unbound","Entered.Experimental.Percentage.Unbound","Fub","source_PPB","Renal_Clearance","Met_Stab","Met_Stab_entered" ,"r2","p.val","Concentration..uM.","Css_lower_5th_perc.mg.L.","Css_median_perc.mg.L.","Css_upper_95th_perc.mg.L.","Css_lower_5th_perc.uM.","Css_median_perc.uM.","Css_upper_95th_perc.uM.", and "Species".} 
\item{species}{Species desired (either "Rat" or default "Human").}
}


\value{
\item{info}{Table/vector containing values specified in "info" for valid chemicals.}
}

\author{
John Wambaugh
}

\examples{

get_wetmore_cheminfo()

get_wetmore_cheminfo(info=c('CAS','MW'))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Wetmore}
\keyword{Retrieval}% __ONLY ONE__ keyword per line
