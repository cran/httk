\name{calc_css}
\alias{calc_css}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find the steady state concentration and the day it is reached.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function finds the day a chemical comes within the specified range of the analytical steady state venous blood or plasma concentration(from calc_analytic_css) for the multiple compartment, three compartment, and one compartment models, the fraction of the true steady state value reached on that day, the maximum concentration, and the average concentration at the end of the simulation.
}
\usage{
calc_css(parameters=NULL,chem.name=NULL,chem.cas=NULL,species="Human", p = .01,
         daily.dose=1, doses.per.day=3,days = 10,output.units = "uM",
         concentration='plasma',suppress.messages=F,model='pbtk',fu.hep.correct=T,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
 \item{chem.name}{
   Either the chemical name, CAS number, or parameters must be specified.
%%     ~~Describe \code{obs} here~~
}
  \item{chem.cas}{
   Either the chemical name, CAS number, or parameters must be specified.
%%     ~~Describe \code{pred} here~~
}

  \item{p}{
  Fractional distance from the final steady state concentration that the average concentration must come within to be considered at steady state.
%%     ~~Describe \code{pred} here~~
}
  \item{parameters}{Chemical parameters from parameterize_pbtk function, overrides chem.name and chem.cas.}
  \item{species}{Species desired (either "Rat", "Rabbit", "Dog", "Mouse", or default "Human").}
  \item{daily.dose}{Total daily dose, mg/kg BW.}
  \item{doses.per.day}{Number of doses per day.}
  \item{days}{Initial number of days to run simulation that is multiplied on each iteration.}
  \item{output.units}{Units for returned concentrations, defaults to uM (specify units = "uM") but can also be mg/L.}
   \item{concentration}{Desired concentration type, 'blood' or default 'plasma'.}
   \item{suppress.messages}{Whether or not to suppress messages.}
   \item{model}{Model used in calculation, 'pbtk' for the multiple compartment model,'3compartment' for the three compartment model, '3compartmentss' for the three compartment steady state model, and '1compartment' for one compartment model.}
   \item{fu.hep.correct}{Whether or not to correct the in vitro measured hepatic clearance for the fraction of chemical predicted to bind to hepatocytes using the method of Kilford et al. (2008) (default to TRUE).}
  \item{...}{Additional arguments passed to solve_pbtk.}
  


}

\value{
\item{frac}{Fraction of the true steady state concentration reached on the day steady state is reached.}
\item{max}{The maximum concentration of the simulation.}
\item{avg}{The average concentration on the final day of the simulation.}
\item{the.day}{The day the average concentration comes within 100 * p percent of the true steady state concentration.}

}
\details{
 When species is specified as rabbit, dog, or mouse, the function uses the appropriate physiological data(volumes and flows) but substitues human fraction unbound, partition coefficients, and intrinsic hepatic clearance. 

}

\author{
Robert Pearce
}

\examples{
library(httk)
parms <- parameterize_pbtk(chem.name='Bisphenol-A')
calc_css(parms,concentration='blood')
calc_css(chem.name='Bisphenol-A',doses.per.day=5,p=.001,output.units='mg/L')
# Fig 3ab Approach to Steady-State
\dontrun{
library(ggplot2)
library(parallel)

cluster <- makeCluster(10)
#clusterEvalQ(cl =cluster,.libPaths(c("e:/R-library",.libPaths()[2])))
clusterEvalQ(cl =cluster,library(httk))

all.params.list <- list()            
for (this.CAS in get_cheminfo(model="pbtk")){
    all.params.list[[this.CAS]] <- parameterize_pbtk(this.CAS)
    #all.params.list[[this.CAS]][["Kliver2plasma"]] <- 1 
}

all.Css.list <- parLapply(cluster, 
                          all.params.list, 
                          calc_css, 
                          concentration="plasma",
                          doses.per.day=3,
                          output.units="mg/L")

chem.names <- get_cheminfo(info=c("Compound","CAS"))

Css.table <- NULL
options(scipen=4)
thresh1 <- 1000
thresh1.less <- paste("<",thresh1)
thresh1.more <- paste(">",thresh1)
thresh.parm <- "as.numeric(all.Css.list[[this.CAS]][[\"the.day\"]])"
for (this.CAS in names(all.Css.list))
{
  this.row <- as.data.frame(chem.names[chem.names[,"CAS"]==this.CAS,])
  this.row <- cbind(this.row,
                    as.data.frame(calc_analytic_css(chem.cas=this.CAS,
                                                    output.units="mg/L",
                                                    model="3compartment")))
  this.row <- cbind(this.row,
                   as.data.frame(as.numeric(all.Css.list[[this.CAS]][["avg"]])))
  this.row <- cbind(this.row,
                   as.data.frame(as.numeric(all.Css.list[[this.CAS]][["max"]])))
  this.row <- cbind(this.row,
                  as.data.frame(as.numeric(all.Css.list[[this.CAS]][["frac"]])))
  this.row <- cbind(this.row,
               as.data.frame(as.numeric(all.Css.list[[this.CAS]][["the.day"]])))
  this.row <- cbind(this.row,
                  as.data.frame(calc_volume_of_distribution(chem.cas=this.CAS)))
  this.row <- cbind(this.row,
                        as.data.frame(calc_elimination_rate(chem.cas=this.CAS)))
  if (this.row[thresh.parm] < thresh1)
  {
    this.row <- cbind(this.row,
                      as.data.frame(thresh1.less,stringsAsFactors=F))
  } else {
    this.row <- cbind(this.row,
                      as.data.frame(thresh1.more,stringsAsFactors=F))
  }
  colnames(this.row) <- c("CAS", "Compound", "Css", "Average", "Max", "Ratio",
                          "Days", "V_d", "K_el", "Class")
  Css.table <- rbind(Css.table,this.row)
}

write.table(subset(Css.table,Css.table$Days>1000), 
            row.names=F,
            file=paste("NoSteadyState-chems-",Sys.Date(),".txt",sep=""),
            sep="\t")



below <- sum(Css.table[,"Class"] == thresh1.less)
above <- sum(Css.table[,"Class"] == thresh1.more)
Css.table[Css.table[,"Class"] == thresh1.less,"Class"] <- paste(thresh1.less,
                                                                " (",
                                                                below,
                                                                ")",
                                                                sep="")
Css.table[Css.table[,"Class"] == thresh1.more,"Class"] <- paste(thresh1.more,
                                                                " (",
                                                                above,
                                                                ")",
                                                                sep="")
Css.table[,"Class"] <- factor(Css.table[,"Class"])

scientific_10 <- function(x) {
  out <- gsub("1e", "10^", scientific_format()(x))
  out <- gsub("\\+","",out)
  out <- gsub("10\\^01","10",out)
  out <- parse(text=gsub("10\\^00","1",out))
}

Fig3a <- ggplot(Css.table,aes(x=Days)) + 
  geom_histogram(binwidth = 0.2,aes(fill=Class)) + 
  scale_x_log10("Days to Steady-State",label=scientific_10) + 
  guides(fill = guide_legend(title = "")) +
  theme_bw() +
  theme(legend.position="bottom",
        legend.direction="horizontal",
        axis.text.x  = element_text(hjust=1),
        text  = element_text(size=18))+
  ylab("Number of chemicals")
 
 

print(Fig3a)


print(paste(signif(dim(subset(Css.table,Days<28))[1]/dim(Css.table)[1]*100,1),
            "% (",
            dim(subset(Css.table,Days<28))[1],
            "of",
            dim(Css.table)[1],
            ") chemicals reach steady-state in 28 days."))
print(paste(signif(dim(subset(Css.table,Days<1000))[1]/dim(Css.table)[1]*100,1),
            "% (",
            dim(subset(Css.table,Days<1000))[1],
            "of",dim(Css.table)[1],
            ") chemicals reach steady-state in 1000 days."))


Fig3b <-  ggplot(Css.table,aes(x=Css,y=Max))+
  geom_point(aes(color = Class)) +
  guides(color = guide_legend(title = "Days to Steady-State")) +
  geom_abline(intercept = 0, slope = 1,linetype="dashed") +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.direction="horizontal",
        text  = element_text(size=18)) +
  scale_x_log10(bquote('C'[ss]~"(mg/L)"),label=scientific_10) + 
  scale_y_log10("Peak PBTK Concentration at Steady-State (mg/L)",
                label=scientific_10)  

print(Fig3b)

Fig3b.fit <- lm(log(Css)~log(Max),data=Css.table)
summary(Fig3b.fit)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
