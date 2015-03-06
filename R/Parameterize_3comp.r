parameterize_3comp<- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              default.to.human=F,
                              force.human.clint.fub = F,
                              clint.pvalue.threshold=0.05,
                              fu.hep.correct=TRUE
                            )
{
  parms <- parameterize_pbtk(chem.cas=chem.cas,
                              chem.name=chem.name,
                              species=species,
                              default.to.human=F,
                              tissuelist=list(liver=c("liver"),gut=c("gut")),
                              force.human.clint.fub = force.human.clint.fub,
                              clint.pvalue.threshold=clint.pvalue.threshold,
                              fu.hep.correct=fu.hep.correct)
                              
parms$kinhabs <- parms$kdermabs <- parms$Qkidneyf  <- parms$Vvenc <- parms$Vartc <- NULL
 
 return(parms)                             
}
