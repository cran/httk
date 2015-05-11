# This function parameterizes a PBPK model. The argument tissuelist allows the specific tissues parameerized to be customized.
# All tissues not specified by tissuelist are lumped into a rest of body compartment ("Rest")



parameterize_pbtk <- function(chem.cas=NULL,
                              chem.name=NULL,
                              species="Human",
                              default.to.human=F,
                              tissuelist=list(liver=c("liver"),kidney=c("kidney"),lung=c("lung"),gut=c("gut")),
                              force.human.clint.fub = F,
                              clint.pvalue.threshold=0.05)
{
  PK.physiology.data <- PK.physiology.data
# Look up the chemical name/CAS, depending on what was provide:
  out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name
   
  if(class(tissuelist)!='list') stop("tissuelist must be a list of vectors.") 
  # Clint has units of uL/min/10^6 cells
  Clint <- try(get_invitroPK_param("Clint",species,chem.CAS=chem.cas),silent=T)
  if ((class(Clint) == "try-error" & default.to.human) || force.human.clint.fub) 
  {
    Clint <- try(get_invitroPK_param("Clint","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for metabolic clerance data."))
  }
  if (class(Clint) == "try-error") stop("Missing metabolic clearance data for given species. Set default.to.human to true to substitute human value.")
    # Check that the trend in the CLint assay was significant:
  Clint.pValue <- get_invitroPK_param("Clint.pValue",species,chem.CAS=chem.cas)
  if (!is.na(Clint.pValue) & Clint.pValue > clint.pvalue.threshold) Clint <- 0
  
  # unitless fraction of chemical unbound with plasma
  fub <- try(get_invitroPK_param("Funbound.plasma",species,chem.CAS=chem.cas),silent=T)
  if ((class(fub) == "try-error" & default.to.human) || force.human.clint.fub) 
  {
    fub <- try(get_invitroPK_param("Funbound.plasma","Human",chem.CAS=chem.cas),silent=T)
    warning(paste(species,"coerced to Human for protein binding data."))
  }
  if (class(fub) == "try-error") stop("Missing protein binding data for given species. Set default.to.human to true to substitute human value.")
  if (fub == 0)
  {
    fub <- 0.005
    warning("Fraction unbound = 0, changed to 0.005.")
  }
  
  Fgutabs <- try(get_invitroPK_param("Fgutabs",species,chem.CAS=chem.cas),silent=T)
  if (class(Fgutabs) == "try-error") Fgutabs <- 1
    
  
 # Check the species argument for capitilization problems and whether or not it is in the table:  
  if (!(species %in% colnames(PK.physiology.data)))
  {
    if (toupper(species) %in% toupper(colnames(PK.physiology.data)))
    {
      PK.phys.species <- colnames(PK.physiology.data)[toupper(colnames(PK.physiology.data))==toupper(species)]
      warning(paste(species,"coerced to",PK.phys.species,"for physiological data."))
    } else stop(paste("Physiological PK data for",species,"not found."))
  } else PK.phys.species <- species

# Load the physiological parameters for this species
  this.phys.data <- PK.physiology.data[,PK.phys.species]
  names(this.phys.data) <- PK.physiology.data[,1]
  
  temp <- this.phys.data[['Average Body Temperature']] 
# Load the physico-chemical properties:  
  MW <- get_physchem_param("MW",chem.CAS=chem.cas) #g/mol
  pKa_Donor <- suppressWarnings(get_physchem_param("pKa_Donor",chem.CAS=chem.cas))
  pKa_Accept <- suppressWarnings(get_physchem_param("pKa_Accept",chem.CAS=chem.cas))
  Pow <- 10^get_physchem_param("logP",chem.CAS=chem.cas)
  MA <- suppressWarnings(10^(get_physchem_param("logMA",chem.CAS=chem.cas)))
  
# Predict the PCs for all tissues in the tissue.data table:
  parm <- outlist <- list(Funbound.plasma=fub,Pow=Pow,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept,MA=MA,Fprotein.plasma = 75/1000/1.025,plasma.pH=7.4,temperature=temp)
  PCs <- predict_partitioning_schmitt(parameters=parm)
# Get_lumped_tissues returns a list with the lumped PCs, vols, and flows:
  lumped_params <- get_lumped_tissues(PCs,tissuelist=tissuelist,species=species)


  outlist <- list()
   # Begin flows:
  #mL/min/kgBW converted to L/h/kgBW:
  QGFRc <- this.phys.data["GFR"]/1000*60 
  Qcardiacc = this.phys.data["Cardiac Output"]/1000*60 
  for (this.flow in names(lumped_params$flow))
    if (!(this.flow %in% c("liver","kidney","gut","red blood cells","rest")))
    {
      eval(parse(text=paste("Q",tolower(this.flow),"f = lumped_params$flow$",this.flow,"/this.phys.data[\"Cardiac Output\"]",sep="")))
      eval(parse(text=paste("outlist[[\"Q",tolower(this.flow),"f\"]] = as.numeric(Q",tolower(this.flow),"f)",sep="")))
    }
  Qgutf = lumped_params$flow$gut/this.phys.data["Cardiac Output"]
  Qliverf = (lumped_params$flow$liver-lumped_params$flow$gut)/this.phys.data["Cardiac Output"]
  Qkidneyf = lumped_params$flow$kidney/this.phys.data["Cardiac Output"]
  outlist <- c(outlist,list(
    Qcardiacc = as.numeric(Qcardiacc),
    Qgutf = as.numeric(Qgutf),
    Qliverf = as.numeric(Qliverf),
    Qkidneyf = as.numeric(Qkidneyf),
    Qgfrc = as.numeric(QGFRc))) 
  # end flows  
  
  # Begin volumes
  # units should be L/kgBW  
  Vartc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW
  Vvenc = this.phys.data["Plasma Volume"]/(1-this.phys.data["Hematocrit"])/2/1000 #L/kgBW
  for (this.vol in names(lumped_params$vol))
    if (!(this.vol %in% c("liver","rest","red blood cells")))
      eval(parse(text=paste("outlist[[\"V",tolower(this.vol),"c\"]] = as.numeric(lumped_params$vol$",this.vol,")",sep="")))
  outlist <- c(outlist,list(
    Vartc = as.numeric(Vartc),
    Vvenc = as.numeric(Vvenc),
    Vliverc = as.numeric(lumped_params$vol$liver),
    Vrestc = as.numeric(lumped_params$vol$rest)))
  # end volumes
  
  for (this.K in names(lumped_params$Ktissue2plasma))
    if (!(this.K %in% c("rest","red blood cells")))
      eval(parse(text=paste("outlist[[\"K",tolower(this.K),"2plasma\"]] = as.numeric(lumped_params$Ktissue2plasma$",this.K,")",sep="")))
  outlist <- c(outlist,Krbc2plasma = as.numeric(lumped_params$Ktissue2plasma$'red blood cells'),Krest2plasma = as.numeric(lumped_params$Ktissue2plasma$rest))
# outlist <- c(outlist,Krest2plasma = as.numeric(lumped_params$Ktissue2plasma$Rest))
  
# Create the list of parameters:
  BW <- this.phys.data["Average BW"]
  hematocrit = this.phys.data["Hematocrit"]
  outlist <- c(outlist,list(BW = as.numeric(BW),
    kgutabs = 1, # 1/h
    kinhabs = 1, # 1/h
    kdermabs = 1, # 1/h
    Funbound.plasma = as.numeric(fub), # unitless fraction
    hematocrit = as.numeric(hematocrit), # unitless ratio
    MW = MW)) #g/mol
  
  # Correct for unbound fraction of chemical in the hepatocyte intrinsic clearance assay (Kilford et al., 2008)
 outlist <- c(outlist,list(Fhep.assay.correction=calc_fu_hep(Pow,pKa_Donor=pKa_Donor,pKa_Accept=pKa_Accept)))  # fraction 

  outlist <- c(outlist,
    list(Clmetabolismc= as.numeric(calc_hepatic_clearance(hepatic.model="unscaled",parameters=list(
                                Clint=Clint, #uL/min/10^6 cells
                                Funbound.plasma=fub, # unitless fraction
                                Fhep.assay.correction=outlist$Fhep.assay.correction, 
                                million.cells.per.gliver= 110, # 10^6 cells/g-liver
                                liver.density= 1.05, # g/mL
                                Dn=0.17,BW=BW,
                                Vliverc=lumped_params$vol$liver, #L/kg
                                Qtotal.liverc=(lumped_params$vol$liver+lumped_params$vol$Gut)/1000*60),suppress.messages=T)),million.cells.per.gliver=110,Fgutabs=Fgutabs)) #L/h/kg BW
  

    outlist <- c(outlist,Rblood2plasma=as.numeric(1 - hematocrit + hematocrit * PCs[["red blood cells"]] * fub))
  return(outlist[sort(names(outlist))])
}