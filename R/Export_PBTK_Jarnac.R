#Function written by Robert Pearce for NCCT, December, 2013
#Jarnac port of vLiver PBPK model by James Sluka and John Wambaugh
export_pbtk_jarnac <- function(chem.cas=NULL,
                               chem.name=NULL,
                               species="Human",
                               initial.amounts=list(Agutlumen=0),
                               filename="default.jan", 
                               digits = 4)
{
  chem.invivo.PK.data <- chem.invivo.PK.data
  Agutlumen <- Aart <- Aven <- Alung <- Agut <- Aliver <- Akidney <- Arest <- NULL
  for (this.compartment in c("Agutlumen","Aart","Aven","Alung","Agut","Aliver","Akidney","Arest"))
  {
    if (this.compartment %in% names(initial.amounts)) 
    {
      eval(parse(text=paste(this.compartment,"<-",initial.amounts[[this.compartment]])))
    }
    else eval(parse(text=paste(this.compartment,"<- 0")))
  }
  
     if (is.null(chem.cas) & is.null(chem.name))
  {
    stop("Must specifiy compound name or CAS.\n")
  } else if ((!is.null(chem.cas) & !any(chem.invivo.PK.data$CAS==chem.cas)) & (!is.null(chem.name) & !any(chem.invivo.PK.data$Compound==chem.name)))
  {
    stop("Compound not found.\n")
  }

  inlist <- parameterize_pbtk(chem.cas=chem.cas,chem.name=chem.name,species=species)

   out <- get_chem_id(chem.cas=chem.cas,chem.name=chem.name)
  chem.cas <- out$chem.cas
  chem.name <- out$chem.name

  cat("//------------------------------------------------------------------------------
// Name:       ",filename,"
// Description: A",species,"physiologically-based pharmacokinetic model for",chem.name,"(CASRN",chem.cas,")
//
// Author:      John Wambaugh, Robert Pearce, James Sluka
//
// Created:    ",date(),"
//------------------------------------------------------------------------------
p = defn cell

	
// Amount of chemical entering the gut tissue:	
J1: Aart -> Agut;	Qgut*Aart/Vart;
J2: Agutlumen -> Agut;	kgutabs*Agutlumen;
	
// Change of amount of chemical in lung tissue:	

J3: Alung -> Aart;	Qcardiac*Alung/Vlung*Ratioblood2plasma/Klung2plasma/Fraction_unbound_plasma;

J4: Aven -> Alung;	Qcardiac*Aven/Vven;
	
	
	
	
// Change in amount of chemical in the rest of body tissue:	
J5: Aart -> Arest;	Qrest*Aart/Vart;
J6: Arest -> Aven;	Qrest*Arest/Vrest*Ratioblood2plasma/Krest2plasma/Fraction_unbound_plasma;
	
// Change in amount of chemical in the liver tissue:	

J7: Aart -> Aliver;	Qliver*Aart/Vart;
// -> Liver;	CLbiliary*Cliver/Kliver2plasma/Fraction_unbound_plasma/Vliver;
J13: Aliver -> Ametabolized;	CLmetabolism*Aliver/Vliver/Kliver2plasma;
J8: Agut -> Aliver;	Qgut*Agut/Vgut*Ratioblood2plasma/Kgut2plasma/Fraction_unbound_plasma;
J9: Aliver -> Aven;	(Qliver+Qgut)*Aliver/Vliver*Ratioblood2plasma/Kliver2plasma/Fraction_unbound_plasma;
	
	
	
// Change in amount of chemical in the kidney tissue:	
J10: Aart -> Akidney;	Qkidney*Aart/Vart;
J11: Akidney -> Atubules;	Qgfr*Akidney/Vkidney/Kkidney2plasma;
J12: Akidney -> Aven;	Qkidney*Akidney/Vkidney*Ratioblood2plasma/Kkidney2plasma/Fraction_unbound_plasma;


   
end;

// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Parameters and initial conditions

//   Equilibrium ratio of tissue to free plasma concentration",

"\np.Kliver2plasma = ",signif(inlist$Kliver2plasma,digits),";
p.Krest2plasma = ",signif(inlist$Krest2plasma,digits),";
p.Kkidney2plasma = ",signif(inlist$Kkidney2plasma,digits),";
p.Kgut2plasma = ",signif(inlist$Kgut2plasma,digits),";
p.Klung2plasma = ",signif(inlist$Klung2plasma,digits),";

//   Various things:
BW = ",inlist$BW,";            // Body Weight, Kg
hematocrit = ",inlist$hematocrit,";  // fraction of blood volume that is RBCs, typically 0.4 - 0.5
p.Fraction_unbound_plasma = ",signif(inlist$Fraction_unbound_plasma[[1]],digits),";
p.Ratioblood2plasma =  ",signif(inlist$Ratioblood2plasma[[1]],digits),";
P.Qgfr =",signif(24*inlist$Qgfrc,digits),"* BW^0.75;  // L/day
p.CLmetabolism = ",signif(24*inlist$CLmetabolism,digits),"*BW;

//   Species Concentrations: umol
p.Agutlumen = ",Agutlumen,";   // initial dose
p.Aart    = ",Aart,";
p.Aven    = ",Aven,";
p.Alung   = ",Alung,";
p.Agut    = ",Agut,";
p.Aliver  = ",Aliver,";
p.Akidney = ",Akidney,";
p.Arest   = ",Arest,";

//   Tissue volumes: in L
p.Vart = ", signif(inlist$Vartc,digits), "*BW;  // blood volume in arteries
p.Vven = ",signif(inlist$Vvenc,digits),"* BW;
p.Vgut = ",signif(inlist$Vgutc,digits),"* BW;
p.Vliver = ",signif(inlist$Vliverc,digits),"* BW;
p.Vkidney = ",signif(inlist$Vkidneyc,digits),"* BW;
p.Vlung = ",signif(inlist$Vlungc,digits),"* BW;
p.Vrest = ",signif(inlist$Vrestc,digits),"* BW;

//   Volumetric flows: in L/day
p.Qcardiac = ",signif(24*inlist$Qcardiacc,digits)," * BW^0.75;
p.Qgut = ",signif(inlist$Qgutf,digits),"*p.Qcardiac;
p.Qliver = ",signif(inlist$Qliverf,digits),"*p.Qcardiac;
p.Qkidney = ",signif(inlist$Qkidneyf,digits),"*p.Qcardiac;
p.Qrest = p.Qcardiac - p.Qgut - p.Qliver - p.Qkidney;



//   Rate constants L/day
p.kgutabs = ",24*inlist$kgutabs,";  // rate constant for adsorption of compound from the gut


// simulation command (start time, end time, number of points [optional list of <code>]):
m = p.sim.eval (0, 10, 100,[<p.Time>,<p.Aart/p.Vart>,<p.Aven/p.Vven>,<p.Agut/p.Vgut>,
<p.Aliver/p.Vliver>,<p.Alung/p.Vlung>,<p.Arest/p.Vrest>,<p.Akidney/p.Vkidney>]);
//J1 and J2 show flow through that equation

graph(m);",file=filename)
}
