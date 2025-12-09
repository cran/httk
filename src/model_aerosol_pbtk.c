/* model_aerosol_pbtk.c for R deSolve package
   ___________________________________________________

   Model File:  aerosol.model

   Date:  Mon Dec 10 11:31:47 2018

   Created by:  "C:/Users/mlinakis/Desktop/RLibrary/MCSIMU~1/mod/mod.exe v5.6.5"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2015 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   11 States:
     Agutlumen = 0.0,
     Agut = 0.0,
     Aliver = 0.0,
     Aven = 0.0,
     Alung = 0.0,
     Aart = 0.0,
     Arest = 0.0,
     Akidney = 0.0,
     Atubules = 0.0,
     Ametabolized = 0.0,
     AUC = 0.0,

   10 Outputs:
    "Cgut",
    "Cliver",
    "Cven",
    "Clung",
    "Cart",
    "Crest",
    "Ckidney",
    "Cplasma",
    "Aplasma",
    "MassBal",

   1 Input:
     Cinh (forcing function)

   55 Parameters:
     BW = 70,
     Clmetabolismc = 0.203,
     hematocrit = 0.44,
     kgutabs = 1,
     Kkidney2pu = 0,
     Kliver2pu = 0,
     Krest2pu = 0,
     Kgut2pu = 0,
     Klung2pu = 0,
     Qcardiacc = 4.8,
     Qgfrc = 0.108,
     Qgutf = 0.205,
     Qkidneyf = 0.221,
     Qliverf = 0.0536,
     Qlungf = 0,
     Vartc = 0.0487,
     Vgutc = 0.0158,
     Vkidneyc = 0.00119,
     Vliverc = 0.02448,
     Vlungc = 0.00723,
     Vrestc = 0.77654,
     Vvenc = 0.0487,
     Fraction_unbound_plasma = 0.0682,
     Rblood2plasma = 0.0,
     Clmetabolism = 0.0,
     Qcardiac = 0.0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qkidney = 0.0,
     Qliver = 0.0,
     Qlung = 0.0,
     Qrest = 0.0,
     Vart = 0.0,
     Vgut = 0.0,
     Vkidney = 0.0,
     Vliver = 0.0,
     Vlung = 0.0,
     Vrest = 0.0,
     Vven = 0.0,
     Mnvt = 0,
     Fdeposited = 0,
     FET1 = 0,
     FET2 = 0,
     FBB = 0,
     Fbbb = 0,
     Fds = 0,
     DEalv = 0,
     Kblood2air = 0,
     srapdis = 0,
     sslodis = 0,
     frapdisET1 = 0,
     frapdisET2 = 0,
     frapdisBB = 0,
     frapdisbbb = 0,
     frapdisAlv = 0,
*/

#include <R.h>

/* Model variables: States */
#define ID_Agutlumen 0x00000
#define ID_Agut 0x00001
#define ID_Aliver 0x00002
#define ID_Aven 0x00003
#define ID_Alung 0x00004
#define ID_Aart 0x00005
#define ID_Arest 0x00006
#define ID_Akidney 0x00007
#define ID_Atubules 0x00008
#define ID_Ametabolized 0x00009
#define ID_AUC 0x0000a
#define ID_AET1 0x0000b
#define ID_AET2 0x0000c
#define ID_ABB 0x0000d
#define ID_Abbb 0x000e
#define ID_AAlv 0x000f
#define ID_AInt 0x00010
#define ID_ALym 0x00011
#define ID_Tdose 0x00012

/* Model variables: Outputs */
#define ID_Cgut 0x00000
#define ID_Cliver 0x00001
#define ID_Cven 0x00002
#define ID_Clung 0x00003
#define ID_Cart 0x00004
#define ID_Crest 0x00005
#define ID_Ckidney 0x00006
#define ID_Cplasma 0x00007
#define ID_Aplasma 0x00008
#define ID_AERIN 0x00009
#define ID_AEREX 0x0000a
#define ID_MassBal 0x0000b

/* Parameters */
static double parms[55];

#define BW parms[0]
#define Clmetabolismc parms[1]
#define hematocrit parms[2]
#define kgutabs parms[3]
#define Kkidney2pu parms[4]
#define Kliver2pu parms[5]
#define Krest2pu parms[6]
#define Kgut2pu parms[7]
#define Klung2pu parms[8]
#define Qcardiacc parms[9]
#define Qgfrc parms[10]
#define Qgutf parms[11]
#define Qkidneyf parms[12]
#define Qliverf parms[13]
#define Qlungf parms[14]
#define Vartc parms[15]
#define Vgutc parms[16]
#define Vkidneyc parms[17]
#define Vliverc parms[18]
#define Vlungc parms[19]
#define Vrestc parms[20]
#define Vvenc parms[21]
#define Fraction_unbound_plasma parms[22]
#define Rblood2plasma parms[23]
#define Clmetabolism parms[24]
#define Qcardiac parms[25]
#define Qgfr parms[26]
#define Qgut parms[27]
#define Qkidney parms[28]
#define Qliver parms[29]
#define Qlung parms[30]
#define Qrest parms[31]
#define Vart parms[32]
#define Vgut parms[33]
#define Vkidney parms[34]
#define Vliver parms[35]
#define Vlung parms[36]
#define Vrest parms[37]
#define Vven parms[38]
#define Mnvt parms[39]
#define Fdeposited parms[40]
#define FET1 parms[41]
#define FET2 parms[42]
#define FBB parms[43]
#define Fbbb parms[44]
#define Fds parms[45]
#define DEalv parms[46]
#define Kblood2air parms[47]
#define srapdis parms[48]
#define sslodis parms[49]
#define frapdisET1 parms[50]
#define frapdisET2 parms[51]
#define frapdisBB parms[52]
#define frapdisbbb parms[53]
#define frapdisAlv parms[54]

/* Forcing (Input) functions */
static double forc[1];

#define Cinh forc[0]

/*----- Initializers */
void initmod_aerosol_pbtk (void (* odeparms)(int *, double *))
{
  int N=55;
  odeparms(&N, parms);
}

void initforc_aerosol_pbtk (void (* odeforcs)(int *, double *))
{
  int N=1;
  odeforcs(&N, forc);
}


void getParms_aerosol_pbtk (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  Clmetabolism = Clmetabolismc * 24 * BW ;
  Qcardiac = Qcardiacc * 24 * pow ( BW , 0.75 ) ;
  Qgfr = Qgfrc * pow ( BW , 0.75 ) * 24 ;
  Qgut = Qcardiac * Qgutf ;
  Qkidney = Qcardiac * Qkidneyf ;
  Qliver = Qcardiac * Qliverf ;
  Qlung = Qcardiac * Qlungf ;
  Qrest = Qcardiac - ( Qgut + Qkidney + Qliver) ;
  Vart = Vartc * BW ;
  Vgut = Vgutc * BW ;
  Vkidney = Vkidneyc * BW ;
  Vliver = Vliverc * BW ;
  Vlung = Vlungc * BW ;
  Vrest = Vrestc * BW ;
  Vven = Vvenc * BW ;
  Mnvt = (Mnvt) * 60 * 24; /*Convert L/min to L/day, BW in theory is already accounted for*/

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs_aerosol_pbtk (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  
  yout[ID_AERIN] = (Cinh*Mnvt) ; /*- (FET1 * Cinh * Vdot) - (FET2 * Cinh * Vdot) - (FBB * Cinh * Vdot) - (Fbbb * Cinh * Vdot) - (FAlv * Cinh * Vdot)*/
  	
  yout[ID_AEREX] = yout[ID_AERIN] - (Fdeposited * yout[ID_AERIN]) ; 
	/*Note: There may be a 0.002 fraction sequestered in AET1 as well, but we're ignoring it for now
	Could also make a "nasal discharge" compartment for -0.6 x ET1 
	How does respiratory rate play in? Should be accounted for in Vdot*/
  ydot[ID_AET1] = (FET1 * yout[ID_AERIN]) - 1.5 * y[ID_AET1] /* - 0.6 * y[ID_AET1] */ - (frapdisET1 * (1-0) * srapdis * y[ID_AET1]) - ((1-frapdisET1) * (1-0) * sslodis * y[ID_AET1]) ;
  
  ydot[ID_AET2] = (FET2 * yout[ID_AERIN]) + 1.5 * y[ID_AET1] + (0.998 * 10 * y[ID_ABB]) - (0.998 * y[ID_AET2] * 100) - (0.002 * y[ID_AET2] * 0.001) - (frapdisET2 * (1-0) * srapdis * y[ID_AET2]) - ((1-frapdisET2)*(1-0)*sslodis*y[ID_AET2]) ;
  
  ydot[ID_ABB] = (FBB * yout[ID_AERIN]) + (0.998 * 0.2 * y[ID_Abbb]) - (0.998 * y[ID_ABB] * 10) - (0.002 * y[ID_ABB] * 0.001) - (frapdisBB * (1-0) * srapdis * y[ID_ABB]) - ((1-frapdisBB)*(1-0)*sslodis*y[ID_ABB]); /* - (fr * (1-fb) * Sr) - ((1-fr) * (1-fb) * Ss) - might need to fix this last bound term((fb*Sr)+(fb*Ss)-Sb)) Do we want this to be individually defined variables? Yes, eventually*/

  ydot[ID_Abbb] = (Fbbb * yout[ID_AERIN]) + 0.002 * y[ID_AAlv] - (0.998 * y[ID_Abbb] * 0.2) - (0.002 * y[ID_Abbb] * 0.001)- (frapdisbbb * (1-0) * srapdis * y[ID_Abbb]) - ((1-frapdisbbb)*(1-0)*sslodis*y[ID_Abbb]) ;

  ydot[ID_AAlv] = (DEalv * yout[ID_AERIN]) -0.002 * y[ID_AAlv] - 0.001 * y[ID_AAlv] - (frapdisAlv * (1-0) * srapdis * y[ID_AAlv]) - ((1-frapdisAlv)*(1-0)*sslodis*y[ID_AAlv]);

  ydot[ID_AInt] = 0.001 * y[ID_AAlv] - 0.00003 * y[ID_AInt] ;

  ydot[ID_ALym] = (0.002 * y[ID_AET2]*0.001) + (0.002 * y[ID_ABB]*0.001) + (0.002 * y[ID_Abbb]*0.001) + 0.00003 * y[ID_AInt] - (frapdisbbb * (1-0) * srapdis * y[ID_ALym]) - ((1-frapdisbbb)*(1-0)*sslodis*y[ID_ALym]) ; /* VENOUS DRAINAGE TERM, set to be the same as other absorptions.*/

  yout[ID_Cgut] = y[ID_Agut] / Vgut ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Cven] = y[ID_Aven] / Vven ;

  yout[ID_Clung] = y[ID_Alung] / Vlung ;

  yout[ID_Cart] = y[ID_Aart] / Vart ;

  yout[ID_Crest] = y[ID_Arest] / Vrest ;

  yout[ID_Ckidney] = y[ID_Akidney] / Vkidney ;

  yout[ID_Cplasma] = y[ID_Aven] / Vven / Rblood2plasma ;

  yout[ID_Aplasma] = y[ID_Aven] / Rblood2plasma * ( 1 - hematocrit ) ;

  ydot[ID_Agutlumen] = - kgutabs * y[ID_Agutlumen] + (0.998 * y[ID_AET2] * 100); 

  ydot[ID_Agut] = kgutabs * y[ID_Agutlumen] + Qgut * ( y[ID_Aart] / Vart - y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma )  ;

  ydot[ID_Aliver] = Qliver * y[ID_Aart] / Vart + Qgut * y[ID_Agut] / Vgut * Rblood2plasma / Kgut2pu / Fraction_unbound_plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu / Fraction_unbound_plasma * Rblood2plasma - Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_Aven] = ( ( Qliver + Qgut ) * y[ID_Aliver] / Vliver / Kliver2pu + Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu + Qrest * y[ID_Arest] / Vrest / Krest2pu ) * Rblood2plasma / Fraction_unbound_plasma - Qcardiac * y[ID_Aven] / Vven ;

  ydot[ID_Alung] = Qcardiac * ( y[ID_Aven] / Vven - y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Aart] = Qcardiac * ( y[ID_Alung] / Vlung * Rblood2plasma / Klung2pu / Fraction_unbound_plasma - y[ID_Aart] / Vart ) + (frapdisET1 * (1-0) * srapdis * y[ID_AET1]) + ((1-frapdisET1) * (1-0) * sslodis * y[ID_AET1]) + (frapdisET2 * (1-0) * srapdis * y[ID_AET2]) + ((1-frapdisET2)*(1-0)*sslodis*y[ID_AET2]) + (frapdisBB * (1-0) * srapdis * y[ID_ABB]) + ((1-frapdisBB)*(1-0)*sslodis*y[ID_ABB]) + (frapdisbbb * (1-0) * srapdis * y[ID_Abbb]) + ((1-frapdisbbb)*(1-0)*sslodis*y[ID_Abbb]) + (frapdisAlv * (1-0) * srapdis * y[ID_AAlv]) + ((1-frapdisAlv)*(1-0)*sslodis*y[ID_AAlv]) + (frapdisbbb * (1-0) * srapdis * y[ID_ALym]) + ((1-frapdisbbb)*(1-0)*sslodis*y[ID_ALym]); /* + ( Vdot * DEalv * ( Cinh - yout[ID_Calv] ) ) */ 

  ydot[ID_Arest] = Qrest * ( y[ID_Aart] / Vart - y[ID_Arest] / Vrest * Rblood2plasma / Krest2pu / Fraction_unbound_plasma ) ;

  ydot[ID_Akidney] = Qkidney * y[ID_Aart] / Vart - Qkidney * y[ID_Akidney] / Vkidney / Kkidney2pu * Rblood2plasma / Fraction_unbound_plasma - Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu ;

  ydot[ID_Atubules] = Qgfr * y[ID_Akidney] / Vkidney / Kkidney2pu  ;

  ydot[ID_Ametabolized] = Clmetabolism * y[ID_Aliver] / Vliver / Kliver2pu ;

  ydot[ID_AUC] = y[ID_Aven] / Vven / Rblood2plasma ;

  ydot[ID_Tdose]= (FET1 + FET2 + FBB + Fbbb + DEalv) * yout[ID_AERIN] ;

  yout[ID_MassBal] = y[ID_Tdose] - y[ID_AET1] /* - (0.6 * y[ID_AET1]) */ - y[ID_AET2] - y[ID_ABB] - y[ID_Abbb] - y[ID_AAlv] - y[ID_AInt] - y[ID_ALym] - y[ID_Agutlumen] - y[ID_Agut] - y[ID_Aliver] - y[ID_Aven] - y[ID_Alung] - y[ID_Aart] - y[ID_Arest] - y[ID_Akidney] - y[ID_Atubules] - y[ID_Ametabolized] ;
  /*yout[ID_AERIN] - yout[ID_AEREX] - y[ID_AET1] - y[ID_AET2] - y[ID_ABB] - y[ID_Abbb] - y[ID_AAlv] - y[ID_AInt] - y[ID_ALym] - y[ID_Agutlumen] - y[ID_Agut] - y[ID_Aliver] - y[ID_Aven] - y[ID_Alung] - y[ID_Aart] - y[ID_Arest] - y[ID_Akidney] - y[ID_Atubules] - y[ID_Ametabolized] ;*/
  /* Note: Agutlumen if there's also an oral dose can't be just - for mass balance. But for this model, it's only taking an input through AERIN */

} /* derivs */


/*----- Jacobian calculations: */
void jac_aerosol_pbtk (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event_aerosol_pbtk (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root_aerosol_pbtk (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

