/* 3compPBPKmodel.c for R deSolve package
   ___________________________________________________

   Model File:  3compPBPKmodel.model

   Date:  Mon Jan 26 15:45:26 2015

   Created by:  "L:/Lab/NCCT_E~1/MCSim/mod/mod.exe v5.5.0"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2013 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   7 States:
     Aintestine = 0.0,
     Aportven = 0.0,
     Aliver = 0.0,
     Asyscomp = 0.0,
     Ametabolized = 0.0,
     Atubules = 0.0,
     AUC = 0.0,

   4 Outputs:
    "Cportven",
    "Cliver",
    "Csyscomp",
    "Cserum",

   0 Inputs:

   20 Parameters:
     BW = 70,
     CLmetabolismc = 0.203,
     kgutabs = 1,
     Qcardiacc = 0,
     Qgfrc = 0.108,
     Qgutf = 0.205,
     Qliverf = 0.0536,
     Vportven = 0,
     Vliver = 0,
     Vsyscomp = 0,
     Fraction_unbound_plasma = 0.0682,
     CLmetabolism = 0.0,
     Qcardiac = 0,
     Qgfr = 0.0,
     Qgut = 0.0,
     Qliver = 0.0,
     Kliver2plasma = 0,
     Krest2plasma = 0,
     Kgut2plasma = 0,
     Ratioblood2plasma = 0,
*/

#include <R.h>

/* Model variables: States */
#define ID_Aintestine 0x0000
#define ID_Aportven 0x0001
#define ID_Aliver 0x0002
#define ID_Asyscomp 0x0003
#define ID_Ametabolized 0x0004
#define ID_Atubules 0x0005
#define ID_AUC 0x0006

/* Model variables: Outputs */
#define ID_Cportven 0x0000
#define ID_Cliver 0x0001
#define ID_Csyscomp 0x0002
#define ID_Cserum 0x0003

/* Parameters */
static double parms[20];

#define BW parms[0]
#define CLmetabolismc parms[1]
#define kgutabs parms[2]
#define Qcardiacc parms[3]
#define Qgfrc parms[4]
#define Qgutf parms[5]
#define Qliverf parms[6]
#define Vportven parms[7]
#define Vliver parms[8]
#define Vsyscomp parms[9]
#define Fraction_unbound_plasma parms[10]
#define CLmetabolism parms[11]
#define Qcardiac parms[12]
#define Qgfr parms[13]
#define Qgut parms[14]
#define Qliver parms[15]
#define Kliver2plasma parms[16]
#define Krest2plasma parms[17]
#define Kgut2plasma parms[18]
#define Ratioblood2plasma parms[19]



/*----- Initializers */
void initmod3comp (void (* odeparms)(int *, double *))
{
  int N=20;
  odeparms(&N, parms);
}




void getParms_3comp (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  kgutabs = kgutabs * 24 ;
  CLmetabolism = CLmetabolismc * 24 * BW ;
  Qcardiac = Qcardiacc * 24 * pow(BW, 0.75) ;
  Qgfr = Qgfrc * pow(BW, 0.75) * 24 ;
  Qgut = Qcardiac * Qgutf ;
  Qliver = Qcardiac * Qliverf ;

  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivs3comp (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{

  yout[ID_Cportven] = y[ID_Aportven] / Vportven ;

  yout[ID_Cliver] = y[ID_Aliver] / Vliver ;

  yout[ID_Csyscomp] = y[ID_Asyscomp] / Vsyscomp ;

  yout[ID_Cserum] = y[ID_Aliver] / Vliver / Kliver2plasma / Fraction_unbound_plasma ;

  ydot[ID_Aintestine] = - kgutabs * y[ID_Aintestine] ;

  ydot[ID_Aportven] = kgutabs * y[ID_Aintestine] + Qgut * ( y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - y[ID_Aportven] / Vportven * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma ) ;

  ydot[ID_Aliver] = Qgut * y[ID_Aportven] / Vportven * Ratioblood2plasma / Kgut2plasma / Fraction_unbound_plasma + Qliver * y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - ( Qliver + Qgut ) * y[ID_Aliver] / Vliver * Ratioblood2plasma / Fraction_unbound_plasma / Kliver2plasma - CLmetabolism / Kliver2plasma * y[ID_Aliver] / Vliver ;

  ydot[ID_Asyscomp] = ( Qgut + Qliver ) * y[ID_Aliver] / Vliver * Ratioblood2plasma / Fraction_unbound_plasma / Kliver2plasma - Qgut * y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - Qliver * y[ID_Asyscomp] / Vsyscomp * Ratioblood2plasma / Fraction_unbound_plasma / Krest2plasma - Qgfr / Krest2plasma * y[ID_Asyscomp] / Vsyscomp ;

  ydot[ID_Ametabolized] = CLmetabolism / Kliver2plasma * y[ID_Aliver] / Vliver ;

  ydot[ID_Atubules] = Qgfr / Krest2plasma * y[ID_Asyscomp] / Vsyscomp ;

  ydot[ID_AUC] = y[ID_Aliver] / Vliver / Kliver2plasma / Fraction_unbound_plasma ;

} /* derivs */


/*----- Jacobian calculations: */
void jac3comp (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event3comp (int *n, double *t, double *y)
{

} /* event */

/*----- Roots calculations: */
void root3comp (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

