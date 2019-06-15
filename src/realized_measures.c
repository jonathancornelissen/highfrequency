#include <stdio.h> 
#include <stdlib.h> 
#include <R.h> 
#include "heavy_model.h"
#include <math.h> 
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

double KK(double x, int type) {
  //    double thex = x[0];
  double thex = x;
  switch(type)
  {
  case 0:  // Rectangular
    return(1);
    break;
    
  case 1: // Bartlett
    return(1-thex);
    break;
    
  case 2: //2nd Order
    return(1-2*thex+thex*thex);
    break;
    
  case 3: //Epanechnikov
    return(1-thex*thex);
    break;
    
  case 4: //Cubic
    return(1-3*thex*thex+2*thex*thex*thex);
    break;
    
  case 5: //5th Order
    return(1-10*thex*thex*thex + 15*thex*thex*thex*thex - 6*thex*thex*thex*thex*thex);
    break;
    
  case 6: //6th Order
    return(1 - 15*thex*thex*thex*thex + 24*thex*thex*thex*thex*thex - 10*thex*thex*thex*thex*thex*thex);
    break;
    
  case 7: //7th Order
    return(1 - 21*thex*thex*thex*thex*thex + 35*thex*thex*thex*thex*thex*thex - 15*thex*thex*thex*thex*thex*thex*thex);
    break;
    
  case 8: //8th Order
    return(1 - 28*thex*thex*thex*thex*thex*thex + 48*thex*thex*thex*thex*thex*thex*thex - 21*thex*thex*thex*thex*thex*thex*thex*thex);
    break;
    
  case 9: //Parzen
    if(thex > .5)
    {
      return( 2*(1-thex)*(1-thex)*(1-thex));
    }
    else
    {
      return(1- 6*thex*thex +6*thex*thex*thex);
    }
    break;
    
  case 10: //Tukey-Hanning
    return((1 + sin(M_PI_2 - M_PI * thex))/2);
    break;
    
  case 11: //Modified Tukey-Hanning
    return((1 - sin(M_PI_2 - M_PI *(1 - thex) *(1 - thex)) )/2);
    break;
  }
  return(-999);
}

