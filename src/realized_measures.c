#include <stdio.h> 
#include <stdlib.h> 
#include <R.h> 
#include "heavy_model.h"
#include <math.h> 
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


void covcc(double *a, double *b, double *at, double *bt, int *na, int *nb, double *ans) {
  int i, prevj = 0, j, nbi=*nb, nai = *na;
  double tmpRet = 0;
  
  for (i = 0; i < nai; i++) {
    tmpRet = 0;
    for (j = prevj; j < nbi; j++) {
      tmpRet += b[j];
      if (bt[j] > at[i]) {
        prevj = j;
        j = nbi;
      } else {
        if (bt[j] == at[i]) {
          prevj = j+1;
          j = nbi;
        }
      }
    }
    ans[i] = a[i]* tmpRet;
  }
}

void covkernel(double *a, double *b, int *na, int *q, int *adj, double *ans) {
  int i, j, lags = *q, nab = *na - 1;
  double theadj;
  
  for(i = lags; i <= nab-lags; i++) {
    for(j =0; j<=lags; j++)
      ans[0] += a[i] * b[i-j];
  }
  if(*adj == 0) {
    theadj = 1;
  } else {
    theadj =  ((double)(nab+1)/((double)(nab+1)-(double)(2*lags)));
  }
  ans[0] = ans[0] * theadj;
}

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

void kernel(double *a, int *na, int *q, int *adj, int *nw, double *ab, double *ans) {
  int i, j,lags = *q, nab = *na - 1;
  double theadj, nwadj;
  
  if (*nw == 1) {
    lags = 2*lags;
  }
  for (j = 0; j <= lags; j++) {
    for(i = 0; i <= nab-j; i++) {
      ab[j] += a[i] * a[i+j];
    }
  }
  for (i = 0; i <= lags; i ++) {
    
    nwadj = 1;
    if(i > *q && *nw == 1) {
      nwadj = ((double)*q - ((double)i - (double)*q))/((double)*q);
    }
    
    if (*adj == 0) {
      theadj = 1;
    } else {
      theadj =  ((double)(nab+1)/((double)(nab+1)-(double)i));
    }
    if (i == 0) {
      ans[0] += theadj * ab[i];
    } else {
      ans[0] += 2 * nwadj *  theadj * ab[i];
    }
  }
}

void kernelEstimator(double *a, double *b, int *na, int *q, int *adj, int *type, double *ab,  double *ab2, double *ans) {
  int i, j,lags = *q, nab = *na - 1;
  double theadj, w;
  
  for (j = 0; j <= lags; j++) {
    for(i = 0; i <= nab-j; i++) {
      ab[j] += a[i] * b[i+j];
    }
    for(i = j; i <= nab; i++) {
      ab2[j] += a[i] * b[i-j];
    }
  }
  
  for (i = 0; i <= lags; i ++) {
    if(i == 0) {
      w = 1;
    } else {
      w = KK(((double)i-1.0)/(double)lags, type[0]);
    }
    
    if (*adj == 0) {
      theadj = 1;
    } else {
      theadj =((double)(nab+1)/((double)(nab+1)-(double)i));
    }
    
    if(i == 0) {
      ans[0] += w * theadj * ab[i];
    } else {
      ans[0] += w* ( theadj * ab[i] + theadj * ab2[i]);
    }
  }
}

int nsmaller(int *times, int *lengths, int start, int end, int max) {
  int i=0;
  while ((i < (lengths[end] - lengths[start])) && (*(times+ (lengths[start]+i)) <= max)) {
    i++;
  }
  return i;
}

void pcovcc(double *a, double *ap, double *b, double *at,double *atp, double *bt, int *na, int *nap, int *nb, int *period, double *ans) {
  int i, prevj = 0, j, nbi=*nb, nai = *na, napi = *nap;
  double tmpRet=0;
  
  for(i = 0; i < nai; i++) {
    ap[i/ *period] += a[i];
    atp[i/ *period] = at[i];
  }
  for(i = 0; i < napi; i++) {
    tmpRet=0;
    for(j=prevj; j < nbi; j++) {
      tmpRet += b[j];
      if(bt[j] > atp[i]) {
        prevj = j;
        j = nbi;
      } else {
        if(bt[j] == atp[i]) {
          prevj = j+1;
          j = nbi;
        }
      }
    }
    ans[i] = ap[i]* tmpRet;
  }
}

void refreshpoints( int *times, int *lengths, int *ttau, int *dim, int *aa, int *indices, int *lindex){
  // my first C program, so probably improvable..
  // length start with all starting points: from zero and upto last endpoint +1
  //  getting variables declared
  int  t_max=0, i, j,a,b;;
  int condition = 1;
  int Ntau[*dim];
  int tnext[*dim];
  // int xx tau[*lindex];
  // getting starting values before looping
  *ttau = 0;
  for(i = 0; i<*dim ; i++){
    if( *ttau < *(times + lengths[i]) )
    { *ttau = *(times + lengths[i] ); }
  }
  
  for(i = 0; i < *dim; i++){
    Ntau[i] = nsmaller(times, lengths, i, (i+1), *ttau);
    *(indices + i * (*lindex))  = Ntau[i];}
  j=0;
  // start the loop over all observations
  while( condition >= 1 ){
    
    for(i = 0; i<*dim; i++){
      tnext[i] = *(times + Ntau[i] + lengths[i]);
      if( tnext[i] > t_max ){ t_max = tnext[i]; }
    }
    j++;
    *(ttau+j) = t_max;
    for(i = 0; i<*dim; i++){
      Ntau[i] = nsmaller(times, lengths, i, (i+1), *(ttau+j));
      *(indices + i* (*lindex) + j)  = Ntau[i];
      a = lengths[i+1];
      b =  (Ntau[i] + lengths[i]);
      if (a <= b) { 
        condition = 0; 
      }
    }
    t_max = 0;
    
  }
  *aa = j+1;
}

void rv(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans) {
  int j, k;
  
  for (j = 0; j <= *na-1; j++) {
    tmpa[j/ *period] += a[j];
    tmpb[j/ *period] += b[j];
  }
  for(k = 0; k < *tmpna; k++) {
    ans[0] += tmpa[k]*tmpb[k];
  }
}

