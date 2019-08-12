#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int nsmaller(IntegerVector times, IntegerVector lengths, int start, int end, int max) {
  int i = 0;
  while ( (i < (lengths[end] - lengths[start])) && (times[(lengths[start]+i)] <= max)) {
    i++;
  }
  return i;
}


// [[Rcpp::export]]
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
    if (thex > .5) {
      return( 2*(1-thex)*(1-thex)*(1-thex));
    }  else {
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

// [[Rcpp::export]]
double kernelEstimator(NumericVector a, NumericVector b, int na, int q, int adj, NumericVector type, NumericVector ab,  NumericVector ab2) {
  int i, j,lags = q, nab = na - 1;
  double theadj, w;
  double ans = 0;

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

    if (adj == 0) {
      theadj = 1;
    } else {
      theadj =((double)(nab+1)/((double)(nab + 1)-(double)i));
    }

    if(i == 0) {
      ans += w * theadj * ab[i];
    } else {
      ans += w* ( theadj * ab[i] + theadj * ab2[i]);
    }
  }
  return(ans);
}

// [[Rcpp::export]]
double rv(NumericVector a, NumericVector b, int na, int period, NumericVector tmpa,  NumericVector tmpb, int tmpna) {
  int j, k;
  
  double ans = 0;
  
  for (j = 0; j <= na-1; j++) {
    tmpa[j / period] += a[j];
    tmpb[j / period] += b[j];
  }
  for(k = 0; k < tmpna; k++) {
    ans += tmpa[k] * tmpb[k];
  }
  return(ans);
}

// [[Rcpp::export]]
NumericVector pcovcc(NumericVector a, NumericVector ap, NumericVector b, NumericVector at, NumericVector atp, NumericVector bt, int na, int nap, int nb, int period) {
  int i, prevj = 0, j;
  double tmpRet = 0;
  NumericVector ans;
  
  for(i = 0; i < na; i++) {
    ap[i / period] += a[i];
    atp[i / period] = at[i];
  }
  for(i = 0; i < nap; i++) {
    tmpRet = 0;
    for(j = prevj; j < nb; j++) {
      tmpRet += b[j];
      if(bt[j] > atp[i]) {
        prevj = j;
        j = nb;
      } else {
        if (bt[j] == atp[i]) {
          prevj = j+1;
          j = nb;
        }
      }
    }
    ans[i] = ap[i] * tmpRet;
  }
  return(ans);
}

