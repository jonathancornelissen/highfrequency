#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

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
    return(1 - thex);
    break;
    
  case 2: //2nd Order
    return(1 - 2 * pow(thex, 3.0));
    break;
    
  case 3: //Epanechnikov
    return(1 - pow(thex, 2.0));
    break;
    
  case 4: //Cubic
    return(1 - 3 * pow(thex, 2.0) + 2 * pow(thex, 3.0));
    break;
    
  case 5: //5th Order
    return(1 - 10 * pow(thex, 3.0) + 15 * pow(thex, 4.0) - 6 * pow(thex, 5.0));
    break;
    
  case 6: //6th Order
    return(1 - 15 * pow(thex, 4.0) + 24 * pow(thex, 5.0) - 10 * pow(thex, 6.0));
    break;
    
  case 7: //7th Order
    return(1 - 21 * pow(thex, 5.0) + 35 * pow(thex, 6.0) - 15 * pow(thex, 7.0));
    break;
    
  case 8: //8th Order
    return(1 - 28*pow(thex, 6.0) + 48 * pow(thex, 7.0) - 21*pow(thex, 8.0));
    break;
    
  case 9: //Parzen
    if (thex > .5) {
      return( 2*(1-thex)*(1-thex)*(1-thex));
    }  else {
      return(1- 6 * pow(thex, 2.0) +6 * pow(thex, 3.0));
    }
    break;
    
  case 10: //Tukey-Hanning
    return((1 + sin(M_PI_2 - M_PI * thex))/2);
    break;
    
  case 11: //Modified Tukey-Hanning
    return((1 - sin(M_PI_2 - M_PI * (1 - thex) * (1 - thex)) )/2.0);
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
      theadj = ((double)(nab+1)/((double)(nab + 1)-(double)i));
    }

    if(i == 0) {
      ans += w * theadj * ab[i];
    } else {
      ans += w* ( theadj * ab[i] + theadj * ab2[i]);
    }
  }
  return(ans);
}

// // [[Rcpp::export]]
// double rv(NumericVector a, NumericVector b, int na, int period, NumericVector tmpa,  NumericVector tmpb, int tmpna) {
//   int j, k;
//   
//   double ans = 0;
//   
//   for (j = 0; j <= na-1; j++) {
//     tmpa[j / period] += a[j];
//     tmpb[j / period] += b[j];
//   }
//   for(k = 0; k < tmpna; k++) {
//     ans += tmpa[k] * tmpb[k];
//   }
//   return(ans);
// }

// [[Rcpp::export]]
NumericVector pcovcc(NumericVector a, NumericVector ap, NumericVector b, NumericVector at, NumericVector atp, NumericVector bt, int na, int nap, int nb, int period) {
  int i, prevj = 0, j;
  double tmpRet = 0;
  NumericVector ans(nap);
  
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



arma::mat backfill(const arma::mat& arr, const arma::mat& missings){
  arma::mat res = arr;
  double fill = 0.0;
  for(arma::uword j = 0; j < arr.n_cols; j++){
    fill = 0.0;
    for(arma::uword i = arr.n_rows - 1; i > 0; i--){ // why should this not be i>=0?
      if(missings(i,j)){
        fill = arr(i,j);
      }
      res(i,j) = fill;
    }
  }
  return(res);
}

//[[Rcpp::export]]
Rcpp::List bacImpliedBetaHYCpp(arma::mat& components, const arma::mat& missings, arma::mat& componentWeights){
  arma::mat impliedBeta = arma::zeros<arma::mat>(components.n_cols, components.n_cols);
  arma::mat cov = arma::zeros<arma::mat>(components.n_cols, components.n_cols);
  components = backfill(components, missings);
  componentWeights = backfill(componentWeights, missings);
  // components.rows(0,3).print();
  // components.rows(components.n_rows-4,components.n_rows-1).print();
  // componentWeights.rows(0,3).print();
  // componentWeights.rows(componentWeights.n_rows-4,componentWeights.n_rows-1).print();
  double tmp = 0.0;
  
  // components.brief_print();componentWeights
  // componentWeights.brief_print();
  for(arma::uword k = 0; k < components.n_cols; k++){
    for(arma::uword l = 0; l < components.n_cols; l++){
      double beta = 0.0;
      double __sum = 0.0;
      for(arma::uword i = 0; i < components.n_rows; i++){
        if(missings(i,k) || missings(i,l)){
          tmp = components(i,k) * components(i,l);
          __sum += tmp;
          beta += (tmp * componentWeights(i,k));
        }
      }
      // Rcout<<__sum<<endl;
      impliedBeta(k, l) = beta;
      cov(k, l) = __sum;
    }
  }
  
  Rcpp::List out = Rcpp::List::create(Rcpp::Named("impliedBeta") = sum(impliedBeta,0),
                     Rcpp::Named("cov") = cov);
  return(out);
  // return(impliedBeta);
}



//[[Rcpp::export]]
arma::rowvec bacImpliedBetaCpp(const arma::mat& components, const arma::mat& missings, const arma::mat& componentWeights){
  arma::mat beta = arma::zeros<arma::mat>(components.n_cols, components.n_cols);
  // arma::mat cov = arma::zeros<arma::mat>(components.n_cols, components.n_cols);
  
  int count;
  double currentK, currentL, currentWeight, bayta;//, tmp;
  bool boolK, boolL; 
  
  for(arma::uword k = 0; k < components.n_cols; k++){
    for(arma::uword l = 0; l < components.n_cols; l++){
      count = 0;
      boolK = false;
      boolL = false;
      currentK = 0.0;
      currentL = 0.0;
      bayta = 0.0;
      //tmp = 0.0;
      currentWeight = 0.0;
      
      for(arma::uword i = 0; i < components.n_rows; i++){
        currentWeight += componentWeights(i, k);
        count += 1;
        
        if(missings(i, k)){
          boolK = true;
          currentK += components(i, k);
        }
        
        if(missings(i, l)){
          boolL = true;
          currentL += components(i, l);
        }
        
        if(boolK & boolL){
          bayta += currentWeight / count * currentL * currentK;
          // tmp += currentK * currentL;
          boolK = false;
          boolL = false;
          count = 0;
          currentK = 0.0;
          currentL = 0.0;
          currentWeight = 0.0;
        }
        // cov(k, l) = tmp;
        beta(k, l) = bayta;
      }
      
    }
  }
  
  // return(Rcpp::List::create(Rcpp::Named("beta") = beta,
                            // Rcpp::Named("cov") = cov));
  return(sum(beta, 0));
  
}


//[[Rcpp::export]]

double bacHY(const arma::colvec& component, const arma::colvec& ETF, const arma::uvec& missingComponent, const arma::uvec& missingETF,
             const arma::colvec& componentWeightings){
  double res  = 0.0;
  
  for(uword i = 0; i < component.n_elem; i++){
    
    if(missingComponent[i]){
      for(uword j = i; j < component.n_elem; j++){
        if(missingETF[j]){
          res += component[i] * componentWeightings[i] * ETF[j];
          break;
        }
      }
      continue;
    }
    
    
    if(missingETF[i]){
      for(uword j = i; j < component.n_elem; j++){
        if(missingComponent[j]){
          res += ETF[i] * component[j] * componentWeightings[j];
          break;
        }
        
      }
    }
    
    
    
  }
  

  return res;  
}
