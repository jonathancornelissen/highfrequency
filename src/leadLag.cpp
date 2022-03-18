#include <RcppArmadillo.h>
#include "internals.h"
#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#endif

using namespace arma;

// [[Rcpp::export]]
arma::vec leadLagCpp(const arma::vec& x, const arma::vec& timestampsX, const arma::vec& y, const arma::vec& timestampsY, const arma::vec lags, 
                     const bool normalize) {
  
  // Initialize our variables
  int midPointBackup, midPoint, j0, j1;
  double k, factorX, factorY, returns;
  const double maxTSY = timestampsY(timestampsY.n_elem - 1);
  const double minTSY = timestampsY[0];
  arma::uword nTests = lags.n_elem;
  arma::vec res = zeros(nTests);
  arma::vec clampedTimestamps(timestampsY.n_elem);
  
  
  if(normalize){
    factorX = sqrt(sum(pow(diff(x), 2.0)));
    factorY = sqrt(sum(pow(diff(y), 2.0)));
  } else {
    factorX = 1.0;
    factorY = 1.0;
  }
  
  for(uword j = 0; j < nTests; j++){
    
    k = lags(j);
    
    clampedTimestamps = clamp(timestampsY - k, minTSY, maxTSY);
    
    for(uword i = 0; i < (x.n_elem -1); i++){
      
      returns = x[i + 1] - x[i];
      
      midPointBackup = findFirst(clampedTimestamps, timestampsX[i]);
      midPoint = midPointBackup;
      
      
      while( true ){
        
        if(midPoint < 0 || (midPoint + 1) > ((int) timestampsY.n_elem - 1)){
          break;
        }
        
        j0 = midPoint;
        j1 = midPoint + 1;
        
        
        
        if(overlap(timestampsX[i], timestampsX[i+1], timestampsY[j0] - k, timestampsY[j1] - k)){
          res[j] += returns * (y(j1) - y(j0));
          
          
          midPoint += 1;
        } else {
          break;
        }
        
      }
      
      midPoint = midPointBackup - 1;
      while( true ){
        if(midPoint < 0 || (midPoint + 1) > ((int) timestampsY.n_elem - 1)){
          break;
        }
        
        j0 = midPoint;
        j1 = midPoint + 1;
        
        if(overlap(timestampsX[i], timestampsX[i+1], timestampsY[j0] - k, timestampsY[j1] - k)){
          res[j] += returns * (y(j1) - y(j0));
          midPoint -= 1;
        } else {
          break;
        }
        
      }
    }
    
    
  }
  
  return abs(res)/(factorX * factorY);
}

// [[Rcpp::export]]
arma::vec leadLagCppPAR(const arma::vec& x, const arma::vec& timestampsX, const arma::vec& y, const arma::vec& timestampsY, const arma::vec lags, 
                     const bool normalize, const int iCores) {
// If we have OPENMP available, we will use OPENMP:
#ifdef _OPENMP
omp_set_num_threads(iCores);
  // Initialize our variables
  int midPointBackup, midPoint, j0, j1;
  double k, factorX, factorY, returns;
  const double maxTSY = timestampsY(timestampsY.n_elem - 1);
  const double minTSY = timestampsY[0];
  arma::uword nTests = lags.n_elem;
  arma::vec res = zeros(nTests);
  arma::vec clampedTimestamps(timestampsY.n_elem);
  
  if(normalize){
    factorX = sqrt(sum(pow(diff(x), 2.0)));
    factorY = sqrt(sum(pow(diff(y), 2.0)));
  } else {
    factorX = 1.0;
    factorY = 1.0;
  }
  
  //Parallelization setup
#pragma omp parallel for\
  shared(x, timestampsX, y, timestampsY, factorX, factorY, res)\
    private(returns, midPoint, midPointBackup, j0, j1, clampedTimestamps, k)
  for(uword j = 0; j < nTests; j++){
    
    k = lags(j);
    
    clampedTimestamps = clamp(timestampsY - k, minTSY, maxTSY);
    
    for(uword i = 0; i < (x.n_elem -1); i++){
      
      returns = x[i + 1] - x[i];
      
      midPointBackup = findFirst(clampedTimestamps, timestampsX[i]);
      midPoint = midPointBackup;
      
      
      while( true ){
        
        if(midPoint < 0 || (midPoint + 1) > ((int) timestampsY.n_elem - 1)){
          break;
        }
        
        j0 = midPoint;
        j1 = midPoint + 1;
        
        
        
        if(overlap(timestampsX[i], timestampsX[i+1], timestampsY[j0] - k, timestampsY[j1] - k)){
          res[j] += returns * (y(j1) - y(j0));
          midPoint += 1;
        } else {
          break;
        }
        
      }
      
      midPoint = midPointBackup - 1;
      while( true ){
        if(midPoint < 0 || (midPoint + 1) > ((int) timestampsY.n_elem - 1)){
          break;
        }
        
        j0 = midPoint;
        j1 = midPoint + 1;
        
        if(overlap(timestampsX[i], timestampsX[i+1], timestampsY[j0] - k, timestampsY[j1] - k)){
          res[j] += returns * (y(j1) - y(j0));
          midPoint -= 1;
        } else {
          break;
        }
        
      }
    }
    
    
  }
  arma::vec contrasts = abs(res)/(factorX * factorY);
#else
  // If openMP is not available, we use single core execution.
  Rf_warning("OpenMP is not available. Sequential processing is used.");
  arma::vec contrasts = leadLagCpp(x, timestampsX, y, timestampsY, lags, normalize);
#endif
  return(contrasts);
  
}

