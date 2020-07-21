#include <RcppArmadillo.h>
#include "utils.h"
using namespace arma;

// [[Rcpp::export]]
arma::vec leadLagCpp(const arma::vec& x, const arma::vec& timestampsX, const arma::vec& y, const arma::vec& timestampsY, const arma::vec lags, 
                     const bool normalize) {
  
  // Initialize our variables
  int k, midPointBackup, midPoint, j0, j1;
  double factorX, factorY, returns;
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
  double cov = 0.0;
  
  
  int left = 0;
  int right = 0;
  int counter = 0;
  
  
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
          counter += 1;
          left +=1;
          res[j] += returns * (y(j1) - y(j0));
          cov += returns * (y(j1) - y(j0));
          
          // std::cout<<"xReturns = "<< returns << endl;
          // std::cout<<"yReturns = "<< (y(j1) - y(j0)) << endl;
          // std::cout<<"change = "<< returns * (y(j1) - y(j0)) << endl;
          
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
          cov += returns * (y(j1) - y(j0));
          counter += 1;
          right += 1;
          midPoint -= 1;
        } else {
          break;
        }
        
      }
    }
    
    
  }
  
  
  // std::cout<<"right = "<< right << endl;
  // std::cout<<"left = "<< left << endl;
  // std::cout<<"counter = "<< counter << endl;
  // std::cout<<"cov = "<< cov << endl;
  // std::cout<<"contrast = "<< abs(cov)/(factorX * factorY) << endl;
  // res.print();
  return abs(res)/(factorX * factorY);
}

