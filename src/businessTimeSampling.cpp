#include <RcppArmadillo.h>
using namespace arma;

// [[Rcpp::export]]
arma::vec quadraticKernel(const arma::vec& x){
  //arma::vec res =  0.975 * pow(1.0 - pow(clamp(x, -1.0 , 1.0) , 2.0) , 2.0);
  //const arma::vec res = square(1.0 - square(clamp(x, -1.0 , 1.0)));
  return square(1.0 - square(clamp(x, -1.0 , 1.0))) ;
}



// [[Rcpp::export]]
arma::vec tradeIntensityProcessCpp(arma::vec& time, const double bandwidth) {
  // map time into 0-1 space:
  time -= time(0);
  time /= time(time.n_elem - 1);
  arma::vec tradeIntensityProcess = zeros(time.n_elem);
  const double scalingFactor = 0.975; //Scaling factor for the quadratic kernel
  
  
  //estimate the trade intensity process.
  for(arma::uword i = 0; i < time.n_elem; i++){

    if(time[i] <= bandwidth){
      tradeIntensityProcess[i] = scalingFactor * sum(quadraticKernel((time[i] - time) / bandwidth) + quadraticKernel((time[i] + time) / bandwidth));
    } else if((time[i] > bandwidth) && (time[i] < (1 - bandwidth))){
      tradeIntensityProcess[i] = scalingFactor * sum(quadraticKernel((time[i] - time) / bandwidth));
    } else {
      tradeIntensityProcess[i] = scalingFactor * sum(quadraticKernel((time[i] - time) / bandwidth) + quadraticKernel((time[i] + time - 2.0) / bandwidth));
    }

  }
  
  tradeIntensityProcess = (1.0/bandwidth) * tradeIntensityProcess;
  return tradeIntensityProcess;
}
