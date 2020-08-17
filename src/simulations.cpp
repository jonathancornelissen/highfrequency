#include <RcppArmadillo.h>
using namespace arma;

inline arma::rowvec vecpow(arma::rowvec x, const arma::rowvec power){
  assert (x.n_elem == power.n_elem);
  
  for(arma::uword i = 0; i < x.n_elem; i++){
    if(power[i] == 0.0){
      x[i] = 1.0;
    } else {
      x[i] = pow(x[i], power[i]);
    }
  }
  
  return(x);
  
}


// [[Rcpp::export]]
Rcpp::List vasicekModel(Rcpp::List model, int nObs, int nSeries, int nDays, arma::mat dt){
  
  arma::rowvec a = model["meanReversion"];
  arma::rowvec b = model["drift"];
  arma::rowvec sigma = model["driftVol"];
  
  arma::mat drift = arma::zeros<mat>((nObs * nDays), nSeries);
  
  drift.row(0) = b + 1e-8; // So we don't have problems when drift = 0;
  
  for(int t = 1; t < nObs * nDays; t++){
    drift.row(t) = drift.row(t-1) + a % (b - drift.row(t-1)) % dt.row(t) + sigma % randn(1, nSeries) % sqrt(dt.row(t));
  }

  Rcpp::List out;
  out["drift"] = drift;
  return out;
  
  
}

// // [[Rcpp::export]]
// List hestonSimCpp(int iObs, double dMu, double dKappa, double dSigma, double dXi, double dRho) {
//   
//   arma::vec vVol(iObs);
//   arma::vec vPrice(iObs);
//   arma::vec vWt = arma::randn(iObs) * sqrt(1.0/iObs);
//   arma::vec vBt = arma::randn(iObs) * sqrt(1.0/iObs)  * sqrt((1.0-pow(dRho,2.0))) + dRho * vWt;
//   vVol[0] = dSigma;
//   for(int t = 1; t < iObs; t++){
//     vVol[t] = vVol[(t-1)] + dKappa * (dSigma - vVol[(t-1)]) *  1.0/iObs + dXi * sqrt(vVol[(t-1)]) * vBt[(t-1)];
//   }
//   
//   arma::vec vRet = (dMu + pow(vVol,2.0) * 0.5) * 1.0/iObs + sqrt(vVol) % vWt;
//   List lOut;
//   lOut["sigma"] = vVol;
//   lOut["returns"] = vRet;
//   return lOut;
// }
